# This script conducts exploratory analysis for the Acorn Data Challenge.
# More info on the data here: 
# https://data.lacity.org/A-Safe-City/Traffic-Collision-Data-from-2010-to-Present/d5tf-ez2w

# SETUP -------------------------------------------------------------------

  devtools::install_github("dkahle/ggmap")
  pacman::p_load(RSocrata, feather, skimr, dplyr, corrplot, tidyr, ggplot2, lubridate, 
                 devtools, ggmap, zoo, prophet, readr, forecast, gridExtra)
  register_google(key = Sys.getenv("GOOGLE_KEY"))

# GET DATA ----------------------------------------------------------------
# Getting the data requires an LA Open Data Portal account and app token.
# More info here: https://dev.socrata.com/foundry/data.lacity.org/d5tf-ez2w

  # Download data. Initially downloaded on 2019-08-23. Code saved for posterity.
  # data = read.socrata("https://data.lacity.org/resource/d5tf-ez2w.csv",    Takes a minute or 2
  #                     app_token = Sys.getenv("SOCRATA_APP_TOKEN"), 
  #                     email = Sys.getenv("SOCRATA_EMAIL"), 
  #                     password = Sys.getenv("SOCRATA_PASSWORD"))
  
  # Save as feather file. Code saved for posterity.
  # write_feather(data, "la_traffic_collision_data.feather")
  
  data = read_feather("la_traffic_collision_data.feather")     # Import data
  
# DATA CHECKS -------------------------------------------------------------

  skim(data)                                # Data summary: gives shape, field types, distributions
  
  # Further exploration
  
    table(data$crm_cd_desc)                   # Crime code description has all one value
    table(data$crm_cd)                        # Crime code has all one value
    
    sort(prop.table(table(data$premis_desc))) # Distribution of "premis_desc"
    sort(prop.table(table(data$premis_cd)))   # Distribution of "premis_cd"
    
    length(unique(data$premis_desc))          # "premise_desc" and "premis_cd" unique values
    length(unique(data$premis_cd))
    select(data, c(premis_cd, premis_desc)) %>% unique() %>% dim()
    
    length(unique(data$area))          # "area" and "area_name" unique values
    length(unique(data$area_name))
    select(data, c(area, area_name)) %>% unique() %>% dim()
    
    table(data$area_name)                     # Variable breakdowns
    table(data$cross_street)
    table(data$location)
    table(data$location_1)
    table(data$mocodes)
    table(data$premis_desc)
    table(data$vict_descent)
    table(data$vict_sex)
    
  # Check minimum date for all "area"
  # All "area" have minimum date of "2010-01-01"
  data %>% 
    group_by(area) %>% summarize(min_date = min(date_occ)) %>% ungroup() %>% 
    select(min_date) %>% 
    unique()
  
  # Look at distribution of difference between date occurred and reported
  # Most collisions reported the same day...some not reported for years!
  # Data error?
  table(data$date_occ > data$date_rptd)
  table(data$date_occ == data$date_rptd)
  quantile(ymd(data$date_rptd) - ymd(data$date_occ))
  
  data %>%
    mutate(gap = as.numeric(ymd(date_rptd) - ymd(date_occ))) %>% 
    filter(gap > 0) %>%
    
    ggplot(aes(x = gap)) + 
    geom_histogram(bins = 100)
  
  # What jumps out?
  
    # "premis_cd", "vict_age" have some missing values
    # "premis_desc", "vict_descent", "vict_sex" have some empty values
    # "cross_street", "mocodes" have man empty values
    
    # "crm_cd_desc" (crime code description) has value "TRAFFIC COLLISION" for all values.
    # "crm_cd" (crime code) has value "997 COLLISION" for all values.
    
    # "premis_desc" has value "STREET" ~95% of the time
    # "premis_cd" has value "101" ~95% of the time
    # "premis_desc" and "premis_cd" are 1:1
    # "area" and "area_name" are 1:1
    
    # "area_name", "vict_descent", "vict_sex" have a few different values
    # "cross_street", "location", "location_1", "mocodes", "premis_desc" have many values
  
# INITIAL DATA PREP -------------------------------------------------------
# This section modifies the data based on the results of the "DATA CHECKS" section
  
  data_clean = data %>% 
                select(-c(crm_cd_desc, crm_cd)) %>%               # These fields have only 1 value
      
                mutate(dr_no = as.character(dr_no),               # These fields are not really numeric
                       area = as.character(area), 
                       rpt_dist_no = as.character(rpt_dist_no), 
                       premis_cd = as.character(premis_cd), 
                       
                       location = toupper(gsub(" +", " ", location)),           # Remove extra spaces and make sure
                       cross_street = toupper(gsub(" +", " ", cross_street)),   # fields are upper case
                       
                       location_1 = gsub("\n,  \n\\(", "", location_1),   # Get latitude, longitude
                       location_1 = gsub(")", "", location_1), 
                       lat = substr(location_1, start = 1, stop = as.numeric(gregexpr(",", location_1)) - 1), 
                       lon = substr(location_1, start = as.numeric(gregexpr(",", location_1)) + 1, 
                                    stop = nchar(location_1)), 
                       lat = as.numeric(lat), 
                       lon = as.numeric(lon))      
    
# DATA EXPLORATION --------------------------------------------------------

  # Correlation plot
  # Not very interesting...only 2 numeric columns and "vict_age" has a bunch of nulls.
  # No correlation between "vict_age" and "time_occ"
  corrplot(cor(select_if(data_clean, is.numeric) %>% 
                 filter(is.na(vict_age) == F)))
    
  # Density plot of numeric variables. Not very interesting
  data_clean %>% 
    select_if(is.numeric) %>% 
    filter(is.na(vict_age) == F) %>%
    gather(key = "variable", value = "value") %>% 
    ggplot(aes(value)) + geom_density() + facet_wrap(~variable, scales = "free")
  
  # Histograms of numeric variables
  
    # Collisions by time of day. Interesting and useful!
    # Collisions are lowest around 5am. They have spikes at 8/830am, 1130am/129m, 430/5pm
    # Namely when people are on the road
    ggplot(data = select(data_clean, time_occ), aes(x = time_occ)) + 
      geom_histogram() + 
      scale_x_continuous(breaks = seq(0, 2400, 200)) +
      labs(x = "Time Occurred (24 Hour Format)", y = "Count", title = "Histogram of Collisions by Time")
      
    # Collisions by victim age. Interesting and useful!
    # Rounding...ages are often rounded to the nearest 5.
    # Very few victims below 15 years old, as expected.
    # How do they address multiple victims?
    # Most victims are in their early twenties, data is basically decreasing from there.
    # Except for 99 year old victims...many of those. Some kind of catch-all?
    ggplot(data = select(data_clean, vict_age), aes(x = vict_age)) + 
      geom_histogram(binwidth = 1) + 
      scale_x_continuous(breaks = seq(0, 100, 5)) +
      labs(x = "Victim Age", y = "Count", title = "Histogram of Collisions by Victim Age")
    
    # Plot by "vict_sex"
    # Males much more likely to be in collision
    data_clean %>%
      group_by(vict_sex) %>% 
      summarize(total = n()) %>% 
      ungroup() %>% 
      filter(!(vict_sex %in% c("", "H", "N"))) %>%
      
    ggplot(aes(x = vict_sex, y = total)) + 
      geom_bar(stat = "identity") + 
      labs(x = "Victim Sex", y = "Collisions", title = "Collisions by Victim Sex")

# OUTLIER ANALYSIS --------------------------------------------------------
# This section looks for days with the least/most collisions.
    
  # Create data set with collision count per day
  outliers = data_clean %>% 
              filter(date_occ != "2019-08-17") %>%      # This day looks like it has partial results
              group_by(date_occ) %>% summarize(daily_count = n()) %>% ungroup()
    
  # Look at distribution of daily collisions
  skim(outliers)
  quantile(outliers$daily_count, c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1))
  
  # Look at days with lowest collisions
  lowest = outliers %>% 
            filter(daily_count < quantile(outliers$daily_count, 0.005)) %>% 
    
            mutate(day = weekdays(date_occ), 
                   hypothesis = c("New Year", "Presidents Day", "Thanksgiving", "Thanksgiving", 
                                  "Christmas / New Year", "Presidents Day", "Thanksgiving", "Christmas", 
                                  "New Years", "MLK Day", "Thanksgiving", "Christmas", 
                                  "Christmas / New Year", "Christmas / New Year", "Random Sunday", 
                                  "MLK Day", "Christmas"))
  
  # Look at days with lowest collisions
  highest = outliers %>% 
    filter(daily_count > quantile(outliers$daily_count, 0.995)) #%>% 
    
    # mutate(day = weekdays(date_occ), 
    #        hypothesis = c("Random Friday", "Random Friday", "Random Friday", "Random Friday", 
    #                       "Random Wesnd", "Presidents Day", "Thanksgiving", "Christmas", 
    #                       "New Years", "MLK Day", "Thanksgiving", "Christmas", 
    #                       "Christmas / New Year", "Christmas / New Year", "Random Sunday", 
    #                       "MLK Day", "Christmas"))
    # 
  #### Conclusions
  
  # * Why are only some holidays associated with a low number of collisions? For example, MLK Day often has a low number of collisions but Independence Day never does.
  # * Most days with a low number of collisions occur before early 2014. This reflects the overall rise in collisions from 2014-2017.
  # * Most days with a higher number of collisions occur after 2015.
  # * There's interesting variation within holidays periods. For example, Christmas/New Year time 2011 contains 1 of the lowest-collision days, while the same period in 2013 contains 2.
  # * Most days with a high number of collisions are Fridays.
  # * May be interesting to look at weather for the highest collision days?
  # * Daylight Savings Time does **not** show up as any kind of outlier in any year
    
# KEY QUESTION 1 ----------------------------------------------------------
# How do traffic collision patterns vary by time of day, day of week, and time of year?
    
  # Plot daily collisions for 1 year
  a = data_clean %>% 
    filter(substr(date_occ, 1, 4) == "2018") %>%  # Limit to 2018
    group_by(date_occ) %>% summarize(daily_total = n()) %>% ungroup() %>% 
    
    ggplot(aes(x = as.Date(date_occ), y = daily_total)) + 
    geom_line() + geom_point() + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    ylim(0, 250) +
    labs(x = "", y = "Daily Collisions", title = "2018 Daily Collisions")
  
  # Plot daily collisions reported for 1 year
  # Decently different than collisions
  b = data_clean %>% 
    filter(substr(date_rptd, 1, 4) == "2018") %>%  # Limit to 2018
    group_by(date_rptd) %>% summarize(daily_total = n()) %>% ungroup() %>% 
    
    ggplot(aes(x = as.Date(date_rptd), y = daily_total)) + 
    geom_line() + geom_point() + 
    scale_x_date(date_breaks = "1 month", date_labels = "%b") + 
    ylim(0, 250) +
    labs(x = "", y = "Daily Collisions Reported", title = "2018 Daily Collisions Reported")
  
  grid.arrange(a, b)

  # Monthly collisions over time. Collisions very generally increasing over time
  # Plotting daily collisions is too chaotic
  a = data_clean %>% 
      mutate(date_mod = ymd(paste0(substr(date_occ, 1, 7), "-01"))) %>%
      group_by(date_mod) %>% summarize(month_total = n()) %>% ungroup() %>% 
      filter(date_mod != ymd("2019-08-01")) %>%              # Throw out incomplete month
      
    ggplot(aes(x = date_mod, y = month_total, group = 1)) + 
      geom_line() + geom_point() + 
      ylim(0, 5300) +
      labs(x = "", y = "Collisions", title = "Monthly Collisions")
    
  # Monthly collisions by date reported
  b = data_clean %>% 
    mutate(date_mod = ymd(paste0(substr(date_rptd, 1, 7), "-01"))) %>%
    group_by(date_mod) %>% summarize(month_total = n()) %>% ungroup() %>% 
    filter(date_mod != ymd("2019-08-01")) %>%              # Throw out incomplete month
    
    ggplot(aes(x = date_mod, y = month_total, group = 1)) + 
    geom_line() + geom_point() + 
    ylim(0, 5300) + 
    labs(x = "", y = "Collisions Reported", title = "Monthly Collisions Reported")
  
  grid.arrange(a, b)
    
  # Collisions by time of day. Interesting and useful!
  # Sharply increasing from 4-5am to 730/8am.
  # Drops from 730/8am to 830/9am. Generally increasing from 830/9am to 6pm.
  # Sharply decreasing from 6pm to 4-5am.
  # Collisions are lowest around 4-5am. Spike around 730/8am
  # Highest between 3-4pm and 5-6pm
  # Likely just mirrors number of vehicles on the road...
  # I only have time for collision occurrence, NOT reporting...
  ggplot(data = select(data_clean, time_occ), aes(x = time_occ)) + 
    geom_histogram(binwidth = 50, color = "black") + 
    scale_x_continuous(breaks = seq(0, 2400, 200)) +
    labs(x = "Time Occurred (24 Hour Format)", y = "Count", title = "Collisions by Time of Day")
  
  # Collisions by day of week.
  # Least collisions on Sunday, most on Friday by a lot.
  # Collisions increase from Sunday to Friday, with a big jump from Thurs to Fri
  a = data_clean %>% 
    mutate(day = weekdays(date_occ)) %>% 
    group_by(day) %>% 
    summarize(day_total = n()) %>% 
    ungroup() %>% 
    
    ggplot(aes(x = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), 
               y = day_total)) + 
    geom_bar(stat = "identity") + 
    ylim(0, 80000) +
    labs(x = "", y = "Collisions", title = "Collisions by Day of Week")
  
  # Collisions reported by day of week.
  # Sunday still the lowest, Friday still the highest
  # Weekdays are higher and even now
  b = data_clean %>% 
    mutate(day = weekdays(date_rptd)) %>% 
    group_by(day) %>% 
    summarize(day_total = n()) %>% 
    ungroup() %>% 
    
    ggplot(aes(x = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")), 
               y = day_total)) + 
    geom_bar(stat = "identity") + 
    ylim(0, 80000) +
    labs(x = "", y = "Collisions Reported", title = "Collisions Reported by Day of Week")
  
  grid.arrange(a, b)
  
  # Collisions by month
  # Interesting...most in March, least in Sept, Nov, and Dec
  # Pretty even from Apr-Aug.
  # Oct is higher than surrounding months.
  # Jan and Feb are pretty low
  a = data_clean %>% 
    mutate(month = as.numeric(substr(date_occ, 6, 7))) %>% 
    group_by(month) %>% 
    summarize(month_total = n()) %>% 
    ungroup() %>%
    
  ggplot(aes(x = month, y = month_total)) + 
    geom_bar(stat = "identity") + 
    scale_x_continuous(breaks = c(1:12)) + 
    ylim(0, 45000) + 
    labs(x = "Month", y = "Collisions", title = "Collisions by Month")
  
  # Collisions reported by month
  # Very similar to collisions
  b = data_clean %>% 
    mutate(month = as.numeric(substr(date_rptd, 6, 7))) %>% 
    group_by(month) %>% 
    summarize(month_total = n()) %>% 
    ungroup() %>%
    
    ggplot(aes(x = month, y = month_total)) + 
    geom_bar(stat = "identity") + 
    scale_x_continuous(breaks = c(1:12)) +
    ylim(0, 45000) + 
    labs(x = "Month", y = "Collisions Reported", title = "Collisions Reported by Month")
  
  grid.arrange(a, b)
  
  #### Conclusions   
  
  # * There's large daily variation in the number of collisions and collisions reported.   
  # * Collisions and collisions reported were roughly constant from 2010-2014, rose from 2014 to 2017, and have been roughly constant since.   
  # * Collisions are least frequent in the early morning and most frequent in the evening.   
  # * Collisions are least frequent on Sundays and most frequent on Fridays.   
  # * Collisions are least frequent in September and November and most frequent in March.   
  # * For certain time frames and granularity, collisions and collisions reported can vary substantially.   
  
# KEY QUESTION 2 ----------------------------------------------------------
# How are traffic collisions distributed geographically?
# Is it possible to identify high-risk intersections or areas?
  
  # Plot by "area_name"...a crude measure
  # Significant difference by area
  # Would be more useful w/ some measure of area size or traffic density
  data_clean %>% 
    group_by(area_name) %>%
    summarize(total = n()) %>%
    ungroup() %>%
    
    ggplot(aes(x = reorder(area_name, -total), y = total)) + 
    geom_bar(stat = "identity") + 
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "", y = "Count", title = "Collision Count by Area")
  
  # Other crude measures...most commonly occurring "location" and "cross_street"
  # These mainly reflect major streets.
  # A measure of traffic density would be useful
  
    # Most commonly occurring "location"
    data_clean %>% 
      group_by(location) %>% summarize(count = n()) %>% ungroup() %>% 
      mutate(percentage = round(100 * count / nrow(data_clean), 1)) %>%
      arrange(-count) %>% 
      head(10)
    
    # Most commonly occurring "cross_street"
    data_clean %>% 
      group_by(cross_street) %>% summarize(count = n()) %>% ungroup() %>% 
      mutate(percentage = round(100 * count / nrow(data_clean), 1)) %>%
      arrange(-count) %>% 
      head(10)
    
    # Most commonly occurring "location" and "cross_street" combo
    data_clean %>% 
      group_by(location, cross_street) %>% summarize(count = n()) %>% ungroup() %>% 
      mutate(percentage = round(100 * count / nrow(data_clean), 2)) %>%
      arrange(-count) %>% 
      head(10)
  
  # Create version of data for mapping
  
    # ~500K points is way too many to plot
    # "leaflet" starts lagging at ~10K points
    # There are only ~40K unique lat/long combos in the data
    select(data_clean, lat, lon) %>% unique() %>% nrow()
  
    # Create version of data for mapping
    # Data is pretty cluttered so I do it for one year only
    data_map = data_clean %>% 
                filter(substr(date_occ, 1, 4) == "2018" &   # 2018 only
                       lat != 0 & lon != 0) %>%  # Remove rows where "lat" and "lon" are both 0
                group_by(lat, lon) %>% summarize(count = n()) %>% ungroup()
    
    # Overall map
    # Cluttered, not super useful. But shows the weird shape of Los Angeles
    m = get_map(location = c(lon = mean(data_clean$lon), lat = mean(data_clean$lat)), 
                zoom = 10, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = count), data = data_map) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      scale_colour_gradient(low = "cornflowerblue", high = "red") +
      labs(x = "", y = "", title = "2018 Collisions: All of Los Angeles", color = "Collisions")
    
    # Area maps
    
      # Valley
      m = get_map(location = c(lon = -118.47, lat = 34.3), 
                  zoom = 11, maptype = "roadmap", scale = 2)
      ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = count), data = data_map) + 
        theme(axis.text = element_blank(), 
              axis.ticks = element_blank()) +
        scale_alpha(guide = "none") +
        scale_colour_gradient(low = "cornflowerblue", high = "red") +
        labs(x = "", y = "", title = "The Valley: 2018 Collisions", color = "Collisions")
      
      # East LA
      m = get_map(location = c(lon = -118.285, lat = 34.0407), 
                  zoom = 12, maptype = "roadmap", scale = 2)
      ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = count), data = data_map) + 
        theme(axis.text = element_blank(), 
              axis.ticks = element_blank()) +
        scale_alpha(guide = "none") +
        scale_colour_gradient(low = "cornflowerblue", high = "red") +
        labs(x = "", y = "", title = "2018 Collisions: Central and Downtown LA", color = "Collisions")
      
      # Westside
      m = get_map(location = c(lon = -118.415, lat = 34.0), 
                  zoom = 12, maptype = "roadmap", scale = 2)
      ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = count), data = data_map) + 
        theme(axis.text = element_blank(), 
              axis.ticks = element_blank()) +
        scale_alpha(guide = "none") +
        scale_colour_gradient(low = "cornflowerblue", high = "red") +
        labs(x = "", y = "", title = "West LA: 2018 Collisions", color = "Collisions")
      
      # Long Beach
      m = get_map(location = c(lon = -118.2937, lat = 33.7901), 
                  zoom = 12, maptype = "roadmap", scale = 2)
      ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = count), data = data_map) + 
        theme(axis.text = element_blank(), 
              axis.ticks = element_blank()) + 
        scale_alpha(guide = "none") +
        scale_colour_gradient(low = "cornflowerblue", high = "red") +
        labs(x = "", y = "", title = "Long Beach: 2018 Collisions", color = "Collisions")
      
  # Create a version for mapping average time of accident
      
    # Create version of data for mapping average accident time
    # Data is pretty cluttered so I do it for one year only
    data_map_time = data_clean %>% 
                      filter(substr(date_occ, 1, 4) == "2018" &   # 2018 only
                               lat != 0 & lon != 0) %>%           # Remove rows where "lat" and "lon" are both 0
      
                      mutate(daypart = case_when(time_occ >= 2200 | time_occ < 400 ~ "Late Night (10PM - 4AM)",      # Create dayparts
                                                 time_occ >= 400 & time_occ < 800 ~ "Early Morning (4AM - 8AM)", 
                                                 time_occ >= 800 & time_occ < 1200 ~ "Late Morning (8AM - 12PM)", 
                                                 time_occ >= 1200 & time_occ < 1700 ~ "Afternoon (12PM - 5PM)", 
                                                 TRUE ~ "Evening (5PM - 10PM)"), 
                             
                             daypart = factor(daypart, levels = c("Early Morning (4AM - 8AM)", "Late Morning (8AM - 12PM)", 
                                                                  "Afternoon (12PM - 5PM)", "Evening (5PM - 10PM)", 
                                                                  "Late Night (10PM - 4AM)"))) %>% 
      
                      group_by(lat, lon) %>% mutate(total = n()) %>% ungroup() %>%             # Total collisions per coordinate
      
                      group_by(lat, lon, total, daypart) %>% summarize(count = n()) %>% ungroup() %>%    # Count collisions per coordinate / daypart
                      
                      group_by(lat, lon) %>% mutate(max_val = max(count)) %>% ungroup() %>%      # Only keep max collision dayparts
                      filter(count == max_val) %>% 
      
                      group_by(lat, lon) %>% mutate(row_count = n()) %>% ungroup() %>%    # Remove coordinates with equal collisions in 
                      filter(row_count == 1)                                              # different dayparts. This could lose some big coordinates
    
    # Create color scale
    daypart_values = c("pink", "blue", "indianred1", "purple", "black")
    
    # Overall map
    # Too cluttered, don't include in outputs
    m = get_map(location = c(lon = mean(data_map_time$lon), lat = mean(data_map_time$lat)), 
                zoom = 10, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = daypart), data = data_map_time) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) +
      labs(x = "", y = "", title = "Overall Map: 2018 Collisions by Most Common Daypart", color = "Most Common Daypart")
    
    # Valley
    m = get_map(location = c(lon = -118.47, lat = 34.3), 
                zoom = 11, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = daypart), 
                          data = filter(data_map_time, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) + 
      facet_grid(cols = vars(daypart)) +
      labs(x = "", y = "", title = "The Valley: 2018 Collisions by Daypart (Coordinates with 5+ Collisions)", color = "Most Common Daypart")
    
    m = get_map(location = c(lon = -118.47, lat = 34.3), 
                zoom = 11, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = daypart), 
                          data = filter(data_map_time, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) +
      labs(x = "", y = "", title = "The Valley: 2018 Most Common Daypart for Coordinates with 5+ Collisions", 
           color = "Most Common Daypart")
    
    # East LA
    m = get_map(location = c(lon = -118.285, lat = 34.0407), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = daypart), 
                          data = filter(data_map_time, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) + 
      facet_grid(rows = vars(daypart)) +
      labs(x = "", y = "", title = "2018 Collisions by Daypart: Central and Downtown LA (Coordinates with 5+ Collisions)", color = "Most Common Daypart")
    
    m = get_map(location = c(lon = -118.285, lat = 34.0407), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = daypart), 
                          data = filter(data_map_time, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) +
      labs(x = "", y = "", title = "East LA: 2018 Most Common Daypart for Coordinates with 5+ Collisions", 
           color = "Most Common Daypart")
    
    # Westside
    m = get_map(location = c(lon = -118.415, lat = 34.0), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = daypart), 
                          data = filter(data_map_time, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) + 
      facet_grid(cols = vars(daypart)) +
      labs(x = "", y = "", title = "West LA: 2018 Collisions by Daypart (Coordinates with 5+ Collisions)", color = "Most Common Daypart")
    
    m = get_map(location = c(lon = -118.415, lat = 34.0), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = daypart), 
                          data = filter(data_map_time, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) +
      labs(x = "", y = "", title = "West LA: 2018 Most Common Daypart for Coordinates with 5+ Collisions", 
           color = "Most Common Daypart")
    
    # Long Beach
    m = get_map(location = c(lon = -118.2937, lat = 33.7901), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = daypart), 
                          data = filter(data_map_time, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) + 
      facet_grid(cols = vars(daypart)) +
      labs(x = "", y = "", title = "Long Beach: 2018 Collisions by Daypart (Coordinates with 5+ Collisions)", color = "Most Common Daypart")
    
    m = get_map(location = c(lon = -118.2937, lat = 33.7901), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = daypart), 
                          data = filter(data_map_time, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") +
      scale_color_manual(values = daypart_values) +
      labs(x = "", y = "", title = "Long Beach: 2018 Most Common Daypart for Coordinates with 5+ Collisions", 
           color = "Most Common Daypart")
    
  # Create a version for mapping weekpart of accident
    
    # Create version of data for mapping most common accident weekpart
    # Data is pretty cluttered so I do it for one year only
    data_map_week = data_clean %>% 
      filter(substr(date_occ, 1, 4) == "2018" &   # 2018 only
               lat != 0 & lon != 0) %>%           # Remove rows where "lat" and "lon" are both 0
      
      mutate(day = weekdays(date_occ), 
             week_type = if_else(day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")) %>% 
      
      group_by(lat, lon) %>% mutate(total = n()) %>% ungroup() %>%             # Total collisions per coordinate
      
      group_by(lat, lon, total, week_type) %>% summarize(count = n()) %>% ungroup() %>%    # Count collisions per coordinate / daypart
      
      group_by(lat, lon) %>% mutate(max_val = max(count)) %>% ungroup() %>%      # Only keep max collision dayparts
      filter(count == max_val) %>% 
      
      group_by(lat, lon) %>% mutate(row_count = n()) %>% ungroup() %>%    # Remove coordinates with equal collisions in 
      filter(row_count == 1)                                              # different dayparts. This could lose some big coordinates
    
    # Overall map
    # Too cluttered, don't include in outputs
    m = get_map(location = c(lon = mean(data_map_week$lon), lat = mean(data_map_week$lat)), 
                zoom = 10, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = week_type), data = data_map_week) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") +
      labs(x = "", y = "", title = "Overall Map: 2018 Collisions by Weekpart", color = "Most Common Weekpart")
    
    # Valley
    m = get_map(location = c(lon = -118.47, lat = 34.3), 
                zoom = 11, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = week_type), data = filter(data_map_week, total >= 3)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(week_type)) + 
      scale_color_manual(values = c("#F8766D", "blue")) +
      labs(x = "", y = "", title = "The Valley: 2018 Collisions by Weekpart (Coordinates with 3+ Collisions)", color = "Most Common Weekpart")
    
    m = get_map(location = c(lon = -118.47, lat = 34.3), 
                zoom = 11, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = week_type), 
                          data = filter(data_map_week, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      labs(x = "", y = "", title = "The Valley: 2018 Most Common Weekpart for Coordinates with 5+ Collisions", 
           color = "Most Common Weekpart")
    
    # East LA
    m = get_map(location = c(lon = -118.285, lat = 34.0407), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = week_type), data = filter(data_map_week, total >= 3)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(week_type)) + 
      scale_color_manual(values = c("#F8766D", "blue")) +
      labs(x = "", y = "", title = "2018 Collisions by Weekpart: Central and Downtown LA (Coordinates with 3+ Collisions)", 
           color = "Most Common Weekpart")
    
    m = get_map(location = c(lon = -118.285, lat = 34.0407), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = week_type), 
                          data = filter(data_map_week, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      labs(x = "", y = "", title = "East LA: 2018 Most Common Weekpart for Coordinates with 5+ Collisions", 
           color = "Most Common Weekpart")
    
    # Westside
    m = get_map(location = c(lon = -118.415, lat = 34.0), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = week_type), data = filter(data_map_week, total >= 3)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(week_type)) + 
      scale_color_manual(values = c("#F8766D", "blue")) +
      labs(x = "", y = "", title = "West LA: 2018 Collisions by Weekpart (Coordinates with 3+ Collisions)", color = "Most Common Weekpart")
    
    m = get_map(location = c(lon = -118.415, lat = 34.0), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = week_type), 
                          data = filter(data_map_week, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(week_type)) +
      labs(x = "", y = "", title = "West LA: 2018 Most Common Weekpart for Coordinates with 5+ Collisions", 
           color = "Most Common Weekpart")
    
    # Long Beach
    m = get_map(location = c(lon = -118.2937, lat = 33.7901), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = week_type), data = filter(data_map_week, total >= 3)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(week_type)) + 
      scale_color_manual(values = c("#F8766D", "blue")) +
      labs(x = "", y = "", title = "Long Beach: 2018 Collisions by Weekpart (Coordinates with 3+ Collisions)", color = "Most Common Weekpart")
    
    m = get_map(location = c(lon = -118.2937, lat = 33.7901), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = week_type), 
                          data = filter(data_map_week, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") +
      labs(x = "", y = "", title = "Long Beach: 2018 Most Common Weekpart for Coordinates with 5+ Collisions", 
           color = "Most Common Weekpart")
    
  # Create a version for mapping season of accident
    
    # Create version of data for mapping most common accident season
    # Data is pretty cluttered so I do it for one year only
    data_map_season = data_clean %>% 
      filter(substr(date_occ, 1, 4) == "2018" &   # 2018 only
               lat != 0 & lon != 0) %>%           # Remove rows where "lat" and "lon" are both 0
      
      mutate(month = as.numeric(substr(date_occ, 6, 7)), 
             season = case_when(month %in% c(12, 1, 2) ~ "Dec-Feb", 
                                month %in% c(3, 4, 5) ~ "Mar-May", 
                                month %in% c(6, 7, 8) ~ "June-Aug", 
                                TRUE ~ "Sep-Nov"), 
             season = factor(season, levels = c("Dec-Feb", "Mar-May", "June-Aug", "Sep-Nov"))) %>%
      
      group_by(lat, lon) %>% mutate(total = n()) %>% ungroup() %>%             # Total collisions per coordinate
      
      group_by(lat, lon, total, season) %>% summarize(count = n()) %>% ungroup() %>%    # Count collisions per coordinate / season
      
      group_by(lat, lon) %>% mutate(max_val = max(count)) %>% ungroup() %>%      # Only keep max collision dayparts
      filter(count == max_val) %>% 
      
      group_by(lat, lon) %>% mutate(row_count = n()) %>% ungroup() %>%    # Remove coordinates with equal collisions in 
      filter(row_count == 1)                                              # different dayparts. This could lose some big coordinates
    
    # Overall map
    # Too cluttered, don't include in outputs
    m = get_map(location = c(lon = mean(data_map_season$lon), lat = mean(data_map_season$lat)), 
                zoom = 10, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = season), data = data_map_season) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") +
      labs(x = "", y = "", title = "Overall Map: 2018 Collisions by Season", color = "Most Common Season")
    
    # Valley
    m = get_map(location = c(lon = -118.47, lat = 34.3), 
                zoom = 11, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = season), data = filter(data_map_season, total >= 3)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(season)) + 
      labs(x = "", y = "", title = "The Valley: 2018 Collisions by Season (Coordinates with 3+ Collisions)", color = "Most Common Season")
    
    m = get_map(location = c(lon = -118.47, lat = 34.3), 
                zoom = 11, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = season), 
                          data = filter(data_map_season, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      labs(x = "", y = "", title = "The Valley: 2018 Most Common Season for Coordinates with 5+ Collisions", 
           color = "Most Common Season")
    
    # East LA
    m = get_map(location = c(lon = -118.285, lat = 34.0407), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = season), data = filter(data_map_season, total >= 3)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") + 
      facet_grid(rows = vars(season)) + 
      labs(x = "", y = "", title = "2018 Collisions by Season: Central and Downtown LA (Coordinates with 3+ Collisions)", color = "Most Common Season")
    
    m = get_map(location = c(lon = -118.285, lat = 34.0407), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = season), 
                          data = filter(data_map_season, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") +
      labs(x = "", y = "", title = "2018 Most Common Season for Coordinates with 5+ Collisions", 
           color = "Most Common Season")
    
    # Westside
    m = get_map(location = c(lon = -118.415, lat = 34.0), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = season), data = filter(data_map_season, total >= 3)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(season)) + 
      labs(x = "", y = "", title = "West LA: 2018 Collisions by Season (Coordinates with 3+ Collisions)", color = "Most Common Season")
    
    m = get_map(location = c(lon = -118.415, lat = 34.0), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = season), 
                          data = filter(data_map_season, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) +
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(season)) + 
      labs(x = "", y = "", title = "West LA: 2018 Most Common Season for Coordinates with 5+ Collisions", 
           color = "Most Common Season")
    
    # Long Beach
    m = get_map(location = c(lon = -118.2937, lat = 33.7901), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, alpha = count, color = season), data = filter(data_map_season, total >= 3)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(season)) + 
      labs(x = "", y = "", title = "Long Beach: 2018 Collisions by Season (Coordinates with 3+ Collisions)", color = "Most Common Season")
    
    m = get_map(location = c(lon = -118.2937, lat = 33.7901), 
                zoom = 12, maptype = "roadmap", scale = 2)
    ggmap(m) + geom_point(aes(x = lon, y = lat, color = season), 
                          data = filter(data_map_season, total >= 5)) + 
      theme(axis.text = element_blank(), 
            axis.ticks = element_blank()) + 
      scale_alpha(guide = "none") + 
      facet_grid(cols = vars(season)) + 
      labs(x = "", y = "", title = "Long Beach: 2018 Most Common Season for Coordinates with 5+ Collisions", 
           color = "Most Common Season")
      
  #### Conclusions   
  
  # * ~5% of collisions don't have an associated *cross_street*   
  # * Most locations with the highest number of collisions are in the Valley, East LA, or South LA

# KEY QUESTIONS 3 ---------------------------------------------------------
# How accurately can the number of collisions be predicted?
# I try to predict at the month / area level
  
  # Get data
  model_data = data_clean %>%
                mutate(month = ymd(paste0(substr(date_occ, 1, 7), "-01"))) %>% # Create month variable 
                filter(month != ymd("2019-08-01")) %>%                         # Throw out partial month
                group_by(month, area) %>% summarize(collisions = n()) %>% ungroup()
      
  skim(model_data) # Examine data
  
  # Sample area plot
  ggplot(data = filter(model_data, area == 2), aes(x = month, y = collisions)) + 
    geom_line() + geom_point() + 
    expand_limits(y = 0) +
    labs(x = "", y = "Monthly Collisions", title = "Monthly Collisions for Area 2")
  
  # Sample area decomposition
  ts(filter(model_data, area == 2)$collisions, frequency = 12) %>% decompose %>% autoplot(main = "Area 2 Decomposition")
  
  # All area decomposition
  ts((data_clean %>% 
    mutate(date_mod = ymd(paste0(substr(date_occ, 1, 7), "-01"))) %>%
    group_by(date_mod) %>% summarize(month_total = n()) %>% ungroup() %>% 
    filter(date_mod != ymd("2019-08-01")))$month_total, frequency = 12) %>% decompose %>% autoplot(main = "Overall Data Decomposition")
  
  # Auto-correlation and partial auto-correlation plot for Area 2
  a = ggAcf(filter(model_data, area == 2)$collisions) + labs(title = "ACF for Area 2")
  b = ggPacf(filter(model_data, area == 2)$collisions) + labs(title = "PACF for Area 2")
  
  grid.arrange(a, b)
  
  # Look at best p, q, d for ARIMA
  auto.arima(filter(model_data, area == 2)$collisions)
  
  # Moving average predictions
  ma_preds = model_data %>% 
              arrange(area, month) %>%
              group_by(area) %>%
    
              mutate(m3_pred = (lag(collisions, 1) + lag(collisions, 2) + lag(collisions, 3)) / 3,                    # Compute MA preds
                     m6_pred = (lag(collisions, 1) + lag(collisions, 2) + lag(collisions, 3) + lag(collisions, 4) + 
                             lag(collisions, 5) + lag(collisions, 6)) / 6) %>% 
              ungroup() %>% 
              
              filter(month >= ymd("2018-08-01")) %>%                   # Limit to test set
  
              mutate(m3_mape = abs((m3_pred - collisions) / collisions),        # Compute MAPE
                     m6_mape = abs((m6_pred - collisions) / collisions), 
                     m3_bias = (m3_pred - collisions) / collisions, 
                     m6_bias = (m6_pred - collisions) / collisions)
  
  # ARIMA and Prophet models
  
    preds = model_data %>%                  # Create data frame for Prophet results
      filter(month >= ymd("2018-08-01")) %>% 
      mutate(prophet_pred = 0, arima_pred = 0)
    
    mod_data = model_data %>%                   # Prepare data for Prophet
      rename(ds = month, y = collisions)
    
    # Specify validation set months
    val_months = c("2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", 
                       "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", 
                       "2019-06-01", "2019-07-01")
    
    # Generate predictions with ARIMA and Prophet models
    for (a in unique(mod_data$area)) {
      
      print(paste0("Area: ", a))
      
      for (m in val_months) {
        
        subset = mod_data %>%                                        # Get subset of data
          filter(area == a & ds < ymd(m))
        
        pm = prophet(subset)                                              # Create Prophet model
        future = make_future_dataframe(pm, freq = "month", periods = 1)   # Create future dataframe
        prophet_forecast = predict(pm, future)                                    # Generate predictions
        
        arima_model = auto.arima(subset$y)
        arima_forecast = forecast(arima_model, 1)
        
        prophet_to_add = prophet_forecast %>% filter(ds == ymd(m))                        # Get relevant subset of "forecast"
        
        # Add prediction to "prophet_preds"
        preds = preds %>% 
          mutate(prophet_pred = if_else(month == m & area == a, prophet_to_add$yhat, prophet_pred), 
                 arima_pred = if_else(month == m & area == a, as.numeric(arima_forecast$mean), arima_pred))
        
      }
      
    }
  
    # Compute error metrics for Prophet model
    preds = preds %>% 
                      mutate(prophet_mape = abs((prophet_pred - collisions) / collisions),        # Compute MAPE
                             prophet_bias = (prophet_pred - collisions) / collisions, 
                             arima_mape = abs((arima_pred - collisions) / collisions),        # Compute MAPE
                             arima_bias = (arima_pred - collisions) / collisions)
    
    # More Prophet testing
    # Standard spec: Mean MAPE = 0.0915, Mean Bias = 0.069
    # seasonality.prior.scale = 5: Mean MAPE - 0.0914, Mean Bias = 0.069
    # seasonality.prior.scale = 1: Mean MAPE - 0.0911, Mean Bias = 0.069
    # seasonality.prior.scale = 0.5: Mean MAPE - 0.0916, Mean Bias = 0.069
    
      more_preds = model_data %>%                  # Create data frame for Prophet results
        filter(month >= ymd("2018-08-01")) %>% 
        mutate(prophet_pred = 0)
      
      prophet_data = model_data %>%                   # Prepare data for Prophet
        rename(ds = month, y = collisions)
      
      # Specify validation set months
      val_months = c("2018-08-01", "2018-09-01", "2018-10-01", "2018-11-01", "2018-12-01", 
                     "2019-01-01", "2019-02-01", "2019-03-01", "2019-04-01", "2019-05-01", 
                     "2019-06-01", "2019-07-01")
      
      # Generate predictions with Prophet
      for (a in unique(prophet_data$area)) {
        
        print(paste0("Area: ", a))
        
        for (m in val_months) {
          
          subset = prophet_data %>%                                        # Get subset of data
            filter(area == a & ds < ymd(m))
          
          pm = prophet(subset, seasonality.prior.scale = 0.5)                                              # Create Prophet model
          future = make_future_dataframe(pm, freq = "month", periods = 1)   # Create future dataframe
          forecast = predict(pm, future)                                    # Generate predictions
          
          to_add = forecast %>% filter(ds == ymd(m))                        # Get relevant subset of "forecast"
          
          # Add prediction to "more_preds"
          more_preds = more_preds %>% 
            mutate(prophet_pred = if_else(month == m & area == a, to_add$yhat, prophet_pred))
          
        }
        
      }
      
      # Compute error metrics for Prophet model
      more_preds = more_preds %>% 
        mutate(prophet_mape = abs((prophet_pred - collisions) / collisions),        # Compute MAPE
               prophet_bias = (prophet_pred - collisions) / collisions)
    
  # Combine all predictions together
  all_preds = inner_join(ma_preds, select(preds, -collisions), by = c("month", "area"))
  write_csv(all_preds, "all_preds.csv")
  
  # Look at data and preds for area 2
  model_data %>% 
    filter(area == 2 & month >= ymd("2018-05-01")) %>% 
    left_join(select(all_preds, -c(collisions, m3_mape, m3_bias, m6_mape, m6_bias, arima_mape, arima_bias, prophet_mape, prophet_bias)), 
              by = c("month", "area")) %>% 
    rename(Actual = collisions, ARIMA = arima_pred, `3 Month MA` = m3_pred, `6 Month MA` = m6_pred, Prophet = prophet_pred) %>%
    gather(cat, val, -c(month, area)) %>% 
    mutate(cat = factor(cat, levels = c("3 Month MA", "6 Month MA", "ARIMA", "Prophet", "Actual"))) %>%
  
  ggplot(aes(x = month, y = val, group = cat, color = cat)) + 
    geom_line() + geom_point() + 
    scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "black")) +
    expand_limits(y = 0) + 
    labs(x = "", title = "Actual Collisions and Model Predictions")
  
  # Visualize performance
  
    # Table of mean overall performance per model
    all_preds %>% summarize(mean_m3_mape = round(mean(m3_mape), 4), 
                            mean_m6_mape = round(mean(m6_mape), 4), 
                            mean_arima_mape = round(mean(arima_mape), 4),
                            mean_prophet_mape = round(mean(prophet_mape), 4), 
                            mean_m3_bias = round(mean(m3_bias), 4), 
                            mean_m6_bias = round(mean(m6_bias), 4),
                            mean_arima_bias = round(mean(arima_bias), 4),
                            mean_prophet_bias = round(mean(prophet_bias), 4))

    # Average MAPE per model per month
    all_preds %>% 
      group_by(month) %>% summarize(`3 Month MA` = mean(m3_mape), 
                                    `6 Month MA` = mean(m6_mape), 
                                    `ARIMA` = mean(arima_mape),
                                    Prophet = mean(prophet_mape)) %>% ungroup() %>%
      gather(key = "model", value = "error", -month) %>%
      
      ggplot(aes(x = month, y = error, group = model, color = model)) + 
      geom_line() + geom_point() +
      expand_limits(y = 0) + 
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") +
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "", y = "MAPE", title = "Avg. Monthly MAPE for Validation Data", color = "Model")
    
    # Average bias per model per month
    # Moving average model biases tend to move together
    # Prophet model seems to typically over-estimate
    all_preds %>% 
      group_by(month) %>% summarize(`3 Month MA` = mean(m3_bias), 
                                    `6 Month MA` = mean(m6_bias), 
                                    `ARIMA` = mean(arima_bias),
                                    Prophet = mean(prophet_bias)) %>% ungroup() %>%
      gather(key = "model", value = "error", -month) %>% 
      
      ggplot(aes(x = month, y = error, group = model, color = model)) + 
      geom_line() + geom_point() + 
      ylim(-0.1, 0.15) +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
      theme(axis.text.x = element_text(angle = 90)) +
      labs(x = "", y = "Bias", title = "Avg. Monthly Bias for Validation Data", color = "Model")
    
    # Average MAPE per model per area
    # Chaotic but...
    # Varying MAPEs by "area"
    # Interesting edge cases where Prophet model is the only one that sucks ("area" 12)
    # Or where only 3 and 6 month MA model suck ("area" 8)
    all_preds %>% 
      group_by(area) %>% summarize(`3 Month MA` = mean(m3_mape), 
                                   `6 Month MA` = mean(m6_mape), 
                                   `ARIMA` = mean(arima_mape),
                                   Prophet = mean(prophet_mape)) %>% ungroup() %>% 
      gather(key = "model", value = "error", -area) %>%
      
      ggplot(aes(x = as.numeric(area), y = error, fill = model)) + 
      geom_bar(position = "dodge", stat = "identity") + 
      scale_x_continuous(breaks = 1:21) +
      labs(x = "Area", y = "MAPE", title = "Avg. Area MAPE for Validation Data", fill = "Model")
    
    # Average bias per model per area
    # Chaotic but...
    # Varying biases by "area"
    # Interesting edge cases...
    # Area 1, Prophet has positive bias, no other model does
    # Area 17, 3 month MA has negative bias, no other model does
    # Looks like Prophet model really stinks when it comes to bias
    all_preds %>% 
      group_by(area) %>% summarize(`3 Month MA` = mean(m3_bias), 
                                   `6 Month MA` = mean(m6_bias), 
                                   `ARIMA` = mean(arima_bias),
                                   Prophet = mean(prophet_bias)) %>% ungroup() %>% 
      gather(key = "model", value = "error", -area) %>%
      
      ggplot(aes(x = as.numeric(area), y = error, fill = model)) + 
      geom_bar(position = "dodge", stat = "identity") + 
      scale_x_continuous(breaks = 1:21) +
      labs(x = "Area", y = "Bias", title = "Avg. Area Bias for Validation Data", fill = "Model")
    
    # Highlight worst predictions
    # All in 2018 or 2019
    # Interesting that all models did pretty bad on certain months
    # Like 2019-01-01 / 2
    
      skim(all_preds)     # Look for worst performance in MAPE/bias
      
      filter(all_preds, m3_mape > 0.25)       # 2018-09-01 / 14, 2019-01-01 / 2
      filter(all_preds, m6_mape > 0.26)       # 2019-01-01 / 2,  2019-02-01 / 14
      filter(all_preds, arima_mape > 0.28)    # 2019-01-01 / 2
      filter(all_preds, prophet_mape > 0.33)  # 2019-05-01 / 11
      
      filter(all_preds, m3_bias > 0.25)       # 2018-09-01 / 14, 2019-01-01 / 2 
      filter(all_preds, m6_bias > 0.26)       # 2019-01-01 / 2,  2019-02-01 / 14 
      filter(all_preds, arima_bias > 0.28)    # 2019-01-01 / 2
      filter(all_preds, prophet_bias > 0.33)  # 2019-05-01 / 11
      
    # Daily attempt
      
      # Get data
      daily_data = data_clean %>% 
                    filter(date_occ < ymd("2019-08-01")) %>% 
                    group_by(date_occ, area) %>% summarize(collisions = n()) %>% ungroup()
      
      skim(daily_data) # Examine data
      
      # Sample area plot
      ggplot(data = filter(daily_data, area == 2 & date_occ >= "2018-01-01" & date_occ < "2018-06-01"), 
             aes(x = ymd(date_occ), y = collisions)) + 
        geom_line() + geom_point() + 
        expand_limits(y = 0) + 
        scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
        labs(x = "", y = "Daily Collisions", title = "Daily Collisions for Area 2")
      
      # Moving average predictions
      daily_ma_preds = daily_data %>% 
        arrange(area, date_occ) %>%
        group_by(area) %>%
        
        mutate(m3_pred = (lag(collisions, 1) + lag(collisions, 2) + lag(collisions, 3)) / 3,                    # Compute MA preds
               m6_pred = (lag(collisions, 1) + lag(collisions, 2) + lag(collisions, 3) + lag(collisions, 4) + 
                            lag(collisions, 5) + lag(collisions, 6)) / 6) %>% 
        ungroup() %>% 
        
        filter(date_occ >= ymd("2019-07-01")) %>%                   # Limit to test set
        
        mutate(m3_mape = abs((m3_pred - collisions) / collisions),        # Compute MAPE
               m6_mape = abs((m6_pred - collisions) / collisions), 
               m3_bias = (m3_pred - collisions) / collisions, 
               m6_bias = (m6_pred - collisions) / collisions)
      
      # ARIMA and Prophet models
      
        preds = daily_data %>%                  # Create data frame for Prophet results
          filter(date_occ >= ymd("2019-07-01")) %>% 
          mutate(prophet_pred = 0, arima_pred = 0)
        
        mod_data = daily_data %>%                   # Prepare data for Prophet
          rename(ds = date_occ, y = collisions)
        
        # Specify validation set months
        val_days = c(ymd("2019-07-01") : ymd("2019-07-31"))
        
        # Generate predictions with ARIMA and Prophet models
        for (a in unique(mod_data$area)) {
          
          print(paste0("Area: ", a))
          
          for (m in val_days) {
            
            m = as.Date(m)
            
            subset = mod_data %>%                                        # Get subset of data
              filter(area == a & ds < ymd(m)) %>% 
              mutate(ds = ymd(ds))
            
            pm = prophet(subset)                                              # Create Prophet model
            future = make_future_dataframe(pm, freq = "day", periods = 1)   # Create future dataframe
            prophet_forecast = predict(pm, future)                                    # Generate predictions
            
            arima_model = auto.arima(subset$y)
            arima_forecast = forecast(arima_model, 1)
            
            prophet_to_add = prophet_forecast %>% filter(ds == ymd(m))                        # Get relevant subset of "forecast"
            
            # Add prediction to "prophet_preds"
            preds = preds %>% 
              mutate(prophet_pred = if_else(date_occ == m & area == a, prophet_to_add$yhat, prophet_pred), 
                     arima_pred = if_else(date_occ == m & area == a, as.numeric(arima_forecast$mean), arima_pred))
            
          }
          
        }
        
        # Compute error metrics for Prophet model
        preds = preds %>% 
          mutate(prophet_mape = abs((prophet_pred - collisions) / collisions),        # Compute MAPE
                 prophet_bias = (prophet_pred - collisions) / collisions, 
                 arima_mape = abs((arima_pred - collisions) / collisions),        # Compute MAPE
                 arima_bias = (arima_pred - collisions) / collisions)
        
        # Combine all predictions together
        all_daily_preds = inner_join(daily_ma_preds, select(preds, -collisions), by = c("date_occ", "area"))
        write_csv(all_daily_preds, "all_daily_preds.csv")
        
        all_daily_preds = read_csv("all_daily_preds.csv")
        all_daily_preds = mutate(all_daily_preds, 
                                 date_occ = substr(as.character(all_daily_preds$date_occ), 1, 10), 
                                 area = as.character(area))
        
        # Look at data and preds for area 2
        daily_data %>% 
          filter(area == 2 & date_occ >= ymd("2019-07-01")) %>% 
          left_join(select(all_daily_preds, -c(collisions, m3_mape, m3_bias, m6_mape, m6_bias, arima_mape, arima_bias, prophet_mape, prophet_bias)), 
                    by = c("date_occ", "area")) %>% 
          rename(Actual = collisions, ARIMA = arima_pred, `3 Month MA` = m3_pred, `6 Month MA` = m6_pred, Prophet = prophet_pred) %>%
          gather(cat, val, -c(date_occ, area)) %>% 
          mutate(cat = factor(cat, levels = c("3 Month MA", "6 Month MA", "ARIMA", "Prophet", "Actual")), 
                 date_occ = ymd(date_occ)) %>%
          
          ggplot(aes(x = date_occ, y = val, group = cat, color = cat)) + 
          geom_line() + geom_point() + 
          scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "black")) +
          expand_limits(y = 0) + 
          labs(x = "", y = "", title = "Actual Collisions and Model Predictions for Area 2 (Daily Model)", color = "Model")
        
        # Visualize performance
        
          # Table of mean overall performance per model
          all_daily_preds %>% summarize(mean_m3_mape = round(mean(m3_mape), 4), 
                                    mean_m6_mape = round(mean(m6_mape), 4), 
                                    mean_arima_mape = round(mean(arima_mape), 4),
                                    mean_prophet_mape = round(mean(prophet_mape), 4), 
                                    mean_m3_bias = round(mean(m3_bias), 4), 
                                    mean_m6_bias = round(mean(m6_bias), 4),
                                    mean_arima_bias = round(mean(arima_bias), 4),
                                    mean_prophet_bias = round(mean(prophet_bias), 4)) %>% 
            data.frame()
        
        # Average MAPE per model per month
        all_daily_preds %>% 
          group_by(date_occ) %>% summarize(`3 Month MA` = mean(m3_mape), 
                                        `6 Month MA` = mean(m6_mape), 
                                        `ARIMA` = mean(arima_mape),
                                        Prophet = mean(prophet_mape)) %>% ungroup() %>%
          gather(key = "model", value = "error", -date_occ) %>%
          
          ggplot(aes(x = date_occ, y = error, group = model, color = model)) + 
          geom_line() + geom_point() +
          expand_limits(y = 0) + 
          theme(axis.text.x = element_text(angle = 90)) +
          labs(x = "", y = "MAPE", title = "Avg. Daily MAPE for Validation Data", color = "Model")
        
        # Bias
        # Average bias per model per month
        # Moving average model biases tend to move together
        # Prophet model seems to typically over-estimate
        all_daily_preds %>% 
          group_by(date_occ) %>% summarize(`3 Month MA` = mean(m3_bias), 
                                        `6 Month MA` = mean(m6_bias), 
                                        `ARIMA` = mean(arima_bias),
                                        Prophet = mean(prophet_bias)) %>% ungroup() %>%
          gather(key = "model", value = "error", -date_occ) %>% 
          
          ggplot(aes(x = date_occ, y = error, group = model, color = model)) + 
          geom_line() + geom_point() + 
          #ylim(-0.1, 0.15) +
          #cale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
          theme(axis.text.x = element_text(angle = 90)) +
          labs(x = "", y = "Bias", title = "Avg. Monthly Bias for Validation Data", color = "Model")
        
        # Average MAPE per model per area
        all_daily_preds %>% 
          group_by(area) %>% summarize(`3 Month MA` = mean(m3_mape), 
                                       `6 Month MA` = mean(m6_mape), 
                                       `ARIMA` = mean(arima_mape),
                                       Prophet = mean(prophet_mape)) %>% ungroup() %>% 
          gather(key = "model", value = "error", -area) %>%
          
          ggplot(aes(x = as.numeric(area), y = error, fill = model)) + 
          geom_bar(position = "dodge", stat = "identity") + 
          scale_x_continuous(breaks = 1:21) +
          labs(x = "Area", y = "MAPE", title = "Avg. Area MAPE for Validation Data", fill = "Model")
        
        # Average bias per model per area
        all_daily_preds %>% 
          group_by(area) %>% summarize(`3 Month MA` = mean(m3_bias), 
                                       `6 Month MA` = mean(m6_bias), 
                                       `ARIMA` = mean(arima_bias),
                                       Prophet = mean(prophet_bias)) %>% ungroup() %>% 
          gather(key = "model", value = "error", -area) %>%
          
          ggplot(aes(x = as.numeric(area), y = error, fill = model)) + 
          geom_bar(position = "dodge", stat = "identity") + 
          scale_x_continuous(breaks = 1:21) +
          labs(x = "Area", y = "Bias", title = "Avg. Area Bias for Validation Data", fill = "Model")
    
# NEXT STEPS --------------------------------------------------------------

  # * Data Questions   
  # + "vict_age": How are multiple victims addressed? Why the large spike at 99 years old?   
  #   * Find a measure of number of vehicles on the road by time of day, gender, area, etc. This provides a measure of per capita collisions and would allow me to say whether a given time of day, day of week, etc was more dangerous than another.      
  # * Try more modeling approaches and parameter tuning.      

    


