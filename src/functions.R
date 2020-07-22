md_stay_at_home_start <- as.Date("2020-03-30")
md_stay_at_home_end <- as.Date("2020-05-15")


get_violent_crime_socrata <- function(){
  
  library(tidyverse)
  library(RSocrata)
  
  # Get data from Open Baltimore
  query <- paste0("https://data.baltimorecity.gov/resource/wsfq-mvij.json?$where=",
                  "(Description like 'HOMICIDE' OR ",
                  "Description like 'SHOOTING' OR ", 
                  "Description like 'RAPE' OR ", 
                  "Description like 'AGG. ASSAULT' OR ",
                  "contains(Description, 'ROBBERY'))")
  
  violent_crime <- read.socrata(query)
  
  # Some cleaning
  violent_crime <- violent_crime %>% 
    filter(year(crimedate) >= 2014) %>%
    mutate(
      crimedate = as.Date(crimedate),
      longitude = as.numeric(longitude),
      latitude = as.numeric(latitude),
      #description = ifelse(grepl("ROBBERY", description), "ROBBERY", description),
      description_grouped = 
        case_when(
          grepl("ROBBERY", description) ~ "ROBBERY (ALL)",
          description %in% c("HOMICIDE", "SHOOTING") ~ "HOMICIDE + SHOOTING",
          TRUE ~ description)
    )
}

# Calculate rolling totals --------------

get_daily_counts_by_district <- function(violent_crime){
  
  counts_districts <- violent_crime %>%
    count(description_grouped, district, crimedate) %>%
    group_by(description_grouped, district) %>%
    complete(crimedate = seq.Date(
      as.Date(min(violent_crime$crimedate)), 
      as.Date(max(violent_crime$crimedate)),
      by="day")) %>%
    replace_na(list(n = 0)) %>%
    ungroup()
  
}

get_rolling_counts_by_district <- function(violent_crime){
  
  counts_districts <- violent_crime %>%
    count(description_grouped, description, district, crimedate) %>%
    group_by(description_grouped, description, district) %>%
    complete(crimedate = seq.Date(
      as.Date(min(violent_crime$crimedate)), 
      as.Date(max(violent_crime$crimedate)),
      by="day")) %>%
    replace_na(list(n = 0)) %>%
    ungroup()
  
  hom_shot_combo_district_counts <- counts_districts %>% 
    filter(description_grouped == "HOMICIDE + SHOOTING") %>% 
    group_by(district, crimedate) %>% 
    select(description, district, crimedate, n) %>% 
    spread(key = description, value = n) %>% 
    mutate(n = HOMICIDE + SHOOTING, 
           description = "HOMICIDE + SHOOTING") %>% 
    select(-HOMICIDE, -SHOOTING) %>% 
    ungroup()
  
  robbery_combo_district_counts <- counts_districts %>% 
    filter(description_grouped == "ROBBERY (ALL)") %>% 
    group_by(district, crimedate) %>% 
    select(description, district, crimedate, n) %>% 
    spread(key = description, value = n) %>%
    mutate(
      n = `ROBBERY - RESIDENCE` + `ROBBERY - COMMERCIAL` + `ROBBERY - CARJACKING` + `ROBBERY - STREET`, 
      description = "ROBBERY (ALL)") %>%
    select(
      -`ROBBERY - RESIDENCE`,
      -`ROBBERY - COMMERCIAL`,
      -`ROBBERY - CARJACKING`,
      -`ROBBERY - STREET`
    ) %>% 
    ungroup()
  
  rolling_counts_districts <- counts_districts %>%
    bind_rows(hom_shot_combo_district_counts) %>%
    bind_rows(robbery_combo_district_counts)
  
  rolling_counts_districts <- rolling_counts_districts %>%
    arrange(crimedate) %>%
    group_by(district, description) %>%
    mutate(roll_28 = roll_sum(x = n, n = 28, align = "right", fill = NA),
           roll_7 = roll_sum(x = n, n = 7, align = "right", fill = NA),
           roll_90 = roll_sum(x = n, n = 90, align = "right", fill = NA)) %>%
    ungroup() %>%
    arrange(description, district, crimedate)
  
  rolling_counts_districts
}

get_rolling_counts_citywide <- function(violent_crime){
  
  counts <- violent_crime %>%
    count(description, crimedate) %>%
    group_by(description) %>%
    complete(crimedate = seq.Date(as.Date(min(violent_crime$crimedate)), 
                                  as.Date(max(violent_crime$crimedate)),
                                  by="day")) %>%
    replace_na(list(n = 0)) %>%
    mutate(district = "CITYWIDE")
  
  hom_shot_combo_counts <- counts %>% 
    filter(description %in% c("SHOOTING", "HOMICIDE")) %>% 
    group_by(crimedate) %>% 
    select(description, crimedate, n) %>% 
    spread(key = description, value = n) %>% 
    mutate(n = HOMICIDE + SHOOTING, 
           description = "HOMICIDE + SHOOTING",
           district = "CITYWIDE") %>% 
    select(-HOMICIDE, -SHOOTING) %>% 
    ungroup()
  
  robbery_combo_counts <- counts %>% 
    filter(grepl("ROBBERY", description)) %>%
    group_by(crimedate) %>% 
    select(description, crimedate, n) %>% 
    spread(key = description, value = n) %>%
    mutate(
      n = `ROBBERY - RESIDENCE` + `ROBBERY - COMMERCIAL` + `ROBBERY - CARJACKING` + `ROBBERY - STREET`, 
      description = "ROBBERY (ALL)",
      district = "CITYWIDE") %>%
    select(
      -`ROBBERY - RESIDENCE`,
      -`ROBBERY - COMMERCIAL`,
      -`ROBBERY - CARJACKING`,
      -`ROBBERY - STREET`
    ) %>% 
    ungroup()
  
  rolling_counts <- counts %>%
    bind_rows(hom_shot_combo_counts) %>%
    bind_rows(robbery_combo_counts)
  
  rolling_counts <- rolling_counts %>%
    arrange(crimedate) %>%
    group_by(description) %>%
    mutate(roll_28 = roll_sum(x = n, n = 28, align = "right", fill = NA),
           roll_7 = roll_sum(x = n, n = 7, align = "right", fill = NA),
           roll_90 = roll_sum(x = n, n = 90, align = "right", fill = NA)) %>%
    ungroup() %>%
    arrange(description, crimedate)
  
  rolling_counts
}

# Rolling plots --------------------------

roll28_district_facet_plot <- function(violent_crime, desc){
  
  max.y <- violent_crime %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_28, na.rm = T)) %>%
    pull()
  
  first_date <- min(violent_crime$crimedate)
  last_date <- max(violent_crime$crimedate)
  
  current_state <- violent_crime %>% 
    filter(crimedate == last_date) %>%
    mutate(last_date = last_date)
  
  plt_violent_crime <- violent_crime %>%
    filter(!is.na(district),
           district != "UNKNOWN",
           description == desc)
  
  plt <- ggplot(data = plt_violent_crime, aes(crimedate, roll_28)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +
    
    #ylim(c(0, max())) +
    geom_point(data = subset(plt_violent_crime, crimedate == last_date), 
               aes(x = crimedate, y = roll_28),
               color = "red", size =2) +
    geom_point(data = subset(plt_violent_crime, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_28),
               color = iteam.colors[1], size = 2) +
    geom_point(data = subset(plt_violent_crime, crimedate == last_date - 28), 
               aes(x = crimedate, y = roll_28),
               color = iteam.colors[3], size = 2) +
    labs(title = paste0(desc, ": Rolling 28-day Total\n", first_date," to ", last_date),
         y = "Rolling 28-Day Total",
         caption = paste0("Data from Open Baltimore, accessed: ", today())) +
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(angle = 90),
          panel.grid.major.x = element_line(size = .5, color="gray90" )) +
    
    ylim(0, 1.1 * max.y)
  
  plt
}

roll90_district_facet_plot <- function(violent_crime, desc){
  
  max.y <- violent_crime %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- min(violent_crime$crimedate)
  last_date <- max(violent_crime$crimedate)
  
  plt_violent_crime <- violent_crime %>%
    filter(!is.na(district),
           district != "UNKNOWN",
           description == desc)
  
  plt <- plt_violent_crime %>%
    ggplot(aes(crimedate, roll_90)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +
    
    geom_point(data = subset(plt_violent_crime, crimedate == last_date), 
               aes(x = crimedate, y = roll_90),
               color = "red", size =2) +
    geom_point(data = subset(plt_violent_crime, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[1], size = 2) +
    geom_point(data = subset(plt_violent_crime, crimedate == last_date - 28), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[3], size = 2) +
    #ylim(c(0, max())) +
    labs(title = paste0(desc, ": Rolling 90-day Total\n", first_date, " to ", last_date),
         y = "Rolling 90-Day Total",
         caption = paste0("Data from Open Baltimore, accessed: ", today())) +
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(angle = 90),
          panel.grid.major.x = element_line(size=.5, color="gray90" )) +
    
    ylim(0, 1.1 * max.y)
  
  plt
}

roll90_district_facet_plot_sparkline <- function(violent_crime, desc){
  
  max.y <- violent_crime %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- min(violent_crime$crimedate)
  last_date <- max(violent_crime$crimedate)
  
  plt_violent_crime <- violent_crime %>%
    filter(!is.na(district),
           district != "UNKNOWN",
           description == desc)
  
  plt <- plt_violent_crime %>%
    ggplot(aes(crimedate, roll_90)) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +
    geom_point(data = subset(plt_violent_crime, crimedate == last_date), 
               aes(x = crimedate, y = roll_90),
               color = "red", size =3) +
    geom_point(data = subset(plt_violent_crime, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[1], size = 3) +
    geom_point(data = subset(plt_violent_crime, crimedate == last_date - 28), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[3], size = 3) +
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank()) +
    
    ylim(0, 1.1 * max.y)
  
  plt
}
# roll90_district_facet_plot <- function(desc, district){
#   
#   max.y <- rolling.counts.districts %>%
#     filter(description == desc,) %>%
#     summarise(max.y = max(roll_90, na.rm = T)) %>%
#     pull()
#   
#   plt <- rolling.counts.districts %>%
#     filter(!is.na(district),
#            district != "UNKNOWN",
#            description == desc) %>%
#     ggplot(aes(crimedate, roll_90)) +
#     geom_line() +
#     #ylim(c(0, max())) +
#     facet_wrap(~district) +
#     labs(title = paste0(desc, ": Rolling 90-day Total\n2018-01-01 to ", last_date),
#          y = "Rolling 90-Day Total",
#          caption = paste0("Data from Open Baltimore, accessed: ", today())) +
#     theme(axis.title.x = element_blank(),
#           axis.text.x = element_text(angle = 90)) +
#     theme_iteam_google_docs() +
#     ylim(0, 1.1 * max.y)
#   
#   plt
# }


roll90_single_district_plot <- function(desc, dist){
  
  violent_crime <- rolling_counts_districts %>%
    filter(description == desc,
           district %in% dist)
  
  max.y <- violent_crime %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- min(violent_crime$crimedate)
  last_date <- max(violent_crime$crimedate)
  
  plt <- violent_crime %>%
    ggplot(aes(crimedate, roll_90)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +
    geom_point(data = subset(violent_crime, crimedate == last_date), 
               aes(x = crimedate, y = roll_90),
               color = "red", size =2) +
    geom_point(data = subset(violent_crime, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[1], size = 2) +
    geom_point(data = subset(violent_crime, crimedate == last_date - 28), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[3], size = 2) +
    #ylim(c(0, max())) +
    labs(title = paste0(desc, "\nRolling 90-day Total\n", first_date, " to ", last_date),
         y = "Rolling 90-Day Total") +
    #caption = paste0("Data from Open Baltimore, accessed: ", today())) +
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(p),
          panel.grid.major.x = element_line(size=.2, color="gray90" )) +
    ylim(0, 1.1 * max.y)
  
  plt
}


roll28_single_district_plot <- function(desc, dist){
  
  violent_crime <- rolling_counts_districts %>%
    filter(description == desc,
           district %in% dist)
  
  max.y <- violent_crime %>%
    summarise(max.y = max(roll_28, na.rm = T)) %>%
    pull()
  
  first_date <- min(violent_crime$crimedate)
  last_date <- max(violent_crime$crimedate)
  
  plt <- violent_crime %>%
    ggplot(aes(crimedate, roll_28)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +
    geom_point(data = subset(violent_crime, crimedate == last_date), 
               aes(x = crimedate, y = roll_28),
               color = "red", size =2) +
    geom_point(data = subset(violent_crime, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_28),
               color = iteam.colors[1], size = 2) +
    geom_point(data = subset(violent_crime, crimedate == last_date - 28), 
               aes(x = crimedate, y = roll_28),
               color = iteam.colors[3], size = 2) +
    #ylim(c(0, max())) +
    labs(title = paste0(desc, "\nRolling 28-day Total\n", first_date, " to ", last_date),
         y = "Rolling 28-Day Total") +
    #caption = paste0("Data from Open Baltimore, accessed: ", today())) +
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(p),
          panel.grid.major.x = element_line(size=.2, color="gray90" )) +
    ylim(0, 1.1 * max.y)
  
  plt
}

# Deseason -------------

deseason_rolling_90d_counts <- function(rolling_counts){
  
  deseasoned_historical <- rolling_counts %>%
    filter(
      !is.na(roll_90),
      crimedate >= "2016-01-01",
      crimedate <= "2019-12-31") %>%
    select(description, district, crimedate, roll_90) %>%
    mutate(crimedate.year = year(crimedate)) %>% 
    group_by(description, district, crimedate.year) %>%
    mutate(year_avg = mean(roll_90)) %>%
    ungroup() %>%
    mutate(seasonal_ratio = roll_90 / year_avg,
           day_of_year = yday(crimedate)) %>%
    group_by(description, district, day_of_year) %>%
    mutate(
      seasonal_act_avg = mean(roll_90),
      seasonal_ratio_avg = mean(seasonal_ratio)) %>%
    ungroup() 
  
  deseasoned_w_this_year <- rolling_counts %>%
    filter(year(crimedate) >= 2016) %>%
    mutate(day_of_year = yday(crimedate)) %>% 
    left_join(
      deseasoned_historical %>%
        filter(crimedate.year == 2016) %>%
        select(
          description, 
          district,
          day_of_year,
          seasonal_act_avg,
          seasonal_ratio_avg),
      by = c(
        "description",
        "district",
        "day_of_year"
      )
    ) %>% 
    mutate(deseasoned_90d = roll_90 / seasonal_ratio_avg)
  
  deseasoned_w_this_year
}

