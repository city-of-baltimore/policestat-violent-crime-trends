create_plot_subdirectory <- function(subdirectory_name){
  subdirectory <- paste0(output_folder, subdirectory_name)
  if (!dir.exists(subdirectory)){
    dir.create(subdirectory)
  }
}


roll28_district_facet_plot <- function(violent_crime, desc){
  
  max.y <- violent_crime %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_28, na.rm = T)) %>%
    pull()
  
  first_date <- '2021-01-01'
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
         caption = paste0('Data from "CitiStat.Part1_Crime" table on BCIT SQL server accessed: ', today())) +
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(angle = 90),
          panel.grid.major.x = element_line(size = .5, color="gray90" )) +
    
    ylim(0, 1.5 * max.y)
  
  plt
}

roll90_district_facet_plot <- function(violent_crime, desc){
  
  max.y <- violent_crime %>%
    filter(description == desc,
           crimedate >= '2021-01-01') %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- '2021-01-01'
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
         caption = paste0('Data from "CitiStat.Part1_Crime" table on BCIT SQL server accessed: ', today())) +
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(angle = 90),
          panel.grid.major.x = element_line(size=.5, color="gray90" )) +
    
    ylim(0, 1.5 * max.y)
  
  plt
}

roll90_district_facet_plot_sparkline <- function(violent_crime, desc){
  
  max.y <- violent_crime %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- '2021-01-01'
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
    
    ylim(0, 1.5 * max.y)
  
  plt
}

roll90_single_district_plot <- function(desc, dist){
  
  violent_crime <- rolling_counts_districts %>%
    filter(description == desc,
           district %in% dist)
  
  max.y <- violent_crime %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- '2021-01-01'
  last_date <- max(violent_crime$crimedate)
  
  plt <- violent_crime %>%
    ggplot(aes(crimedate, roll_90)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +

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
    #caption = paste0('Data from "CitiStat.Part1_Crime" table on BCIT SQL server accessed: ', today())) +
    
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(p),
          panel.grid.major.x = element_line(size=.2, color="gray90" )) +
    ylim(0, 1.5 * max.y)
  
  plt
}


roll28_single_district_plot <- function(desc, dist){
  
  violent_crime <- rolling_counts_districts %>%
    filter(description == desc,
           district %in% dist)
  
  max.y <- violent_crime %>%
    summarise(max.y = max(roll_28, na.rm = T)) %>%
    pull()
  
  first_date <- '2021-01-01'
  last_date <- max(violent_crime$crimedate)
  
  plt <- violent_crime %>%
    ggplot(aes(crimedate, roll_28)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +

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
    #caption = paste0('Data from "CitiStat.Part1_Crime" table on BCIT SQL server, accessed: ', today())) +
    theme_iteam_google_docs() +
    scale_x_date(date_breaks = "1 year", 
                 date_labels = "%Y",
                 position = "top") +
    theme(axis.title.x = element_blank(),
          #axis.text.x = element_text(p),
          panel.grid.major.x = element_line(size=.2, color="gray90" )) +
    ylim(0, 1.5 * max.y)
  
  plt
}
