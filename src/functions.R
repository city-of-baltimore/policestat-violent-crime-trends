md_stay_at_home_start <- as.Date("2020-03-30")
md_stay_at_home_end <- as.Date("2020-05-15")


roll28_district_facet_plot <- function(df, desc){
  
  max.y <- df %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_28, na.rm = T)) %>%
    pull()
  
  first_date <- min(df$crimedate)
  last_date <- max(df$crimedate)
  
  current_state <- df %>% 
    filter(crimedate == last_date) %>%
    mutate(last_date = last_date)
  
  plt_df <- df %>%
    filter(!is.na(district),
           district != "UNKNOWN",
           description == desc)
  
  plt <- ggplot(data = plt_df, aes(crimedate, roll_28)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +

    #ylim(c(0, max())) +
    geom_point(data = subset(plt_df, crimedate == last_date), 
               aes(x = crimedate, y = roll_28),
               color = "red", size =2) +
    geom_point(data = subset(plt_df, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_28),
               color = iteam.colors[1], size = 2) +
    geom_point(data = subset(plt_df, crimedate == last_date - 28), 
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

roll90_district_facet_plot <- function(df, desc){
  
  max.y <- df %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- min(df$crimedate)
  last_date <- max(df$crimedate)
  
  plt_df <- df %>%
    filter(!is.na(district),
           district != "UNKNOWN",
           description == desc)
    
  plt <- plt_df %>%
    ggplot(aes(crimedate, roll_90)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +

    geom_point(data = subset(plt_df, crimedate == last_date), 
               aes(x = crimedate, y = roll_90),
               color = "red", size =2) +
    geom_point(data = subset(plt_df, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[1], size = 2) +
    geom_point(data = subset(plt_df, crimedate == last_date - 28), 
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

roll90_district_facet_plot_sparkline <- function(df, desc){
  
  max.y <- df %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- min(df$crimedate)
  last_date <- max(df$crimedate)
  
  plt_df <- df %>%
    filter(!is.na(district),
           district != "UNKNOWN",
           description == desc)
  
  plt <- plt_df %>%
    ggplot(aes(crimedate, roll_90)) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +
    geom_point(data = subset(plt_df, crimedate == last_date), 
               aes(x = crimedate, y = roll_90),
               color = "red", size =3) +
    geom_point(data = subset(plt_df, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[1], size = 3) +
    geom_point(data = subset(plt_df, crimedate == last_date - 28), 
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
  
  df <- rolling_counts_districts %>%
    filter(description == desc,
           district %in% dist)
  
  max.y <- df %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  first_date <- min(df$crimedate)
  last_date <- max(df$crimedate)
  
  plt <- df %>%
    ggplot(aes(crimedate, roll_90)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=max(df$crimedate), ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +
    geom_point(data = subset(df, crimedate == last_date), 
               aes(x = crimedate, y = roll_90),
               color = "red", size =2) +
    geom_point(data = subset(df, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_90),
               color = iteam.colors[1], size = 2) +
    geom_point(data = subset(df, crimedate == last_date - 28), 
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
  
  df <- rolling_counts_districts %>%
    filter(description == desc,
           district %in% dist)
  
  max.y <- df %>%
    summarise(max.y = max(roll_28, na.rm = T)) %>%
    pull()
  
  first_date <- min(df$crimedate)
  last_date <- max(df$crimedate)
  
  plt <- df %>%
    ggplot(aes(crimedate, roll_28)) +
    facet_wrap(~district) +
    geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
              fill = "gray90", alpha = 0.5) +
    geom_line(size = 0.75) +
    geom_vline(aes(xintercept = as.Date("2019-07-01")), 
               color = "red", linetype = "dotted", size = .5) +
    geom_point(data = subset(df, crimedate == last_date), 
               aes(x = crimedate, y = roll_28),
               color = "red", size =2) +
    geom_point(data = subset(df, crimedate == last_date - 365), 
               aes(x = crimedate, y = roll_28),
               color = iteam.colors[1], size = 2) +
    geom_point(data = subset(df, crimedate == last_date - 28), 
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
