
roll28_district_facet_plot <- function(df, desc){
  
  max.y <- df %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_28, na.rm = T)) %>%
    pull()
  
  last_date <- max(df$crimedate)
  
  
  plt <- df %>%
    filter(!is.na(district),
           district != "UNKNOWN",
           description == desc) %>%
    ggplot(aes(crimedate, roll_28)) +
    geom_line() +
    #ylim(c(0, max())) +
    facet_wrap(~district) +
    labs(title = paste0(desc, ": Rolling 28-day Total\n2018-01-01 to ", last_date),
         y = "Rolling 28-Day Total",
         caption = paste0("Data from Open Baltimore, accessed: ", today())) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    theme_iteam_google_docs() +
    ylim(0, 1.1 * max.y)
  
  plt
}

roll90_district_facet_plot <- function(df, desc){
  
  max.y <- df %>%
    filter(description == desc) %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  last_date <- max(df$crimedate)
  
  plt <- df %>%
    filter(!is.na(district),
           district != "UNKNOWN",
           description == desc) %>%
    ggplot(aes(crimedate, roll_90)) +
    geom_line() +
    #ylim(c(0, max())) +
    facet_wrap(~district) +
    labs(title = paste0(desc, ": Rolling 90-day Total\n2018-01-01 to ", last_date),
         y = "Rolling 90-Day Total",
         caption = paste0("Data from Open Baltimore, accessed: ", today())) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    theme_iteam_google_docs() +
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
  
  df <- rolling.counts.districts %>%
    filter(description == desc,
           district %in% dist)
  
  max.y <- df %>%
    summarise(max.y = max(roll_90, na.rm = T)) %>%
    pull()
  
  plt <- df %>%
    ggplot(aes(crimedate, roll_90)) +
    geom_line() +
    #ylim(c(0, max())) +
    facet_wrap(~district) +
    labs(title = paste0(desc, ": Rolling 90-day Total\n2018-01-01 to ", max.x),
         y = "Rolling 90-Day Total",
         caption = paste0("Data from Open Baltimore, accessed: ", today())) +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    theme_iteam_google_docs() +
    ylim(0, 1.1 * max.y)
  
  plt
}