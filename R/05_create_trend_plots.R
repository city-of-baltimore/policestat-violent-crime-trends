message(paste0(Sys.time(), ": ", "Starting script 05_create_trend_plots.R")) 
# City-wide 90-day rolling plots 
create_plot_subdirectory("citywide_90d")
create_plot_subdirectory("citywide_28d")

subdirectory90 <- paste0(output_folder, "citywide_90d/")
subdirectory28 <- paste0(output_folder, "citywide_28d/")

for(desc in unique(rolling_counts_citywide$description)){
  
  fn_roll90 <- paste0(
    subdirectory90, 
    last_date, 
    "_rolling_90_day_citywide_", 
    desc, 
    ".png"
    )

  fn_roll28 <- paste0(
    subdirectory28, 
    last_date, 
    "_rolling_28_day_citywide_", 
    desc, 
    ".png"
  )
  
  plt_roll90 <- roll90_district_facet_plot(rolling_counts_citywide, desc)
  plt_roll28 <- roll28_district_facet_plot(rolling_counts_citywide, desc)

  ggsave(
    filename = fn_roll90, 
    plt_roll90, 
    device = "png", 
    width = 5, 
    height = 3, 
    units = "in"
    )
  
  ggsave(
    filename = fn_roll28, 
    plt_roll28,
    device = "png", 
    width = 5, 
    height = 3, 
    units = "in"
  )
  
}

# City-wide sparkline plots save
create_plot_subdirectory("citywide_90d_sparklines")
subdirectory90 <- paste0(output_folder, "citywide_90d_sparklines/")

for(desc in unique(rolling_counts_citywide$description)){
  
  filename <- paste0(
    subdirectory90, 
    last_date, 
    "_rolling_90_day_citywide_sparkline_", 
    desc, 
    ".png"
    )
  
  plt_roll90 <- roll90_district_facet_plot(rolling_counts_citywide, desc) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.subtitle = element_blank(),
      plot.caption = element_blank(),
      strip.text = element_blank(),
      axis.text.y = element_blank(),
      title = element_blank(),
      plot.title = element_blank())
  
  ggsave(
    filename = filename, 
    plot = plt_roll90, 
    device = "png", 
    width = 2, 
    height = 1,
    units = "in"
    )
  
  print(plt_roll90)
}

# District plots save
create_plot_subdirectory("by_district_90d")
create_plot_subdirectory("by_district_28d")
subdirectory90 <- paste0(output_folder, "by_district_90d/")
subdirectory28 <- paste0(output_folder, "by_district_28d/")

for(desc in unique(rolling_counts_by_district$description)){
  
  fn_roll90 <- paste0(
    subdirectory90, 
    last_date, 
    "_rolling_90_day_by_district_", 
    desc, 
    ".png")
  
  fn_roll28 <- paste0(
    subdirectory28, 
    last_date, 
    "_rolling_28_day_by_district_",
    desc, 
    ".png"
  )
  
  plt_roll28 <- roll28_district_facet_plot(rolling_counts_by_district, desc)
  plt_roll90 <- roll90_district_facet_plot(rolling_counts_by_district, desc)
  
  ggsave(
    filename = fn_roll28, 
    plot = plt_roll28, 
    device = "png", 
    width = 7, 
    height = 9, 
    units = "in"
    )
  
  ggsave(
    filename = fn_roll90, 
    plot = plt_roll90, 
    device = "png", 
    width = 7, 
    height = 9, 
    units = "in"
  )

}

cum.plot <- hom_cumsums %>%
  ungroup() %>%
  ggplot(aes(day_of_year, hom_cumsum, 
             group = crime.year, 
             color = as.factor(crime.year))) +
  geom_line() +
  # geom_rect(aes(xmin=stay_at_home_start, 
  #               xmax=max(hom_cumsums$crimedate), 
  #               ymin=0, ymax=Inf),
  #           fill = "gray90", alpha = 0.45) +
  geom_point(aes(x = hom_cumsums %>% 
                   filter(crime.year == 2019) %>%
                   summarise(max(day_of_year)) %>%
                   pull(), 
                 y = hom_cumsums %>% 
                   filter(crime.year == 2019) %>%
                   summarise(max(hom_cumsum)) %>%
                   pull()),
             color = iteam.colors[3]) +
  geom_point(aes(x = 365, 
                 y = hom_cumsums %>% 
                   filter(crime.year == 2018) %>%
                   summarise(max(hom_cumsum)) %>%
                   pull()),
             color = iteam.colors[2]) +
  geom_point(aes(x = 365, 
                 y = hom_cumsums %>% 
                   filter(crime.year == 2017) %>%
                   summarise(max(hom_cumsum)) %>%
                   pull()),
             color = iteam.colors[1]) +
  geom_point(aes(x = 365, y = hom_projections), color = iteam.colors[3]) +
  geom_text(aes(x = hom_cumsums %>% 
                  filter(crime.year == 2019) %>%
                  summarise(max(day_of_year)) %>%
                  pull(), 
                y = hom_cumsums %>% 
                  filter(crime.year == 2019) %>%
                  summarise(max(hom_cumsum)) %>%
                  pull() +10,
                label = hom_cumsums %>% 
                  filter(crime.year == 2019) %>%
                  summarise(max(hom_cumsum)) %>%
                  pull(),
                hjust = 1),
            color = iteam.colors[3]) +
  geom_text(aes(x = 375, 
                y = hom_cumsums %>% 
                  filter(crime.year == 2018) %>%
                  summarise(max(hom_cumsum)) %>%
                  pull(),
                label = hom_cumsums %>% 
                  filter(crime.year == 2018) %>%
                  summarise(max(hom_cumsum)) %>%
                  pull(),
                hjust = 0),
            color = iteam.colors[2]) +
  geom_text(aes(x = 375, 
                y = hom_cumsums %>% 
                  filter(crime.year == 2017) %>%
                  summarise(max(hom_cumsum)) %>%
                  pull() - 10,
                label = hom_cumsums %>% 
                  filter(crime.year == 2017) %>%
                  summarise(max(hom_cumsum)) %>%
                  pull(),
                hjust = 0),
            color = iteam.colors[1]) +
  geom_text(aes(x = 375, y = hom_projections + 10,
                label = paste(hom_projections, "(proj.)"),
                hjust = 0), 
            color = iteam.colors[3]) +
  #scale_alpha_manual(values = as.factor(c(1, 1, 0.3, 1., 1))) +
  theme_iteam_presentations() +
  #scale_color_discrete_iteam() +
  scale_color_viridis_d() +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 365),
                     limits = c(0, 450)) +
  ylab("Cumulative Homicides") +
  xlab("Day of Year") 

ggsave(
  filename = paste0(output_folder, last_date, "_cumulative_homshot.png"), 
  plot = cum.plot, 
  device = "png", 
  width = 4, 
  height = 4, 
  units = "in"
)

# Decrease the increase

# dec_the_inc <- violent_crime %>%
#   filter(description_grouped == "HOMICIDE/SHOOTING") %>%
#   count(crimedate) %>%
#   complete(crimedate = seq.Date(as.Date(min(violent_crime$crimedate)), 
#                                 as.Date(max(violent_crime$crimedate)),
#                                 by="day")) %>%
#   replace_na(list(n = 0)) %>%
#   arrange(crimedate) %>%
#   mutate(day_of_year = as.numeric(strftime(crimedate, "%j")),
#          crime.year = year(crimedate)) %>%
#   filter(crime.year %in% c(2018, 2019)) %>%
#   group_by(crime.year) %>%
#   mutate(hs_cumsum = cumsum(n)) %>%
#   select(-n, -crimedate) %>%
#   spread(key = crime.year, value = hs_cumsum) %>%
#   mutate(pct_change = (`2019` - `2018`) / `2018`)
# 
# dec_the_inc_plot <- dec_the_inc %>%
#   ggplot(aes(day_of_year, pct_change)) +
#   geom_line() +
#   geom_point(data = subset(dec_the_inc, day_of_year == yday(last_date)), 
#              aes(x = day_of_year, y = pct_change),
#              color = "red", size =2) +
#   geom_point(data = subset(dec_the_inc, day_of_year == yday(last_date) - 28), 
#              aes(x = day_of_year, y = pct_change),
#              color = "red", size =2) +
#   geom_point(data = subset(dec_the_inc, day_of_year == yday("2019-07-01")), 
#              aes(x = day_of_year, y = pct_change),
#              color = "red", size =2) +
#   theme_iteam_presentations() +
#   scale_color_discrete_iteam() +
#   theme(legend.title = element_blank()) +
#   ylab("% Change from 2018") +
#   xlab("Day of Year")  +
#   scale_y_continuous(limits = c(-.5, .5), labels = scales::percent) 
# 
# ggsave(filename = paste0(output_folder, last_date, "_decrease_the_increase.png"), dec_the_inc_plot, device = "png", 
#        width = 7, height = 4, units = "in")


# Cumulative faceted --------
this_year_color <- red_orange

# plot faceted by crime type
cum_plot <- cumsums %>%
  filter(crime_year >= 2014,
         description %in% c("ROBBERY (ALL)", "AGG. ASSAULT", "RAPE", "HOMICIDE", "SHOOTING")) %>%
  ggplot() +
    geom_line(aes(day_of_year, crime_cumsum, 
                group = crime_year, 
                color = as.factor(crime_year),
                size = as.factor(crime_year))) +
        geom_point(
      data = cumsums %>%
        filter(crime_year == 2021,
               description %in% c("ROBBERY (ALL)", "AGG. ASSAULT", "RAPE", "HOMICIDE", "SHOOTING")) %>%
        group_by(description) %>%
        summarise(last_day = max(day_of_year), 
                  last_cumsum = max(crime_cumsum)),
      aes(x = last_day, y = last_cumsum),
      color = this_year_color
    ) +
    scale_color_manual(values = c(iteam.colors[3], iteam.colors[2], iteam.colors[4], iteam.colors[5], iteam.colors[6], "gray20", iteam.colors[1], this_year_color)) +
  facet_wrap(~description, nrow = 1, scales = "free_y") +
  theme_iteam_presentations() +
  #scale_color_manual(values = c(iteam.colors[3], "gray80", "gray80", "gray80", "gray50", "gray20", iteam.colors[1], this_year_color)) +
    #Edit March 11 c(iteam.colors[3], "gray80", "gray80", "gray80", "gray50", "gray20", iteam.colors[1], this_year_color)) +
  #+
  scale_color_manual(values = c(iteam.colors[3], iteam.colors[2], iteam.colors[4], iteam.colors[5], iteam.colors[6], "gray20", iteam.colors[1], this_year_color)) +
  scale_size_manual(values = c(.75, .5, .5, .5, .5, .5, .75, 1)) +
  #scale_color_manual(values = c("gray80", "gray50", "gray20", this_year_color)) +
  scale_color_viridis_d() +
  #scale_size_manual(values = c(.5, .5, .5, 1.3)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(
    breaks = c(0, 180, 365),
    limits = c(0, 450)
  ) +
  ylab("Cumulative Events") +
  xlab("Day of Year") 

ggsave(
  filename = paste0(output_folder, last_date, "_cumulative_facet.png"), 
  plot = cum_plot,
  device = "png", 
  width = 7,
  height = 2, 
  units = "in"
)

# homicides and shootings against goal
cum_goal_hom_shot_plot <- cumsums %>%
  filter(crime_year == 2021,
         description == "HOMICIDE + SHOOTING") %>%
  ggplot() +
  geom_point(
    data = cumsums %>%
      filter(crime_year == 2021,
             description == "HOMICIDE + SHOOTING") %>%
      group_by(description) %>%
      summarise(last_day = max(day_of_year), 
                last_cumsum = max(crime_cumsum)),
    aes(x = last_day, y = last_cumsum, label = `last_cumsum`),
    color = this_year_color
    ) +
  geom_smooth(method = lm, se = FALSE, aes(day_of_year, crime_cumsum), fullrange=TRUE, size = .4) +
  geom_line(aes(day_of_year, crime_cumsum, 
                group = crime_year, 
                color = as.factor(crime_year),
                size = as.factor(crime_year))) +
  geom_line(data = data.frame(x = c(0, 365), y = c(0, 901)), aes(x = x, y = y)) +
  geom_point(aes(x = 365, y = 901)) +
  theme_iteam_presentations() +
  scale_color_manual(values = c(red_orange)) +
  scale_size_manual(values = c(1.1)) +
  theme(
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(
    breaks = c(0, 180, 365),
    limits = c(0, 400)
  ) +
  labs(
    #title = "Cumulative Homicides + Shootings",
    y = "Homicides + Shootings",
    x = "Day of Year"
  ) 

ggsave(
  filename = paste0(output_folder, last_date, "_hom_shot+goal.png"), 
  plot = cum_goal_hom_shot_plot, 
  device = "png", 
  width = 2, 
  height = 2, 
  units = "in"
)

# NEW Robbery Section
cum_goal_hom_shot_plot <- cumsums %>%
  filter(crime_year == 2021,
         description == "ROBBERY (ALL)") %>%
  ggplot() +
  geom_point(
    data = cumsums %>%
      filter(crime_year == 2021,
             description == "ROBBERY (ALL)") %>%
      group_by(description) %>%
      summarise(last_day = max(day_of_year), 
                last_cumsum = max(crime_cumsum)),
    aes(x = last_day, y = last_cumsum, label = `last_cumsum`),
    color = this_year_color
    ) +
  geom_smooth(method = lm, se = FALSE, aes(day_of_year, crime_cumsum), fullrange=TRUE, size = .4) +
  geom_line(aes(day_of_year, crime_cumsum, 
                group = crime_year, 
                color = as.factor(crime_year),
                size = as.factor(crime_year))) +
  geom_line(data = data.frame(x = c(0, 365), y = c(0, 2995)), aes(x = x, y = y)) +
  geom_point(aes(x = 365, y = 2995)) +
  theme_iteam_presentations() +
  scale_color_manual(values = c(red_orange)) +
  scale_size_manual(values = c(1.1)) +
  theme(
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(
    breaks = c(0, 180, 365),
    limits = c(0, 400)
  ) +
  labs(
    #title = "Cumulative Homicides + Shootings",
    y = "Robberies (ALL)",
    x = "Day of Year"
  ) 

ggsave(
  filename = paste0(output_folder, last_date, "_robberies+goal.png"), 
  plot = cum_goal_hom_shot_plot, 
  device = "png", 
  width = 2, 
  height = 2, 
  units = "in"
)

# NEW homicides against goal
cum_goal_hom_shot_plot <- cumsums %>%
  filter(crime_year == 2021,
         description == "HOMICIDE") %>%
  ggplot() +
  geom_point(
    data = cumsums %>%
      filter(crime_year == 2021,
             description == "HOMICIDE") %>%
      group_by(description) %>%
      summarise(last_day = max(day_of_year), 
                last_cumsum = max(crime_cumsum)),
    aes(x = last_day, y = last_cumsum, label = `last_cumsum`),
    color = this_year_color
    ) +
  geom_smooth(method = lm, se = FALSE, aes(day_of_year, crime_cumsum), fullrange=TRUE, size = .4) +
  geom_line(aes(day_of_year, crime_cumsum, 
                group = crime_year, 
                color = as.factor(crime_year),
                size = as.factor(crime_year))) +
  geom_line(data = data.frame(x = c(0, 365), y = c(0, 285)), aes(x = x, y = y)) +
  geom_point(aes(x = 365, y = 285)) +
  theme_iteam_presentations() +
  scale_color_manual(values = c(red_orange)) +
  scale_size_manual(values = c(1.1)) +
  theme(
    legend.title = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(
    breaks = c(0, 180, 365),
    limits = c(0, 400)
  ) +
  labs(
    #title = "Cumulative Homicides + Shootings",
    y = "Homicides",
    x = "Day of Year"
  ) 

ggsave(
  filename = paste0(output_folder, last_date, "_homicides+goal.png"), 
  plot = cum_goal_hom_shot_plot, 
  device = "png", 
  width = 2, 
  height = 2, 
  units = "in"
)


message(paste0(Sys.time(), ": ", "05_create_trend_plots.R complete"))  
