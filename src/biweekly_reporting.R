library(tidyverse)
library(RSocrata)
library(RcppRoll)
library(ggiteam)
library(scales)
library(lubridate)
library(rgdal)
library(leaflet)
library(RODBC)
library(htmltools)
library(readxl)

source("src/functions.R")

folder <- paste0("../../../../Google Drive/Office of Performance & Innovation/CitiStat/PoliceStat/Regular Reporting/", last_date, "/")

districts <- readOGR("data/raw/districts", "Police_Districts",
                     verbose = T)

# Get data from Open Baltimore
query <- paste0("https://data.baltimorecity.gov/resource/wsfq-mvij.json?$where=",
                "(Description like 'HOMICIDE' OR ",
                "Description like 'SHOOTING' OR ", 
                "Description like 'RAPE' OR ", 
                "Description like 'AGG. ASSAULT' OR ",
                "contains(Description, 'ROBBERY'))")

df <- read.socrata(query)

# Some cleaning
df <- df %>% 
  filter(!is.na(latitude),
         year(crimedate) >= 2017) %>%
  mutate(crimedate = as.Date(crimedate),
         longitude = as.numeric(longitude),
         latitude = as.numeric(latitude),
         description = ifelse(grepl("ROBBERY", description), 
                              "ROBBERY", description),
         description_grouped = 
           case_when(
             grepl("ROBBERY", description) & grepl("FIREARM", weapon) ~ "ARMED ROBBERY",
             grepl("ROBBERY", description) & !grepl("FIREARM", weapon) ~ "UNARMED ROBBERY",
             description %in% c("HOMICIDE", "SHOOTING") ~ "HOMICIDE/SHOOTING",
             TRUE ~ description)
  )

# Last date in dataset
last_date <- max(df$crimedate)

# convert crime df to geospatial
df_geo <- SpatialPointsDataFrame(
  coords = df %>% select(longitude, latitude), 
  df, 
  proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
)

# transform to right coords system
df_geo <- spTransform(
  df_geo, 
  CRSobj = CRS("+init=epsg:4326 +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84
+towgs84=0,0,0")
)

# Rolling counts

# By district

counts_districts <- df_geo@data %>%
  count(description, district, crimedate) %>%
  group_by(description, district) %>%
  complete(crimedate = seq.Date(as.Date(min(df_geo@data$crimedate)), 
                                as.Date(max(df_geo@data$crimedate)),
                                by="day")) %>%
  replace_na(list(n = 0)) %>%
  ungroup()

hom_shot_combo_district_counts <- counts_districts %>% 
  filter(description %in% c("SHOOTING", "HOMICIDE")) %>% 
  group_by(district, crimedate) %>% 
  select(description, district, crimedate, n) %>% 
  spread(key = description, value = n) %>% 
  mutate(n = HOMICIDE + SHOOTING, 
         description = "HOMICIDE + SHOOTING") %>% 
  select(-HOMICIDE, -SHOOTING) %>% 
  ungroup()

rolling_counts_districts <- counts_districts %>%
  bind_rows(hom_shot_combo_district_counts) %>%
  arrange(crimedate) %>%
  group_by(district, description) %>%
  mutate(roll_28 = roll_sum(x = n, n = 28, align = "right", fill = NA),
         roll_7 = roll_sum(x = n, n = 7, align = "right", fill = NA),
         roll_90 = roll_sum(x = n, n = 90, align = "right", fill = NA)) %>%
  ungroup() %>%
  arrange(description, district, crimedate)

# Citywide

counts <- df_geo@data %>%
  count(description, crimedate) %>%
  group_by(description) %>%
  complete(crimedate = seq.Date(as.Date(min(df_geo@data$crimedate)), 
                                as.Date(max(df_geo@data$crimedate)),
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

rolling_counts <- counts %>%
  bind_rows(hom_shot_combo_counts) %>%
  arrange(crimedate) %>%
  group_by(description) %>%
  mutate(roll_28 = roll_sum(x = n, n = 28, align = "right", fill = NA),
         roll_7 = roll_sum(x = n, n = 7, align = "right", fill = NA),
         roll_90 = roll_sum(x = n, n = 90, align = "right", fill = NA)) %>%
  ungroup() %>%
  arrange(description, crimedate)

if (!dir.exists(folder)){
  dir.create(folder)
}


# City-wide plots save
for(desc in unique(rolling_counts$description)){
  
  fn_roll90 <- paste0(folder, last_date, "_rolling_90_day_citywide_", desc, ".png")
  fn_roll28 <- paste0(folder, last_date, "_rolling_28_day_citywide_", desc, ".png")
  
  plt_roll28 <- roll28_district_facet_plot(rolling_counts, desc)
  plt_roll90 <- roll90_district_facet_plot(rolling_counts, desc)
  
  ggsave(filename = fn_roll28, plt_roll28, device = "png", 
         width = 5, height = 3, units = "in")
  
  ggsave(filename = fn_roll90, plt_roll90, device = "png", 
         width = 5, height = 3, units = "in")
  
  print(plt_roll28)
  print(plt_roll90)
}


# City-wide sparkline plots save
for(desc in unique(rolling_counts$description)){
  
  fn_roll90 <- paste0(folder, last_date, "_rolling_90_day_citywide_sparkline", desc, ".png")
  plt_roll90 <- roll90_district_facet_plot(rolling_counts, desc) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.subtitle = element_blank(),
          plot.caption = element_blank(),
          strip.text = element_blank(),
          axis.text.y = element_blank(),
          title = element_blank(),
          plot.title = element_blank())
  
  ggsave(filename = fn_roll90, plt_roll90, device = "png", 
         width = 2, height = 1, units = "in")

  print(plt_roll90)
}


# District plots save
for(desc in unique(rolling_counts_districts$description)){
  
  fn_roll90 <- paste0(folder, last_date, "_rolling_90_day_by_district_", desc, ".png")
  fn_roll28 <- paste0(folder, last_date, "_rolling_28_day_by_district_", desc, ".png")
  
  plt_roll28 <- roll28_district_facet_plot(rolling_counts_districts, desc)
  plt_roll90 <- roll90_district_facet_plot(rolling_counts_districts, desc)
  
  ggsave(filename = fn_roll28, plt_roll28, device = "png", 
         width = 7, height = 9, units = "in")
  
  ggsave(filename = fn_roll90, plt_roll90, device = "png", 
         width = 7, height = 9, units = "in")
  
  
  print(plt_roll28)
  print(plt_roll90)
}

# Cumulative homicides/shooting plots
hom_cumsums <- df_geo@data %>%
  filter(description %in% c("HOMICIDE")) %>%
  count(crimedate) %>%
  complete(crimedate = seq.Date(as.Date(min(df_geo@data$crimedate)), 
                                as.Date(max(df_geo@data$crimedate)),
                                by="day")) %>%
  replace_na(list(n = 0)) %>%
  arrange(crimedate) %>%
  mutate(day_of_year = as.numeric(strftime(crimedate, "%j")),
         crime.year = year(crimedate)) %>%
  group_by(crime.year) %>%
  mutate(hom_cumsum = cumsum(n))

hom_current <- hom_cumsums %>%
  filter(crime.year == 2019) %>%
  summarise(max(hom_cumsum)) %>%
  pull()

current_day <- max()

hom_projections <- round(365 * hom_current / current_day, 0)

cum.plot <- hom_cumsums %>%
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
  scale_color_discrete_iteam() +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0, 100, 200, 300, 365),
                     limits = c(0, 450)) +
  ylab("Cumulative Homicides") +
  xlab("Day of Year") 


ggsave(filename = paste0(folder, last_date, "_cumulative_homshot.png"), cum.plot, device = "png", 
       width = 4, height = 4, units = "in")

# Decrease the increase

dec_the_inc <- df_geo@data %>%
  filter(description_grouped == "HOMICIDE/SHOOTING") %>%
  count(crimedate) %>%
  complete(crimedate = seq.Date(as.Date(min(df_geo@data$crimedate)), 
                                as.Date(max(df_geo@data$crimedate)),
                                by="day")) %>%
  replace_na(list(n = 0)) %>%
  arrange(crimedate) %>%
  mutate(day_of_year = as.numeric(strftime(crimedate, "%j")),
         crime.year = year(crimedate)) %>%
  filter(crime.year %in% c(2018, 2019)) %>%
  group_by(crime.year) %>%
  mutate(hs_cumsum = cumsum(n)) %>%
  select(-n, -crimedate) %>%
  spread(key = crime.year, value = hs_cumsum) %>%
  mutate(pct_change = (`2019` - `2018`) / `2018`)

dec_the_inc_plot <- dec_the_inc %>%
  ggplot(aes(day_of_year, pct_change)) +
  geom_line() +
  geom_point(data = subset(dec_the_inc, day_of_year == yday(last_date)), 
             aes(x = day_of_year, y = pct_change),
             color = "red", size =2) +
  geom_point(data = subset(dec_the_inc, day_of_year == yday(last_date) - 28), 
             aes(x = day_of_year, y = pct_change),
             color = "red", size =2) +
  geom_point(data = subset(dec_the_inc, day_of_year == yday("2019-07-01")), 
             aes(x = day_of_year, y = pct_change),
             color = "red", size =2) +
  theme_iteam_presentations() +
  scale_color_discrete_iteam() +
  theme(legend.title = element_blank()) +
  ylab("% Change from 2018") +
  xlab("Day of Year")  +
  scale_y_continuous(limits = c(-.5, .5), labels = scales::percent) 

ggsave(filename = paste0(folder, last_date, "_decrease_the_increase.png"), dec_the_inc_plot, device = "png", 
       width = 7, height = 4, units = "in")




# Cumulative faceted --------

start_date <- min(df_geo@data$crimedate)
end_date <- max(df_geo@data$crimedate)

# calc cumsums
cumsums <- df_geo@data %>%
  #filter(description %in% c("HOMICIDE")) %>%
  count(description, crimedate) %>%
  group_by(description) %>%
  complete(crimedate = seq.Date(start_date, 
                                end_date,
                                by="day")) %>%
  replace_na(list(n = 0)) %>%
  arrange(description, crimedate) %>%
  mutate(day_of_year = as.numeric(strftime(crimedate, "%j")),
         crime_year = year(crimedate)) %>%
  group_by(description, crime_year) %>%
  mutate(crime_cumsum = cumsum(n)) %>%
  ungroup()

this_year_color <- iteam.colors[1]

# plot faceted by crime type
cum_plot <- ggplot(cumsums) +
  geom_point(data = cumsums %>%
               filter(crime_year == 2020) %>%
               group_by(description) %>%
               summarise(last_day = max(day_of_year), 
                         last_cumsum = max(crime_cumsum)),
             aes(x = last_day, y = last_cumsum),
             color = this_year_color) +
  geom_line(data = cumsums,
            aes(day_of_year, crime_cumsum, 
                group = crime_year, 
                color = as.factor(crime_year))) +
  facet_wrap(~description, nrow = 1, scales = "free_y") +
  theme_iteam_presentations() +
  scale_color_manual(values = c("gray80", "gray50", "gray20", this_year_color)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = c(0, 180, 365),
                     limits = c(0, 450)) +
  ylab("Cumulative Events") +
  xlab("Day of Year") 

ggsave(filename = paste0(folder, last_date, "_cumulative_facet.png"), 
       cum_plot, device = "png", 
       width = 14, height = 3, units = "in")

# table 
max_day_of_year <- cumsums %>%
  filter(crime_year == 2020) %>%
  summarise(max(day_of_year)) %>%
  pull()

cumsums %>%
  filter(crime_year %in% c(2019, 2020),
         day_of_year == max_day_of_year ) %>%
  select(-crimedate, -n, -day_of_year) %>%
  spread(key = crime_year, value = crime_cumsum) %>%
  mutate(pct_change = scales::percent(round((`2020` - `2019`)/`2019`, 2), accuracy = 2)) %>%
  t() %>%
  as.data.frame() %>%
  write_csv(paste0(folder, last_date, "_YTD_changes.csv"))
