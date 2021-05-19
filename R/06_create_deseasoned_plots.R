message(paste0(Sys.time(), ": ", "Starting script 06_create_deseasoned_plots.R")) 

deseasoned_major_types_plot <- deseasoned_all_citywide %>%
  filter(description %in% c("AGG. ASSAULT", "RAPE", "HOMICIDE + SHOOTING", "ROBBERY (ALL)")) %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=deseasoned_90d, color = description)) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = deseasoned_90d),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = deseasoned_90d),
             color = red_orange, size = 3) +
  facet_wrap(ncol = 1, ~description, scales="free_y") +
  theme_iteam_presentations() +
  scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90")) +
  labs(title = "Seasonally Adjusted 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total")

fn <- paste0(output_folder, last_date, "_rolling_90_day_citywide_facet_deseasoned", ".png")

ggsave(filename = fn, deseasoned_major_types_plot, device = "png", 
       width = 2.5, height = 3, units = "in")

actual_major_types_plot <- deseasoned_all_citywide %>%
  filter(description %in% c("AGG. ASSAULT", "RAPE", "HOMICIDE + SHOOTING", "ROBBERY (ALL)")) %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=roll_90, color = description)) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = roll_90),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = roll_90),
             color = red_orange, size = 3) +
  facet_wrap(ncol = 1, ~description, scales="free_y") +
  theme_iteam_presentations() +
  scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90")) +
  labs(title = "Actual 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total")

fn <- paste0(output_folder, last_date, "_rolling_90_day_citywide_facet_actual", ".png")

ggsave(filename = fn, actual_major_types_plot, device = "png", 
       width = 2.5, height = 3, units = "in")


the_deseasoned_book <- deseasoned_all_by_district %>%
  filter(#description == "HOMICIDE + SHOOTING",
    crimedate >= "2019-01-01") %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=deseasoned_90d),  color = iteam.colors[2]) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = deseasoned_90d),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = deseasoned_90d),
             color = red_orange, size = 3) +
  facet_wrap( ~description + district, scales = "free_y") +
  theme_iteam_google_docs() +
  #scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Seasonally Adjusted 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total") +
  scale_x_date(date_breaks = "1 year", date_labels = format("%Y"),
               date_minor_breaks = "1 month") 

fn <- paste0(output_folder, last_date, "_the_deseasoned_book", ".png")

ggsave(filename = fn, the_deseasoned_book, device = "png", 
       width = 20, height = 17, units = "in")


district_hom_deseasoned <- deseasoned_all_by_district %>%
  filter(description == "HOMICIDE + SHOOTING",
         crimedate >= "2019-01-01") %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=deseasoned_90d),  color = iteam.colors[2]) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = deseasoned_90d),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = deseasoned_90d),
             color = red_orange, size = 3) +
  facet_wrap( ~description + district) +
  theme_iteam_presentations() +
  #scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Seasonally Adjusted 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total") +
  scale_x_date(date_breaks = "1 year", date_labels = format("%Y"),
               date_minor_breaks = "1 month") 

fn <- paste0(output_folder, last_date, "_rolling_90_day_by_district_hom+shot_deseasoned", ".png")

ggsave(filename = fn, district_hom_deseasoned, device = "png", 
       width = 6, height = 6, units = "in")


district_aggassault_deseasoned <- deseasoned_all_by_district %>%
  filter(description == "AGG. ASSAULT",
         district != "UNKNOWN",
         crimedate >= "2019-01-01") %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=deseasoned_90d),  color = iteam.colors[2]) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = deseasoned_90d),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = deseasoned_90d),
             color = red_orange, size = 3) +
  facet_wrap( ~description + district) +
  theme_iteam_presentations() +
  #scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Seasonally Adjusted 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total") +
  scale_x_date(date_breaks = "1 year", date_labels = format("%Y"),
               date_minor_breaks = "1 month") 

fn <- paste0(output_folder, last_date, "_rolling_90_day_by_district_aggassault_deseasoned", ".png")

ggsave(
  filename = fn, 
  plot = district_aggassault_deseasoned, 
  device = "png", 
       width = 6, height = 6, units = "in")

district_robbery_deseasoned <- deseasoned_all_by_district %>%
  filter(description == "ROBBERY (ALL)",
         district != "UNKNOWN",
         crimedate >= "2019-01-01") %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=deseasoned_90d),  color = iteam.colors[2]) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = deseasoned_90d),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = deseasoned_90d),
             color = red_orange, size = 3) +
  facet_wrap( ~description + district) +
  theme_iteam_presentations() +
  #scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Seasonally Adjusted 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total") +
  scale_x_date(date_breaks = "1 year", date_labels = format("%Y"),
               date_minor_breaks = "1 month") 

fn <- paste0(output_folder, last_date, "_rolling_90_day_by_district_robbery_deseasoned", ".png")

ggsave(filename = fn, district_robbery_deseasoned, device = "png", 
       width = 6, height = 6, units = "in")



district_facet_actual_and_deseasoned <- function(deseasoned_all_by_district, crime_of_interest){
  
  district_actual_and_deseasoned <- deseasoned_all_by_district %>%
    filter(description == crime_of_interest,
           crimedate >= "2020-01-01") %>%
    # mutate(district_short = case_when(
    #   district == "SOUTHEAST" ~ "SE",
    #   district == "NORTHEAST" ~ "NE",
    #   district == "CENTRAL" ~ "CEN",
    #   district == "SOUTHERN"  ~ "S",
    #   district == "NORTHERN" ~ "N",     
    #   district == "NORTHWEST" ~ "NW",
    #   district == "SOUTHWEST" ~ "SW",
    #   district == "EASTERN" ~ "E",
    #   district == "WESTERN" ~ "W"
    # )) %>%
  mutate(district_short = case_when(
    district == "Southeastern" ~ "SE",
    district == "Northeastern" ~ "NE",
    district == "Central" ~ "CEN",
    district == "Southern"  ~ "S",
    district == "Northern" ~ "N",     
    district == "Northwestern" ~ "NW",
    district == "Southwestern" ~ "SW",
    district == "Eastern" ~ "E",
    district == "Western" ~ "W"
  )) %>%
    transmute(crimedate, 
              district_short, 
              day_of_year,
              "Actual" = roll_90) %>%
    gather(key = type, value = "90d total", -district_short, -crimedate, -day_of_year)
  
  
  district_actual_and_deseasoned_plot <- district_actual_and_deseasoned %>%
    ggplot(aes(crimedate, `90d total`)) +
    geom_line(size=.4) +
    geom_text(data = district_actual_and_deseasoned %>%
                filter(year(crimedate) == 2021) %>%
                filter(day_of_year == max(day_of_year),
                       type == "Actual"),
              aes(x = crimedate - 21, y = 1.25 * `90d total`, label = `90d total`),
              size = 8) +
    facet_wrap(~district_short, ncol=5) +
    scale_x_date(date_labels = "%b %y") +
    scale_y_continuous(n.breaks = 4) +
    labs(y = "90 Day Total") +
    #scale_y_continuous(breaks = 4) +
    theme_iteam_presentations() +
    theme(axis.text.x = element_text(size=30),
          axis.text.y = element_text(size=30, face="bold"),
          text = element_text(size=30),
          legend.position = "none",
          axis.title.x = element_blank(),
          strip.text.y = element_text(face = "bold", size = 30),
          axis.title.y = element_text(size=30, face="bold"),
          #panel.grid.major.x = element_line(color = "gray90")) +
          panel.grid.major.y = element_blank()) +
    
    geom_hline(yintercept = 0, color = "black", size = .5)
}

# robbery - deseasoned - rolling 90day - by district
plt <- district_facet_actual_and_deseasoned(deseasoned_all_by_district, "ROBBERY (ALL)")
fn <- paste0(
  output_folder, 
  last_date, 
  "_ROBBERY_ALL_rolling_90d_by_district_actual_and_deseasoned", 
  ".png"
)
ggsave(
  filename = fn, 
  plot = plt,
  device = "png", 
  width = 9,
  height = 2.5, 
  units = "in"
)

# aggravated assault - deseasoned - rolling 90day - by district
plt <- district_facet_actual_and_deseasoned(deseasoned_all_by_district, "AGG. ASSAULT")
fn <- paste0(
  output_folder, 
  last_date, 
  "_AGG_ASSAULT_rolling_90d_day_by_district_actual_and_deseasoned", 
  ".png"
  )
ggsave(
  filename = fn, 
  plot = plt, 
  device = "png", 
  width = 9, 
  height = 2.5, 
  units = "in"
  )

#  homicide/shootings - deseasoned - rolling 90day - by district
plt <- district_facet_actual_and_deseasoned(deseasoned_all_by_district, "HOMICIDE + SHOOTING")
fn <- paste0(
  output_folder, 
  last_date, 
  "_HOMICIDE+SHOOTING_rolling_90d_by_district_actual_and_deseasoned", 
  ".png"
)
ggsave(
  filename = fn, 
  plot = plt,
  device = "png", 
  width = 9,
  height = 2.5, 
  units = "in"
)
#  carjackings - deseasoned - rolling 90day - by district
plt <- district_facet_actual_and_deseasoned(deseasoned_all_by_district, "ROBBERY - CARJACKING")
fn <- paste0(
  output_folder, 
  last_date, 
  "_ROBBERY - CARJACKING_rolling_90_day_by_district_actual_and_deseasoned", 
  ".png"
)
ggsave(
  filename = fn, 
  plot = plt, 
  device = "png", 
  width = 9, 
  height = 2.5, 
  units = "in"
)

 
# NEW Homicide and shooting actual 
district_hom_deseasoned <- rolling_counts_by_district %>%
  filter(description == "ROBBERY (ALL)",
         crimedate >= "2019-01-01") %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=roll_90),  color = iteam.colors[2]) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = roll_90),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = roll_90),
             color = red_orange, size = 3) +
  facet_wrap( ~description + district) +
  theme_iteam_presentations() +
  #scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Actual Adjusted 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total") +
  scale_x_date(date_breaks = "1 year", date_labels = format("%Y"),
               date_minor_breaks = "1 month") 

fn <- paste0(output_folder, last_date, "_rolling_90_day_by_district_robbery_actual", ".png")

ggsave(filename = fn, district_hom_deseasoned, device = "png", 
       width = 6, height = 6, units = "in")

# NEW Homicide and shooting actual 
district_hom_deseasoned <- rolling_counts_by_district %>%
  filter(description == "AGG. ASSAULT",
         crimedate >= "2019-01-01") %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=roll_90),  color = iteam.colors[2]) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = roll_90),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = roll_90),
             color = red_orange, size = 3) +
  facet_wrap( ~description + district) +
  theme_iteam_presentations() +
  #scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Actual Adjusted 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total") +
  scale_x_date(date_breaks = "1 year", date_labels = format("%Y"),
               date_minor_breaks = "1 month") 

fn <- paste0(output_folder, last_date, "_rolling_90_day_by_district_agg_assualt_actual", ".png")

ggsave(filename = fn, district_hom_deseasoned, device = "png", 
       width = 6, height = 6, units = "in")
# NEW Homicide and shooting actual 
district_hom_deseasoned <- rolling_counts_by_district %>%
  filter(description == "HOMICIDE + SHOOTING",
         crimedate >= "2014-01-01") %>%
  ggplot(aes(crimedate)) +
  geom_rect(aes(xmin=md_stay_at_home_start, xmax=md_stay_at_home_end, ymin=0, ymax=Inf),
            fill = "gray90", alpha = 0.5) +
  geom_line(aes(y=roll_90),  color = iteam.colors[2]) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate) - 365), 
             aes(x = crimedate, y = roll_90),
             color = iteam.colors[1], size = 3) +
  geom_point(data = . %>% 
               filter(crimedate == max(crimedate)), 
             aes(x = crimedate, y = roll_90),
             color = red_orange, size = 3) +
  facet_wrap( ~description + district) +
  theme_iteam_presentations() +
  #scale_color_discrete_iteam() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        panel.grid.major.x = element_line(color = "gray90"),
        panel.grid.minor.x = element_blank()) +
  labs(title = "Actual Adjusted 90-Day Crime Trends") +
  labs(y = "90 Day Rolling Total") +
  scale_x_date(date_breaks = "1 year", date_labels = format("%Y"),
               date_minor_breaks = "1 month") 

fn <- paste0(output_folder, last_date, "_rolling_90_day_by_district_hom+shot_actual", ".png")

ggsave(filename = fn, district_hom_deseasoned, device = "png", 
       width = 12, height = 6, units = "in")