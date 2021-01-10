message(paste0(Sys.time(), ": ", "Starting script 03_data_wrangling.R")) 

rolling_counts_by_district <- get_rolling_counts_by_district(violent_crime)
rolling_counts_citywide <- get_rolling_counts_citywide(violent_crime)

# calc cumsums
cumsums <- violent_crime %>%
  #filter(description %in% c("HOMICIDE")) %>%
  count(description, crimedate) %>%
  group_by(description) %>%
  complete(crimedate = seq.Date(start_date, 
                                last_date,
                                by="day")) %>%
  replace_na(list(n = 0)) %>%
  arrange(description, crimedate) %>%
  mutate(day_of_year = as.numeric(strftime(crimedate, "%j")),
         crime_year = year(crimedate)) %>%
  group_by(description, crime_year) %>%
  mutate(crime_cumsum = cumsum(n)) %>%
  ungroup() 

group_cumsums <- violent_crime %>%
  filter(description_grouped %in% c("ROBBERY (ALL)", "HOMICIDE + SHOOTING")) %>%
  count(description_grouped, crimedate) %>%
  group_by(description_grouped) %>%
  complete(crimedate = seq.Date(start_date, 
                                last_date,
                                by="day")) %>%
  replace_na(list(n = 0)) %>%
  arrange(description_grouped, crimedate) %>%
  mutate(day_of_year = as.numeric(strftime(crimedate, "%j")),
         crime_year = year(crimedate)) %>%
  group_by(description_grouped, crime_year) %>%
  mutate(crime_cumsum = cumsum(n)) %>%
  ungroup()
  
cumsums <- bind_rows(cumsums, group_cumsums %>% rename("description" = description_grouped))

# Cumulative homicides/shooting
hom_cumsums <- violent_crime %>%
  filter(description %in% c("HOMICIDE")) %>%
  count(crimedate) %>%
  complete(crimedate = seq.Date(as.Date(min(violent_crime$crimedate)), 
                                as.Date(max(violent_crime$crimedate)),
                                by="day")) %>%
  replace_na(list(n = 0)) %>%
  arrange(crimedate) %>%
  mutate(day_of_year = as.numeric(strftime(crimedate, "%j")),
         crime.year = year(crimedate)) %>%
  group_by(crime.year) %>%
  mutate(hom_cumsum = cumsum(n)) %>%
  ungroup()

hom_current <- hom_cumsums %>%
  filter(crime.year == 2020) %>%
  summarise(max(hom_cumsum)) %>%
  pull()

current_day <- yday(last_date)

hom_projections <- round(365 * hom_current / current_day, 0)


# Deseason --------------------------

# dataframes for deseason analysis
deseasoned_homicides_shootings_historical <- rolling_counts_citywide %>%
  filter(description == "HOMICIDE + SHOOTING", 
         !is.na(roll_90),
         crimedate >= "2016-01-01",
         crimedate <= "2019-12-31") %>%
  select(crimedate, roll_90) %>%
  mutate(crimedate.year = year(crimedate)) %>% 
  group_by(crimedate.year) %>%
  mutate(year_avg = mean(roll_90)) %>%
  ungroup() %>%
  mutate(seasonal_ratio = roll_90 / year_avg,
         day_of_year = yday(crimedate)) %>%
  group_by(day_of_year) %>%
  mutate(
    seasonal_act_avg = mean(roll_90),
    seasonal_ratio_avg = mean(seasonal_ratio)) %>%
  ungroup() 


deseasoned_homicides_shootings_w_this_year <- rolling_counts_citywide %>%
  filter(description == "HOMICIDE + SHOOTING",
         year(crimedate) >= 2016) %>%
  mutate(day_of_year = yday(crimedate)) %>% 
  left_join(
    deseasoned_homicides_shootings_historical %>%
      filter(crimedate.year == 2016) %>%
      select(day_of_year,
             seasonal_ratio,
             seasonal_act_avg,
             seasonal_ratio_avg),
    by = "day_of_year"
  ) %>% 
  mutate(deseasoned_90d = roll_90 / seasonal_ratio_avg)

deseasoned_all_citywide <- deseason_rolling_90d_counts(rolling_counts_citywide)
deseasoned_all_by_district <- deseason_rolling_90d_counts(rolling_counts_by_district)