message(paste0(Sys.time(), ": ", "Starting script 04_create_tables.R")) 

if (!dir.exists(paste0(output_folder, "tables/"))){
  dir.create(paste0(output_folder, "tables/"))
}

# table 
max_day_of_year <- cumsums %>%
  filter(crime_year == 2020) %>%
  summarise(ifelse(max(day_of_year)==366, 365, max(day_of_year))) %>%
  pull()

cumsums %>%
  filter(crime_year %in% c(2020, 2019),
         day_of_year == max_day_of_year) %>%
  select(-crimedate, -n, -day_of_year) %>%
  spread(key = crime_year, value = crime_cumsum) %>%
  mutate(pct_change = scales::percent(round((`2020` - `2019`)/`2019`, 2), accuracy = 2)) %>%
  t() %>%
  as.data.frame() %>%
  write_csv(paste0(output_folder, "tables/", last_date, "_YTD_changes.csv"))