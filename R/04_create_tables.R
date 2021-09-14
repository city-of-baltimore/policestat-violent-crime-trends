message(paste0(Sys.time(), ": ", "Starting script 04_create_tables.R")) 

if (!dir.exists(paste0(output_folder, "tables/"))){
  dir.create(paste0(output_folder, "tables/"))
}

# table 
thisyear = year(today())
lastyear = year(today()) - 1
max_day_of_year <- cumsums %>%
  filter(crime_year == thisyear) %>%
  summarise(ifelse(max(day_of_year)==366, 365, max(day_of_year))) %>%
  pull()

cumsums %>%
  filter(crime_year %in% c(thisyear, lastyear),
         day_of_year == max_day_of_year) %>%
  select(-crimedate, -n, -day_of_year) %>%
  spread(key = crime_year, value = crime_cumsum) %>%
  mutate(pct_change = scales::percent(round((`2021` - `2020`)/`2020`, 2), accuracy = 2)) %>%
  t() %>%
  as.data.frame() %>%
  write_csv(paste0(output_folder, "tables/", last_date, "_YTD_changes.csv"))

#Disposition
#clearedyear <- Disposition %>%
 # filter(disp_year == thisyear,
  #       dispositionname %in% c('Cleared by Arrest Adult ','Cleared by Arrest Juvenile', 'Cleared by Exception')) %>%
#  summarise(ifelse(max(day_of_year)==366, 365, max(day_of_year))) %>%
 # pull()


#Disposition %>%
 # filter(disp_year == 2021,
  #       dispositionname %in% c("Cleared by Arrest Adult                           ",
   #                             "Cleared by Arrest Juvenile                        ",
    #                            "Cleared by Exception                              "))%>%
  #select(-description, -n, -disp_year) %>%
  #select(-crimedate, -crime_cumsum, -disp_year, -crime_year, -dispositiondate, -day_of_year, -dispositionname) %>%
  #group_by(description) %>%
#  spread(key = description, value = n) %>%
  #mutate(pct_change = scales::percent(round((`2021` - `2020`)/`2020`, 2), accuracy = 2)) %>%
 # t() %>%
  #as.data.frame() %>%
#  write_csv(paste0(output_folder, "tables/", last_date, "_clearedYTD_changes.csv"))

