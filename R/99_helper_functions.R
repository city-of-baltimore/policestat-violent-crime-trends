


get_violent_crime_sql <- function(start_date, end_date = NA){
  # this function returns a dataframe of part 1 crime from the citistat database on the BCIT SQL server

  
  # con <- dbConnect(
  #   odbc::odbc(),
  #   .connection_string = paste0("Driver={FreeTDS Driver};
  #                                Server=", Sys.getenv("BALT_SQL"), ";
  #                                Database=", Sys.getenv("CITISTAT_DB"), ";
  #                                UID=", Sys.getenv("CITY_USERNAME"),  ";
  #                                PWD=", Sys.getenv("CITY_PWD"), ";
  #                                Integrated Security=NTLM"),
  #   timeout = 10)
  
#  con <- odbc::dbConnect(
 #   odbc::odbc(),
 #   Driver = "SQL Server",
#    Server = Sys.getenv("BALT_SQL"),
#    Database = Sys.getenv("CITISTAT_DB"),
#    uid = Sys.getenv("CITY_USERNAME"),
#    pwd = Sys.getenv("CITY_PWD"),
#    Port = 1433
#  )
   con <-dbConnect(odbc::odbc(),
                  Driver = "SQL Server",
                  Server = "BALT-SQL-FC",
                  Database = "CitiStat",
                  Trusted_Connection = "True",
                  host = "localhost",
                  port = 1433)
  
  message(paste0(Sys.time(), ": Getting crime data from SQL server"))
  
  q1 <- tbl(con, "Part1_Crime") %>%
    filter(`Crime Date` >= start_date)

  q1 <- collect(q1)
  
  message(paste0(Sys.time(), ": Crime data retrieved from SQL server"))
 
  names(q1)<-str_replace_all(names(q1), c(" " = "" ))
  
  q1 <- q1 %>%
    rename_all(funs(tolower)) %>%
    rename(district = "policedistrict")
  
}


get_violent_crime_socrata <- function(start_date, end_date = NA){
  # DEPRECATE
  # this function retrievs a data frame of part 1 crime for baltimore from the Open Baltimore portal
  
  library(tidyverse)
  library(RSocrata)

  # Get data from Open Baltimore
  query <- paste0(
    "https://data.baltimorecity.gov/resource/wsfq-mvij.json?$where=",
    "(Description like 'HOMICIDE' OR ",
    "Description like 'SHOOTING' OR ", 
    "Description like 'RAPE' OR ", 
    "Description like 'AGG. ASSAULT' OR ",
    "contains(Description, 'ROBBERY')) AND ",
    "(crimedate >= '", start_date, "')")
    
  if (!is.na(end_date)) {
    query <- paste0(query, ' AND (crimedate <= "', end_date, '")')
  }
  
  violent_crime <- read.socrata(query)

}

clean_violent_crime_data <- function(df){
  df <- df %>%
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
  df
}

to_incident_based <- function(violent_crime){
  # this function takes victim-level crime data and returns an incident-level dataframe (one record per incident rather than victim) - open baltimore data is victim-based
  violent_crime <- violent_crime %>%
    group_by(crimedate, crimetime, crimecode, location) %>%
    mutate(Victims = n()) %>%
    filter(row_number() == 1) %>%
    ungroup()
}


# Data Wrangling Functions -----------------


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

deseason_rolling_90d_counts <- function(rolling_counts){
  
  deseasoned_historical <- rolling_counts %>%
    filter(
      !is.na(roll_90),
      crimedate >= "2016-01-01",
      crimedate <= "2019-12-31",
      !is.na(district)
    ) %>%
    select(
      description, 
      district, 
      crimedate, 
      roll_90
    ) %>%
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

#DV Counts
DVget_rolling_counts_by_district <- function(violent_crime){
  
  DVcounts_districts <- violent_crime %>%
    filter(domesticviolence == "Y") %>%
    count(description_grouped, description, district, crimedate) %>%
    group_by(description_grouped, description, district) %>%
    complete(crimedate = seq.Date(
      as.Date(min(violent_crime$crimedate)), 
      as.Date(max(violent_crime$crimedate)),
      by="day")) %>%
    replace_na(list(n = 0)) %>%
    ungroup()
  
  DVhom_shot_combo_district_counts <- DVcounts_districts %>% 
    filter(description_grouped == "HOMICIDE + SHOOTING") %>% 
    group_by(district, crimedate) %>% 
    select(description, district, crimedate, n) %>% 
    spread(key = description, value = n) %>% 
    mutate(n = HOMICIDE + SHOOTING, 
           description = "HOMICIDE + SHOOTING") %>% 
    select(-HOMICIDE, -SHOOTING) %>% 
    ungroup()
  
  DVrobbery_combo_district_counts <- DVcounts_districts %>% 
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
  
  DVrolling_counts_districts <- DVcounts_districts %>%
    bind_rows(DVhom_shot_combo_district_counts) %>%
    bind_rows(DVrobbery_combo_district_counts)
  
  DVrolling_counts_districts <- DVrolling_counts_districts %>%
    arrange(crimedate) %>%
    group_by(district, description) %>%
    mutate(roll_28 = roll_sum(x = n, n = 28, align = "right", fill = NA),
           roll_7 = roll_sum(x = n, n = 7, align = "right", fill = NA),
           roll_90 = roll_sum(x = n, n = 90, align = "right", fill = NA)) %>%
    ungroup() %>%
    arrange(description, district, crimedate)
  
  DVrolling_counts_districts
}
