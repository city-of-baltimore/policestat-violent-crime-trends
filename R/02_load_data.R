message(paste0(Sys.time(), ": ", "Starting script 02_load_data.R")) 

# load shapefile for districts
url <- "https://opendata.baltimorecity.gov/egis/rest/services/Hosted/Police_Districts/FeatureServer/0/query?where=1%3D1&outFields=*&outSR=4326&f=json"
#districts <- read_sf(url)

if (data_source == "open baltimore"){
  # NEEDS UPDATE 
  
  #message(paste0(Sys.time(), ": ", "Fetching data from Open Baltimore")) 
  #violent_crime <- get_violent_crime_socrata(start_date = "2015-01-01")

} else if (data_source == "csv"){
  
  message(paste0(Sys.time(), ": ", "Loading csv")) 
  violent_crime <- read_csv(paste0(data_folder, crime_csv_path))

} else {
  
  message(paste0(Sys.time(), ": ", "Fetching data from SQL table")) 
  violent_crime <- get_violent_crime_sql(start_date = "2014-01-01")

}

# clean file from d. vaught
# violent_crime <- violent_crime %>%
#   rename_with(tolower) %>%
#   rename(
#     "crimedate" = "from_date_date",
#     "crimetime" = "from_time",
#     "description" = "name_combine",
#     "longitude" = "xcoordinatecase",
#     "latitude" = "ycoordinatecase"
# 
#   )

violent_crime <- clean_violent_crime_data(violent_crime)

start_date <- min(violent_crime$crimedate, na.rm=T)
last_date <- max(violent_crime$crimedate, na.rm=T)
