#!/usr/local/bin/Rscript
library(tidyverse)
library(RSocrata)
library(RcppRoll)
library(ggiteam)
library(scales)
library(lubridate)
library(sf)
library(rgdal)
library(leaflet)
library(htmltools)
library(readxl)
library(RcppRoll)
library(odbc)
library(RODBC)
library(DBI)
library(dplyr)

data_source <- "sql" # options: "sql", "csv,", "open baltimore"
red_orange <- "#f05a28"
data_folder <- paste0(
  #Sys.getenv("HOME")
  "data//"
  #/Office of Performance & Innovation/", 
  #"CitiStat/PoliceStat/data/raw/"
  ) 
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

crime_csv_path <- "crime/2021-01-05_pt1_crime_drew_vaught_export.csv"

# stay at home order dates for plots
md_stay_at_home_start <- as.Date("2020-03-30")
md_stay_at_home_end <- as.Date("2020-05-15")

# list of bpd/other facilites that should be excluded in geospatial analysis
facilities <- c(
  "300 E MADISON ST", # central booking
  "1400 E NORTH AVE", # district court
  "1900 ARGONNE DR", # northeastern
  "2200 W Cold Spring Ln", # northern
  "1000 N MOUNT ST", # western
  "5200 REISTERSTOWN RD ", # northwest
  "1600 EDISON HWY", # eastern
  "400 FONTHILL AVE", # southwest
  "0 CHERRY HILL RD", # southern
  "600 E FAYETTE ST", # central/hq
  "5700 EASTERN AVE", # southeast
  "2000 W BALTIMORE ST",  # bon secours
  "4000 DEEPWOOD RD", # loch raven va
  "300 N GAY ST", # juv booking
  "4900 EASTERN AV", # bayview
  "1800 ORLEANS ST", # jhh downtown
  "600 N WOLFE ST", # jhh downtown
  "0 S GREENE ST", # umd medical center
  "3400 N CALVERT ST" # union memorial
)

source("99_helper_functions.R")
source("98_plot_functions.R")
source("02_load_data.R")

output_folder <- paste0(
  "output/",
  last_date, "\\"
)


if (!dir.exists(output_folder)){
  dir.create(output_folder)
}

source("03_data_wrangling.R")
source("04_create_tables.R")
source("05_create_trend_plots.R")
source("06_create_deseasoned_plots.R")

