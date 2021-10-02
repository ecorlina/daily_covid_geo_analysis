# load packages ----
library(tidyverse)
library(lubridate)
library(readxl)
library(rvest)
library(tidyxl)
library(unpivotr)
library(hablar)
library(janitor)
library(busdater)
library(khroma)

library(rgdal)
library(sf)
library(raster)
library(tmap)    # for static and interactive maps
library(tmaptools)  # for access to read_osm function for basemaps
library(rJava)
library(OpenStreetMap)
library(rmapshaper)
library(leaflet)

# define functions ----

# Excel-equivalent vlookup function courtesy of a tweet by Jenny Bryan of the RStudio team
vlookup <- function(this, data, key, value) {
   m <- match(this, data[[key]])
   data[[value]][m]
}

lastFriday <- function(date_today) {
   date_today <- ymd(date_today)
   day_today <- wday(date_today, label = TRUE)
   shift <- ifelse(as.numeric(day_today) < 6, 7, 0)
   date_today + days(6 - as.numeric(day_today) - shift)
}

# import shape file, COUNTYWIDE STATISTICAL AREAS (CSA) as used by DPH ----

csa_map_simplified <- read_sf("./data/gis_data/Community Boundaries (CSA).geojson") %>%
   sf::st_make_valid() %>%
   arrange(label) %>%
   mutate(across(c(objectid), as.numeric))

# import DPH dashboard data for today and the last update for the previous 14-day window ----

# so if today is Tuesday, July 27, the current 14-days-ending date will be 2021-07-23
# and the previous, non-overlapping 14-days-ending date will be 2021-07-09
# those dates can be automatically determined with the lastFriday function and lubridate

folders_there <- list.files("../daily_dashboard/data") %>%
   purrr::discard(str_detect(., "readme"))

data_date <- last(folders_there)

latest_window_date <- lastFriday(data_date)
previous_window_date <- latest_window_date - days(14)


readr::read_csv(paste("../daily_dashboard/data",
                      data_date,
                      "LA_County_Covid19_CSA_14day_case_death_table.csv", sep = "/"),
                show_col_types = F)

readr::read_csv(paste("../daily_dashboard/data",
                      previous_window_date,
                      "LA_County_Covid19_CSA_14day_case_death_table.csv", sep = "/"),
                show_col_types = F)

# as I think about it, I remember that I tried to make a map with percent change before
# I should find that code and use at least some of it here, assuming I'm continuing with this
# but I saw that article Roni sent from STAT and am really interested in their acceleration metric

