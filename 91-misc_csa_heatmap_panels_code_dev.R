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


# define functions ----

# Excel-equivalent vlookup function courtesy of a tweet by Jenny Bryan of the RStudio team
vlookup <- function(this, data, key, value) {
   m <- match(this, data[[key]])
   data[[value]][m]
}


los_angeles_population_estimate <- 10257557


# THINX: I can't easily do what I want to do from the dph_count_data table that I've been constructing
# day after day because that table captures the cumulative counts to date. Replicating the Indiana maps
# requires daily new case counts at the local level, and I only have that in the tables I've downloaded
# from the dashboard starting on 2020-09-25. So I only have four weeks of that, so I could only do four maps of 7 days each or two maps of 14 days (average 7-day rates over two consecutive weeks.)


# NEVER MIND. It's actually worse, since I only started downloading the two local tables on 2020-10-13.
# I have less that 2 weeks of data to work with unless I can find the tables somewhere else.

# create local count data over time table ----

folders_here <- list.files("../daily_voala_dashboard/data") %>%
   purrr::discard(str_detect(., "readme"))


local_new_cases_by_time <- NULL

for (folder in folders_here) {
   this_file <- list.files(paste0("../daily_voala_dashboard/data/", folder)) %>%
      purrr::keep(str_detect(., "LA_County_Covid19_cases_deaths_date_table")) %>%
      unlist()
   this_table <- readr::read_csv(paste("./data", folder, this_file, sep = "/")) %>%
      dplyr::select(date_use, avg_cases) %>%
      mutate(date_use = as_date(date_use)) %>%
      rename(!!folder := avg_cases)
   if (is_null(cases_history_by_time)) {
      cases_history_by_time <- this_table
   } else {
      cases_history_by_time <- right_join(cases_history_by_time, this_table, by = c("date_use"))
   }
}

cases_history_by_time <- cases_history_by_time %>% rename(date_about = date_use)
cases_history_by_time <- cases_history_by_time %>% arrange(date_about)


# process count data ----

dph_count_data_long <- dph_count_data %>%
   pivot_longer(-city_community, names_to = "date", values_to = "cases")
