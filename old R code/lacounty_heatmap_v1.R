# load packages ----
library(tidyverse)
library(lubridate)
library(readxl)
library(tidyxl)
library(unpivotr)
library(hablar)
library(janitor)
library(busdater)

library(rgdal)
library(sf)
library(raster)
library(tmap)    # for static and interactive maps
library(tmaptools)  # for access to read_osm function for basemaps
library(rJava)
library(OpenStreetMap)


# define functions ----

# Excel-equivalent vlookup function courtesy of a tweet by Jenny Bryan of the RStudio team
vlookup <- function(this, data, key, value) {
   m <- match(this, data[[key]])
   data[[value]][m]
}


# import shapefiles

# LA COUNTY
# read the full data set with 9 SoCal counties
counties <- read_sf("../Mapping/QGIS DATA/County Boundaries.geojson")

# extract just mainland LA County from the counties data set
lacounty_mainland <- counties %>% filter(objectid == "22") %>% st_as_sf()

la_county_topomap <- readRDS("../Mapping/Basemap downloads/la_county_topomap.rds")


# LA COUNTY CITIES
city_boundaries <-readOGR("../Mapping/QGIS DATA/LA_County_City_Boundaries", layer = "LA_County_City_Boundaries", stringsAsFactors = F)

city_boundaries <- city_boundaries[city_boundaries$CITY_NAME != "Unincorporated", ] %>% st_as_sf()

# LA CITY NEIGHBORHOODS

la_hoods <-readOGR("../Mapping/QGIS DATA/LA_Times_Neighborhood_Boundaries", layer = "LA_Times_Neighborhood_Boundaries", stringsAsFactors = F) %>% st_as_sf()


city_boundaries$CITY_NAME
la_hoods$name



# import count data and merge with geo data
counts <- read_xlsx("./latimes_city_neighborhood_counts.xlsx", sheet = "20200402 0900")

cities_counts <- left_join(city_boundaries, counts, by = c("CITY_NAME" = "place"))
hoods_counts <- left_join(la_hoods, counts, by = c("name" = "place"))


la_county_topomap +
   tm_shape(lacounty_mainland) +
   tm_borders() +
   tm_shape(cities_counts) +
   tm_fill(col = "count", n = 10, style = "fixed", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110), alpha = 0.85, title = "", legend.show = F) +
   tm_borders() +
   tm_shape(hoods_counts) +
   tm_fill(col = "count", n = 10, style = "fixed", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110), alpha = 0.85, title = "") +
   tm_layout(legend.show = T) +
   tm_borders()




# grab VOALA program data from previous work ----
# -----
# 3. Import data files 
voala_programs <- read_csv("../Mapping/Patti/VOALA_Program_analysisTable.csv", col_types = "fffiffcccdfdccddc")

# voala_programs <- voala_programs %>% mutate(FundSource = ifelse(FederalState == 1, "FederalState", "Local"))

# reorder BudgetCat factor levels to something intentional
voala_programs <- voala_programs %>% mutate(BudgetCat = BudgetCat %>% fct_infreq())

# clean up the Director Name entries if it hasn't already been done
voala_programs$DirectorALT <- voala_programs$DirectorName %>% str_replace("\\s\\(.+\\)$", "")

# drop entries with no physical address
voala_programs_filtered <- voala_programs %>% filter(!is.na(Latitude)) %>% filter(MapLocalized == 1)

# convert the tibble into a geometry object
voala_locations <- st_as_sf(voala_programs_filtered, coords = c("Longitude", "Latitude"), crs = 4326)
# Note that the CRS choice comes from examining the CRS from several of the geojson and kml files downloaded from geohub.lacity.org


la_county_topomap +
   tm_shape(voala_locations) + 
   tm_dots(
      col = "dodgerblue",
      shapes = c(21, 23), shape.showNA = F,
      size = 0.125,
      jitter = 0.075)


la_county_topomap +
   tm_shape(lacounty_mainland) +
   tm_borders() +
   tm_shape(cities_counts) +
   tm_fill(col = "count", style = "fixed", breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160), alpha = 0.85, title = "", legend.show = F) +
   tm_borders() +
   tm_shape(hoods_counts) +
   tm_fill(col = "count", style = "fixed", breaks = c(0, 20, 40, 60, 80, 100, 120, 140, 160), alpha = 0.85, title = "Confirmed Cases") +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_borders() +
   tm_shape(voala_locations) + 
   tm_symbols(
      shape = 16, shape.showNA = F,
      size = 0.1, col = "dodgerblue",
      border.col = "black", border.alpha = .5,
      jitter = 0.075)


