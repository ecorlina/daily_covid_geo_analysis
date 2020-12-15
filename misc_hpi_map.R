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


# import shapefiles and set up maps ----

# LA COUNTY OUTLINE
# read the full data set with 9 SoCal counties
counties <- read_sf("../../Mapping/QGIS DATA/County Boundaries.geojson")

# extract just mainland LA County from the counties data set
lacounty_mainland <- counties %>% filter(objectid == "22") %>% st_as_sf()


# https://github.com/CartoDB/basemap-styles
# this is where I got the info to use for carto basemaps!

# la_county_topomap_osm <- read_osm(
#    lacounty_mainland,
#    ext = 1.1,
#    zoom = 11,
#    type = "https://b.basemaps.cartocdn.com/light_nolabels/{z}/{x}/{y}{scale}.png"
# )
# 
# la_county_topomap <- tm_shape(la_county_topomap_osm) + tm_rgb()
# 
# saveRDS(la_county_topomap, file = "la_county_carto_light_basemap.rds")

la_county_topomap <- readRDS("la_county_carto_light_basemap.rds")


# la_county_topomap

lac_supervisorial <- read_sf("../data/Supervisorial_District_2011")


# https://egis-lacounty.hub.arcgis.com/datasets/countywide-statistical-areas-csa/data
# This is where I got the shapefile that matches the DPH map!!

csa_map <- read_sf("../data/Community Boundaries (CSA)") %>%
   arrange(label)

ct_map <- read_sf("../data/CENSUS_TRACTS_2010") %>%
   arrange(GEOID10)

# SUPERVISORIAL DISTRICTS
csa_by_supdist_map_v0 <- read_sf("../data/Community Boundaries (CSA) w SupDists") %>%
   arrange(label, supdist) %>%
   dplyr::select(label, supdist, everything())

csa_area_totals <- csa_by_supdist_map_v0 %>%
   group_by(label) %>%
   summarize(total_area = sum(area)) %>%
   st_drop_geometry()

csa_by_supdist_map <- left_join(csa_by_supdist_map_v0, csa_area_totals)

csa_by_supdist_map <- csa_by_supdist_map %>%
   mutate(area_pct = area / total_area)

csa_map$label


la_county_topomap +
   tm_shape(ct_map) +
   tm_borders()


# import the HPI data table

hpi <- read_csv("../data/HPI/hpi.csv") %>%
   arrange(geoid) %>%
   filter(county_name == "Los Angeles")

sum(duplicated(ct_map$GEOID10))
sum(duplicated(hpi$geoid))

length(unique(ct_map$GEOID10))
length(unique(hpi$geoid))

setdiff(ct_map$GEOID10, hpi$geoid)    # "06037137000"
setdiff(hpi$geoid, ct_map$GEOID10)    # "06037930401" "06037990100" "06037990200"
# why should order matter...

hpigeoid <- hpi %>% dplyr::select(geoid)
ctmapgeoid <- ct_map %>% dplyr::select(GEOID10) %>% st_drop_geometry()

full_join(hpigeoid, ctmapgeoid, by = c("geoid" = "GEOID10"), keep = T) %>% View()

# that finally answered the riddle.
# there are actually three mismatched values.
# in the HPI table, values 06037990100 and 06037990200 do not have a match
# in the CT table, value 06037137000 does not have a match

# I'm going to commit to moving forward and say that dropping 3 items out of a merged list of 2347 isn't going to affect my ability to detect a pattern across LA County

ct_hpi_map <- left_join(ct_map, hpi, by = c("GEOID10" = "geoid")) %>%
   mutate(hpi_quartile = case_when(between(hpi2_pctile, 0, 25) ~ 1,
                                   between(hpi2_pctile, 25, 50) ~ 2,
                                   between(hpi2_pctile, 50, 75) ~ 3,
                                   between(hpi2_pctile, 75, 100) ~ 4)) %>%
   mutate(hpi_quartile = as_factor(hpi_quartile))

ct_hpi_map$hpi_quartile

la_county_topomap +
   tm_shape(ct_hpi_map) +
   tm_fill(col = "hpi2_pctile",
               palette = hcl.colors(11, "Sunset")[2:10],
               n = 9) +
   tm_layout(
      main.title = str_c("HPI Percentiles in LA County"),
      main.title.size = 1.25,
      legend.position = c("left", "bottom"),
      legend.frame = TRUE,
      legend.title.size = 0.8,
      legend.text.size = 0.6
   )

tmap_save(filename = "./hpi map/hpi_map_pctile.pdf", width = 6.5, height = NA, units = "in")


la_county_topomap +
   tm_shape(ct_hpi_map) +
   tm_fill(col = "hpi_quartile",
           palette = c("#762A83", "#E7D4E8", "#D9F0D3", "#ACD39E"),
           n = 4) +
   tm_layout(
      main.title = str_c("HPI Quartiles in LA County"),
      main.title.size = 1.25,
      legend.position = c("left", "bottom"),
      legend.frame = TRUE,
      legend.title.size = 0.8,
      legend.text.size = 0.6
   )

hpi_qt_map <- tm_shape(ct_hpi_map) +
   tm_fill(col = "hpi_quartile",
           palette = c("#762A83", "#E7D4E8", "#D9F0D3", "#ACD39E"),
           n = 4,
           title = "HPI Quartiles",
           group = str_c("Healthy Places Index Quartiles, 2020")) +
   tm_layout(
      main.title = str_c("HPI Percentiles in LA County"),
      main.title.size = 1.25,
      legend.position = c("left", "bottom"),
      legend.frame = TRUE,
      legend.title.size = 0.8,
      legend.text.size = 0.6
   )


tmap_save(filename = "./hpi map/hpi_map_quartile.pdf", width = 6.5, height = NA, units = "in")
