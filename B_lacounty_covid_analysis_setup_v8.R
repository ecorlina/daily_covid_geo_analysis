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


# critical for new workflow: define the output path for all geo_analysis output
voala_covid_folder_path <- "/Users/rickorlina/Dropbox/Rocinante Research/VOALA MBPro/Covid_19"
covid_analysis_output_path <- str_c(voala_covid_folder_path, "/covid_analysis_output")
geo_analysis_output_path <- str_c(covid_analysis_output_path, "/geo_analysis")


# import shapefiles and set up maps ----

# LA COUNTY OUTLINE
# read the full data set with 9 SoCal counties
counties <- read_sf("./data/gis_data/County Boundaries.geojson")

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
# saveRDS(la_county_topomap, file = "./data/la_county_carto_light_basemap.rds")

la_county_topomap <- readRDS("./data/la_county_carto_light_basemap.rds")

# la_county_topomap

lac_supervisorial <- read_sf("./data/gis_data/Supervisorial_District_2011")


# USC NDSC city/neighborhood data

ndsc_map <- read_sf("./data/ndsc_geodata/Neighborhoods_from_gw34_aa8i.geojson")

latimes_map <- read_sf("./data/la_times/la-county-neighborhoods-current.geojson")


# COUNTYWIDE STATISTICAL AREAS (CSA) as used by DPH
csa_map <- read_sf("./data/gis_data/Community Boundaries (CSA)") %>%
   arrange(label)

# following line was added on Nov 2
# this geo file came from LA Times data division github
# main benefit is that it uses simplified CSA borders, which results in smaller map files
# going forward, this is the preferred object for mapping DPH Covid-19 city/community data
csa_map_simplified <- read_sf("./data/gis_data/los-angeles-countywide-statistical-areas.json") %>%
   arrange(label)


# SUPERVISORIAL DISTRICTS
csa_by_supdist_map_v0 <- read_sf("./data/gis_data/Community Boundaries (CSA) w SupDists") %>%
   arrange(label, supdist) %>%
   dplyr::select(label, supdist, everything())

csa_area_totals <- csa_by_supdist_map_v0 %>%
   group_by(label) %>%
   summarize(total_area = sum(area)) %>%
   st_drop_geometry()

csa_by_supdist_map <- left_join(csa_by_supdist_map_v0, csa_area_totals)

csa_by_supdist_map <- csa_by_supdist_map %>%
   mutate(area_pct = area / total_area)

# SCHOOL DISTRICTS
csa_by_schldist_map_v0 <- read_sf("./data/gis_data/Community Boundaries (CSA) w schldist") %>%
   arrange(label, LABEL_2) %>%
   dplyr::select(label, LABEL_2, everything())

csa_by_schldist_area_totals <- csa_by_supdist_map_v0 %>%
   group_by(label) %>%
   summarize(total_area = sum(area)) %>%
   st_drop_geometry()

csa_by_schldist_map <- left_join(csa_by_schldist_map_v0, csa_by_schldist_area_totals)

csa_by_schldist_map <- csa_by_schldist_map %>%
   mutate(area_pct = area / total_area)

# LAUSD LOCAL DISTRICTS
csa_by_lausd_map_v0 <- read_sf("./data/gis_data/Community Boundaries (CSA) - LAUSDlocdist") %>%
   arrange(label, LD_DIST) %>%
   dplyr::select(label, LD_DIST, everything())

csa_by_lausd_area_totals <- csa_by_lausd_map_v0 %>%
   group_by(label) %>%
   summarize(total_area = sum(area)) %>%
   st_drop_geometry()

csa_by_lausd_map <- left_join(csa_by_lausd_map_v0, csa_by_lausd_area_totals)

csa_by_lausd_map <- csa_by_lausd_map %>%
   mutate(area_pct = area / total_area)


# https://egis-lacounty.hub.arcgis.com/datasets/countywide-statistical-areas-csa/data
# This is where I got the shapefile that matches the DPH map!!

csa_map$label
ndsc_map$name
latimes_map$name
csa_map_simplified$label

setdiff(ndsc_map$name, latimes_map$name)

local_population <- read_xlsx("./data/neighborhood_population_2018.xlsx", sheet = "Demography")

local_population$`Shape Name`

setdiff(ndsc_map$name, local_population$`Shape Name`)

names(ndsc_map)
names(local_population)

ndsc_map <- left_join(ndsc_map, local_population, by = c("name" = "Shape Name"))

setdiff(csa_map$label, csa_map_simplified$label)


# on 2020-08-05, after updating R and the dplyr package, the rename command started to return an error
# for now, the second half of the above command is removed and commented out here
# because this map it isn't actually used anymore
# if you do need this later, just use the original variable name: "Total"
# ndsc_map %>% dplyr::rename(population = Total)


# use the CSA map since that's a match to the data that you get from DPH
# AND if you do, you don't need to compute (most) rates since you can use the rates they report!




# grab VOALA program data from previous work ----

# 3. Import VOALA programs data file
voala_programs <- read_csv("~/Dropbox/Rocinante Research/VOALA MBPro/Mapping/VOALA programs 2019/VOALA_Program_analysisTable-20200406.csv", col_types = "fffiffcccdfdccddcf")

# voala_programs <- voala_programs %>% mutate(FundSource = ifelse(FederalState == 1, "FederalState", "Local"))

# reorder BudgetCat factor levels to something intentional
voala_programs <- voala_programs %>% mutate(BudgetCat = BudgetCat %>% fct_infreq())

# clean up the Director Name entries if it hasn't already been done
voala_programs$DirectorALT <- voala_programs$DirectorName %>% str_replace("\\s\\(.+\\)$", "")

voala_programs_subset <- voala_programs %>%
   dplyr::select(-FederalState, -Local, -FundSource, -Budget, -Bacon, -Rate, -DirectorName)

# drop entries with no physical address
voala_programs_filtered <- voala_programs_subset %>%
   filter(!is.na(Latitude)) %>% filter(MapLocalized == 1)

# convert the tibble into a geometry object
voala_locations <- st_as_sf(voala_programs_filtered, coords = c("Longitude", "Latitude"), crs = 4326)
# Note that the CRS choice comes from examining the CRS from several of the geojson and kml files downloaded from geohub.lacity.org

unique(voala_locations$BudgetCat)

voala_locations_other <- voala_locations %>% filter(ResidentialShelter == 0) # "#66A61E"
voala_locations_shelterres <- voala_locations %>% filter(ResidentialShelter == 1) # "#E7298A"


la_county_topomap + tm_shape(voala_locations) +
   tm_symbols(
      col = "ResidentialShelter",
      palette = "Dark2",
      shape.showNA = F,
      size = 0.125,
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted")


# import location data for homeless shelters listed by DPH ----

homeless_locations <- read_xlsx("./data/dph_homeless_shelter_locations.xlsx",
                                sheet = "homeless_shelter_locations") %>%
   dplyr::select(-address, -source, -notes)


# import DPH city/community Covid-19 data from 2020-07-01, but just for their population numbers

csa_pops <- read_csv("./data/city_community_table.csv") %>%
   dplyr::select(geo_merge, population)

csa_map_pops <- left_join(csa_map_simplified, csa_pops, by = c("label" = "geo_merge")) %>%
   mutate(population = ifelse(label == "City of Long Beach", 465865, population)) %>%
   mutate(population = ifelse(label == "City of Pasadena", 140906, population))


# there are communities for which I don't have population numbers
# the follow code quickly checked to see if that's a problem
# there are no cases in the communities with no population estimate,
# so the missing values won't affect rate calculations
# 
# missing_pops <- csa_map_pops %>% dplyr::filter(is.na(population))
# dph_count_data %>%
#    dplyr::filter(city_community %in% missing_pops$label) %>%
#    dplyr::filter(!str_detect(city_community, "City of"))

csa_by_supdist_map_pops <- left_join(csa_by_supdist_map, csa_pops, by = c("label" = "geo_merge")) %>%
   mutate(population = ifelse(label == "City of Long Beach", 465865, population)) %>%
   mutate(population = ifelse(label == "City of Pasadena", 140906, population))

csa_by_supdist_map_pops <- csa_by_supdist_map_pops %>%
   mutate(split_pop = area_pct * population)


csa_by_schldist_map_pops <- left_join(csa_by_schldist_map, csa_pops, by = c("label" = "geo_merge")) %>%
   mutate(population = ifelse(label == "City of Long Beach", 465865, population)) %>%
   mutate(population = ifelse(label == "City of Pasadena", 140906, population))

csa_by_schldist_map_pops <- csa_by_schldist_map_pops %>%
   mutate(split_pop = area_pct * population)


csa_by_lausd_map_pops <- left_join(csa_by_lausd_map, csa_pops, by = c("label" = "geo_merge")) %>%
   mutate(population = ifelse(label == "City of Long Beach", 465865, population)) %>%
   mutate(population = ifelse(label == "City of Pasadena", 140906, population))

csa_by_lausd_map_pops <- csa_by_lausd_map_pops %>%
   mutate(split_pop = area_pct * population)


# import historical count data ----

dph_count_data <- read_rds("./data/dph_count_data.rds")
dph_deaths_data <- read_rds("./data/dph_deaths_data.rds")
dph_rate_data <- read_rds("./data/dph_rate_data.rds")
dph_deaths_rate_data <- read_rds("./data/dph_deaths_rate_data.rds")
neighborhoods_crosswalk <- read_rds("./data/neighborhoods_crosswalk.rds")  # you won't need this much longer!
dph_shelter_tbl <- read_rds("./data/dph_shelter_tbl.rds")

setdiff(dph_count_data$city_community, csa_map$label)
