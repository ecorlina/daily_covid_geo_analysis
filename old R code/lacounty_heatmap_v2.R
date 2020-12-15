# load packages ----
library(tidyverse)
library(lubridate)
library(readxl)
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


# define functions ----

# Excel-equivalent vlookup function courtesy of a tweet by Jenny Bryan of the RStudio team
vlookup <- function(this, data, key, value) {
   m <- match(this, data[[key]])
   data[[value]][m]
}


# import shapefiles and set up maps ----

# LA COUNTY OUTLINE
# read the full data set with 9 SoCal counties
counties <- read_sf("../Mapping/QGIS DATA/County Boundaries.geojson")

# extract just mainland LA County from the counties data set
lacounty_mainland <- counties %>% filter(objectid == "22") %>% st_as_sf()

la_county_topomap <- readRDS("../Mapping/Basemap downloads/la_county_topomap.rds")


# USC NDSC city/neighborhood data

ndsc_map <- read_sf("./ndsc geodata/Neighborhoods_from_gw34_aa8i.geojson")

latimes_map <- read_sf("./la times/la-county-neighborhoods-current.geojson")

ndsc_map$name
latimes_map$name

setdiff(ndsc_map$name, latimes_map$name)

local_population <- read_xlsx("./data/neighborhood_population_2018.xlsx", sheet = "Demography")

local_population$`Shape Name`

setdiff(ndsc_map$name, local_population$`Shape Name`)

ndsc_map <- left_join(ndsc_map, local_population, by = c("name" = "Shape Name")) %>%
   rename(population = Total)



# use the ndsc map since that's the basis for the connections with the DPH reporting

# import count data and merge with geo data ----

sheets_here <- excel_sheets("./data/dph_city_community_counts.xlsx")
this_sheet <- sheets_here[1]
local_counts <- read_xlsx("./data/dph_city_community_counts.xlsx", sheet = this_sheet)

local_counts_summary <- local_counts %>%
   group_by(map_to) %>%
   summarize(count = sum(count, na.rm = T))



# add the count data to the map

ndsc_map_counts <- left_join(ndsc_map, local_counts_summary, by = c("name" = "map_to")) %>%
   mutate(rate_comp = (count / as.numeric(population)) * 100000 )
   





# grab VOALA program data from previous work ----
# -----
# 3. Import data files 
voala_programs <- read_csv("../Mapping/Patti/VOALA_Program_analysisTable-20200406.csv", col_types = "fffiffcccdfdccddcf")

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


tm_shape(voala_locations) +
   tm_dots(
      col = "dodgerblue",
      shapes = c(21, 23), shape.showNA = F,
      size = 0.125,
      jitter = 0.075,
      id = "ProgramNameFormatted")



tmap_mode("plot")

# draw the map - prelim code ----

# tm_shape(lacounty_mainland) +
#    tm_borders() +
#    tm_shape(ndsc_map_counts) +
#    tm_fill(col = "count", style = "fixed", breaks = c(0, 1, 20, 40, 60, 80, 100, 120, 140, 160), alpha = 0.65, title = "Confirmed Cases") +
#    tm_layout(main.title = "COVID-19 in Los Angeles County, April 2", main.title.size = 1.1, legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
#    tm_borders() +
#    tm_shape(voala_locations) + 
#    tm_symbols(
#       shape = 16, shape.showNA = F,
#       size = 0.1, col = "dodgerblue",
#       border.col = "black", border.alpha = .5,
#       jitter = 0.075) +
#    tm_credits("Data from LA County DPH -- http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm",
#               size = 0.5,
#               position = c("left", "bottom"))
# 
# 
# tmap_save(filename = str_c("./lac_covid-", today(), ".pdf"), width = 6.5, height = NA, units = "in")



# draw the map - final ----

unique(voala_locations$BudgetCat)

voala_locations_childcare <- voala_locations %>% filter(BudgetCat == "Childcare") # "#A6761D"
voala_locations_youth <- voala_locations %>% filter(BudgetCat == "Youth & Respite") # "#E6AB02"
voala_locations_corrections <- voala_locations %>% filter(BudgetCat == "Corrections") # "#1B9E77"
voala_locations_recovery <- voala_locations %>% filter(BudgetCat == "Recovery") # "#D95F02"
voala_locations_veterans <- voala_locations %>% filter(BudgetCat == "Veterans") # "#7570B3"
voala_locations_apartments <- voala_locations %>% filter(BudgetCat == "Apartments") # "#66A61E"
voala_locations_homeless <- voala_locations %>% filter(BudgetCat == "Homeless") # "#E7298A"

the_count_map <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   tm_shape(ndsc_map_counts) +
   tm_fill(col = "rate_comp",
           style = "log10_pretty",
           title = "Confirmed Case Rates (per 100k)",
           group = "COVID-19 case rate") +
   tm_borders() +
   tm_shape(ndsc_map_counts) +
   tm_fill(col = "count",
           style = "fixed",
           breaks = c(0, 1, 25, 50, 75, 100, 125, 150, 175, 200),
           title = "Confirmed Cases",
           group = "COVID-19 case count") +
   tm_borders() +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_shape(voala_locations_childcare) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#A6761D",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Childcare (brown)") +
   tm_shape(voala_locations_youth) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#E6AB02",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Youth & Respite (yellow)") +
   tm_shape(voala_locations_corrections) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#1B9E77",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Corrections (dark green)") +
   tm_shape(voala_locations_recovery) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#D95F02",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Recovery (orange)") +
   tm_shape(voala_locations_veterans) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#7570B3",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Veterans (purple)") +
   tm_shape(voala_locations_apartments) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#66A61E",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Apartments (green)") +
   tm_shape(voala_locations_homeless) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#E7298A",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Homeless (pink)") +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()), main.title.size = 1.1)

# interactive
tmap_leaflet(
   the_count_map,
   mode = "view"
)

static_count_map <- la_county_topomap + the_count_map
tmap_save(static_count_map, filename = str_c("./maps/lac_covid-", today(), ".pdf"), width = 6.5, height = NA, units = "in")






# using rates -- BUT IT ISN'T CORRECT BECAUSE I DON'T KNOW EACH POPULATION VALUE ----
# SO DONT REPORT THESE. CODE IS FOR EXPERIMENTING ONLY

local_rate_summary <- local_counts %>%
   group_by(map_to) %>%
   summarize(rate = sum(100000*rate, na.rm = T)/100000)


ndsc_map_rates <- left_join(ndsc_map, local_rate_summary, by = c("name" = "map_to"))


the_rate_map <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   tm_shape(ndsc_map_rates) +
   tm_fill(col = "rate",
           style = "log10_pretty",
           alpha = 0.75,
           title = "Confirmed Case Rates (per 100k)",
           group = "COVID-19 case rate") +
   tm_borders() +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_shape(voala_locations_childcare) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#A6761D",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Childcare (brown)") +
   tm_shape(voala_locations_youth) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#E6AB02",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Youth & Respite (yellow)") +
   tm_shape(voala_locations_corrections) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#1B9E77",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Corrections (dark green)") +
   tm_shape(voala_locations_recovery) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#D95F02",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Recovery (orange)") +
   tm_shape(voala_locations_veterans) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#7570B3",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Veterans (purple)") +
   tm_shape(voala_locations_apartments) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#66A61E",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Apartments (green)") +
   tm_shape(voala_locations_homeless) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#E7298A",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Homeless (pink)") +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()), main.title.size = 1.1)

# interactive
tmap_leaflet(
   the_rate_map,
   mode = "view"
)

static_rate_map <- la_county_topomap + the_rate_map
tmap_save(static_rate_map, filename = str_c("./maps/lac_covid-", today(), ".pdf"), width = 6.5, height = NA, units = "in")
