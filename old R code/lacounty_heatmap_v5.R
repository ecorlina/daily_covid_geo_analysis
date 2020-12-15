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

# la_county_topomap <- readRDS("../Mapping/Basemap downloads/la_county_topomap.rds")


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

# saveRDS(la_county_topomap, file = "la_county_carto_light_basemap.rds")

la_county_topomap <- readRDS("la_county_carto_light_basemap.rds")

# la_county_topomap


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




# grab VOALA program data from previous work ----

# 3. Import VOALA programs data file
voala_programs <- read_csv("../Mapping/VOALA programs 2019/VOALA_Program_analysisTable-20200406.csv", col_types = "fffiffcccdfdccddcf")

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


# import count data from DPH, add to data history, and merge each day with geo data ----

# sheets_here <- excel_sheets("./data/dph_city_community_counts.xlsx")
# n_days <- length(sheets_here)
# this_sheet <- sheets_here[1]
# local_counts <- read_xlsx("./data/dph_city_community_counts.xlsx", sheet = this_sheet)

dph_count_data <- read_rds("dph_count_data.rds")
dph_rate_data <- read_rds("dph_rate_data.rds")
neighborhoods_crosswalk <- read_rds("neighborhoods_crosswalk.rds")


# get the latest data from the web
dphpage <- read_html("http://publichealth.lacounty.gov/media/Coronavirus/locations.htm")

tbl <- dphpage %>% html_nodes("table") %>% .[1] %>% html_table() %>% .[[1]]

lb_pas_data_start_row <- which(str_detect(tbl$X1, "Laboratory Confirmed Cases")) + 2
lb_pas_data_end_row <- which(str_detect(tbl$X1, "Laboratory Confirmed Cases")) + 3

neighborhood_data_start_row <- which(str_detect(tbl$X1, "CITY/COMMUNITY")) + 1
neighborhood_data_end_row <- max(which(str_detect(tbl$X1, "Under [Ii]nvestigation"))) - 1


# new_count_data$city_community <- str_remove(new_count_data$city_community, "\\*")
# new_rate_data$city_community <- str_remove(new_rate_data$city_community, "\\*")


lb_pas_data <- tibble(tbl[lb_pas_data_start_row:lb_pas_data_end_row,]) %>%
   rename(city_community = X1,
          count = X2,
          rate = X3) %>%
   mutate(city_community = str_replace(city_community, "-+ ", "City of ")) %>%
   mutate(count = ifelse(count == "--", NA, count)) %>%
   mutate(count = as.numeric(count)) %>%
   mutate(rate = ifelse(count == "--", NA, rate)) %>%
   mutate(rate = as.numeric(rate))

citycomm_data <- tibble(tbl[neighborhood_data_start_row:neighborhood_data_end_row,]) %>%
   rename(city_community = X1,
          count = X2,
          rate = X3) %>%
   mutate(city_community = str_remove(city_community, "\\*")) %>%
   mutate(count = ifelse(count == "--", NA, count)) %>%
   mutate(count = as.numeric(count)) %>%
   mutate(rate = ifelse(count == "--", NA, rate)) %>%
   mutate(rate = as.numeric(rate))

new_data <- bind_rows(citycomm_data, lb_pas_data)

test <- left_join(dph_count_data[,c(1,ncol(dph_count_data))], new_data[,1:2]) %>%
   setNames(c("place", "last_import", "current_page")) %>%
   mutate(change = current_page - last_import)

sum(test$change, na.rm = T)

if (sum(test$change, na.rm = T) != 0) {
   new_count_data <- new_data[, c("city_community", "count")] %>% setNames(c("city_community", as.character(today())))
   dph_count_data <- left_join(dph_count_data, new_count_data)
   new_rate_data <- new_data[, c("city_community", "rate")] %>% setNames(c("city_community", as.character(today())))
   dph_rate_data <- left_join(dph_rate_data, new_rate_data)
}

write_rds(dph_count_data, "dph_count_data.rds")
write_rds(dph_rate_data, "dph_rate_data.rds")

# dph_count_data <- dph_count_data %>% dplyr::select(-`2020-05-09`)
# dph_rate_data <- dph_rate_data %>% dplyr::select(-`2020-05-09`)

# compute quantiles ----

# for manual autoscaling as the counts grow
# breaks = c(0, 1, seq(25, 300, by = 25)
# breaks = c(0, 1, seq(50, 750, by = 50)
# breaks = c(0, seq(100, 800, by = 100)

# remove LA from data since it's covered by all the neighborhoods


omega <- ncol(dph_count_data)
count_quintiles <- round(unname(quantile(dph_count_data[,omega], probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)))

today_counts <- dph_count_data[, c(1, ncol(dph_count_data))] %>%
   setNames(c("city_community", "count")) %>%
   mutate(map_to = vlookup(.$city_community, neighborhoods_crosswalk, "city_community", "map_to"))
today_data <- today_counts %>%
   group_by(map_to) %>%
   summarize(count = sum(count, na.rm = T))
ndsc_map_today <- left_join(ndsc_map, today_data, by = c("name" = "map_to")) %>%
   mutate(rate_comp = round((count / as.numeric(population)) * 100000, digits = 2) )
count_deciles <- ceiling(unname(quantile(
   ndsc_map_today$count,
   probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
   na.rm = T
)))
rate_quintiles <- ceiling(unname(quantile(
   ndsc_map_today$rate_comp,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))

count_deciles
rate_quintiles


# make all the basemaps ----

for (i in 2:ncol(dph_count_data)) {
   these_counts <- dph_count_data[, c(1, i)]
   this_date <- colnames(these_counts)[2]
   these_counts <- these_counts %>%
      setNames(c("city_community", "count")) %>%
      mutate(map_to = vlookup(.$city_community, neighborhoods_crosswalk, "city_community", "map_to"))
   these_data <- these_counts %>%
      group_by(map_to) %>%
      summarize(count = sum(count, na.rm = T))
   ndsc_map_counts <- left_join(ndsc_map, these_data, by = c("name" = "map_to")) %>%
      mutate(rate_comp = round((count / as.numeric(population)) * 100000, digits = 2))
   if (i == ncol(dph_count_data)) {
      basemap_counts <- 
         tm_shape(ndsc_map_counts) +
         tm_fill(col = "count",
                 style = "fixed",
                 breaks = c(0, seq(100, 1000, by = 100)),  # mine; natural
                 #breaks = count_deciles,  # a la DPH dashboard; quintiles/deciles
                 title = "Confirmed Cases",
                 group = str_c("COVID-19 case count, ", this_date)) +
         tm_borders()
      basemap_rates <- 
         tm_shape(ndsc_map_counts) +
         tm_fill(col = "rate_comp",
                 style = "fixed",
                 #breaks = c(0, 10, 32, 100, 316, 1000, 3162, 10000),  # mine; 10^(0.5x)
                 breaks = rate_quintiles,  # a la DPH dashboard; quintiles
                 title = "Confirmed Case Rates (per 100k)",
                 group = str_c("COVID-19 case rate")) +
         tm_borders()
   }
   map_layer_counts <-
      tm_shape(ndsc_map_counts) +
      tm_fill(col = "count",
              style = "fixed",
              breaks = c(0, seq(100, 1000, by = 100)),  # mine; natural
              #breaks = count_deciles,  # a la DPH dashboard; quintiles/deciles
              title = "Confirmed Cases",
              group = str_c("COVID-19 case count, ", this_date),
              legend.show = ifelse(i == 2, T, F)) +
      tm_borders()
   assign(str_c("basemap_", i-1), map_layer_counts)
   map_layer_rates <-
      tm_shape(ndsc_map_counts) +
      tm_fill(col = "rate_comp",
              style = "fixed",
              #breaks = c(0, 10, 32, 100, 316, 1000, 3162, 10000),  # mine; 10^(0.5x)
              breaks = rate_quintiles,  # a la DPH dashboard; quintiles
              title = "Confirmed Case Rates (per 100k)",
              group = str_c("COVID-19 case rate, ", this_date),
              legend.show = ifelse(i == 2, T, F)) +
      tm_borders()
   assign(str_c("basemap_rates_", i-1), map_layer_rates)
}


# THE ORIGINAL BASEMAP LOOP IS IN v4

# draw the map - final ----

map_styling <- tm_layout(
   main.title = str_c("COVID-19 in LA County"),
   main.title.size = 1.25,
   legend.position = c("left", "bottom"),
   legend.frame = TRUE,
   legend.title.size = 0.8,
   legend.text.size = 0.6
)


# count map ----

the_count_map_binary_by_day <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_1 +
   basemap_8 +
   basemap_15 +
   basemap_22 +
   basemap_29 +
   basemap_36 +
   basemap_38 +
   basemap_39 +
   basemap_40 +
   basemap_41 +
   tm_shape(voala_locations_other) + 
   tm_symbols(
      size = 0.1, shape.showNA = F,
      col = "#66A61E",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "All other programs (green)") +
   tm_shape(voala_locations_shelterres) + 
   tm_symbols(
      size = 0.1, shape.showNA = F,
      col = "#E7298A",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Residential and shelter (pink)") +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()), main.title.size = 1.1)

# interactive
tmap_leaflet(
   the_count_map_binary_by_day,
   mode = "view"
)

static_count_map_binary <- la_county_topomap + the_count_map_binary_by_day + map_styling
tmap_save(static_count_map_binary, filename = str_c("./maps/lac_covid_2cat-", today(), ".pdf"), width = 6.5, height = NA, units = "in")



# rate map ----

the_rate_map_binary_by_day <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_rates_1 +
   basemap_rates_8 +
   basemap_rates_15 +
   basemap_rates_22 +
   basemap_rates_29 +
   basemap_rates_36 +
   basemap_rates_38 +
   basemap_rates_39 +
   basemap_rates_40 +
   basemap_rates_41 +
   tm_shape(voala_locations_other) + 
   tm_symbols(
      size = 0.1, shape.showNA = F,
      col = "#66A61E",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "All other programs (green)") +
   tm_shape(voala_locations_shelterres) + 
   tm_symbols(
      size = 0.1, shape.showNA = F,
      col = "#E7298A",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Residential and shelter (pink)") +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()), main.title.size = 1.1)

# interactive
tmap_leaflet(
   the_rate_map_binary_by_day,
   mode = "view"
)

static_rate_map_binary <- la_county_topomap + the_rate_map_binary_by_day + map_styling
tmap_save(static_rate_map_binary, filename = str_c("./maps/lac_covid_2cat_rates-", today(), ".pdf"), width = 6.5, height = NA, units = "in")



# single-day mixed map ----

the_count_map_binary_with_rate <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_rates +
   basemap_counts +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_shape(voala_locations_other) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#66A61E",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "All other programs (green)") +
   tm_shape(voala_locations_shelterres) + 
   tm_symbols(
      shape = 16, size = 0.1, shape.showNA = F,
      col = "#E7298A",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Residential and shelter (pink)") +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()), main.title.size = 1.1)

# interactive
tmap_leaflet(
   the_count_map_binary_with_rate,
   mode = "view"
) %>%
   leaflet::hideGroup("COVID-19 case rate")


# animation of spread (without VOALA dots) ----

dph_data <- dph_count_data %>% pivot_longer(-city_community, names_to = "date", values_to = "total_confirmed_cases")

dph_data %>% filter(city_community != "Los Angeles") %>%
   ggplot(mapping = aes(x = date, y = total_confirmed_cases, color = city_community)) +
   geom_point(show.legend = F)

# this is informative, but also useless since it skips the important step of transforming from the DPH neighborhoods to the LA Times neighborhoods
# need to do the aggregation date by date and incrementally bind_rows to build up a usable long-form data table

dph_count_data_long <- NULL

for (i in 2:ncol(dph_count_data)) {
   these_counts <- dph_count_data[, c(1, i)]
   this_date <- colnames(these_counts)[2]
   these_counts <- these_counts %>%
      setNames(c("city_community", "count")) %>%
      mutate(map_to = vlookup(.$city_community, neighborhoods_crosswalk, "city_community", "map_to"))
   these_data <- these_counts %>%
      group_by(map_to) %>%
      summarize(count = sum(count, na.rm = T)) %>%
      filter(!is.na(map_to)) %>%
      mutate(date = this_date)
   dph_count_data_long <- bind_rows(dph_count_data_long, these_data)
}

dph_count_data_long %>%
   ggplot(mapping = aes(x = date, y = count, color = map_to)) +
   geom_point(show.legend = F)


lac_covid_long_geo <- left_join(ndsc_map, dph_count_data_long, by = c("name" = "map_to")) %>%
   mutate(rate_comp = round((count / as.numeric(population)) * 100000, digits = 2) )
lac_covid_long_geo <- lac_covid_long_geo %>% filter(!is.na(date))


# small cases map
lac_cases <-
   la_county_topomap +
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   tm_shape(lac_covid_long_geo) +
   tm_borders(lwd = 0.5) +
   tm_fill(col = "count", n = 10, title = "Confirmed Cases") +
   tm_layout(scale = 0.5, legend.position = c("right", "top")) +
   tm_facets(along = "date", free.coords = F, free.scales = F, nrow = 1, ncol = 1) +
   # tm_shape(voala_locations_other) + 
   # tm_symbols(
   #    size = 0.05, shape.showNA = F,
   #    col = "#66A61E",
   #    border.lwd = 0.5, border.col = "black", border.alpha = 0.85,
   #    jitter = 0.075,
   #    id = "ProgramNameFormatted",
   #    popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
   #    group = "All other programs (green)") +
   tm_shape(voala_locations_shelterres) + 
   tm_symbols(
      size = 0.05, shape.showNA = F,
      col = "#E7298A",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.85,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Residential and shelter (pink)") +
   tm_layout(main.title.size = 0.6, legend.outside = T)


# lac_cases

tmap_animation(lac_cases, filename = str_c("./movies/", "lac_cases-", today(), ".gif"), width = 800, height = 620, delay = 75, restart.delay = 75)


# small rates map
lac_rates <-
   la_county_topomap +
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   tm_shape(lac_covid_long_geo) +
   tm_borders(lwd = 0.5) +
   tm_fill(col = "rate_comp", style = "log10_pretty", title = "Cases per 100k") +
   tm_layout(scale = 0.5, legend.position = c("right", "top")) +
   tm_facets(along = "date", free.coords = F, free.scales = F, nrow = 1, ncol = 1) +
   # tm_shape(voala_locations_other) + 
   # tm_symbols(
   #    size = 0.05, shape.showNA = F,
   #    col = "#66A61E",
   #    border.lwd = 0.5, border.col = "black", border.alpha = 0.85,
   #    jitter = 0.075,
   #    id = "ProgramNameFormatted",
   #    popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
   #    group = "All other programs (green)") +
   tm_shape(voala_locations_shelterres) + 
   tm_symbols(
      size = 0.05, shape.showNA = F,
      col = "#E7298A",
      border.lwd = 0.5, border.col = "black", border.alpha = 0.85,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Residential and shelter (pink)") +
   tm_layout(main.title.size = 0.6, legend.outside = T)


# lac_rates

tmap_animation(lac_rates, filename = str_c("./movies/", "lac_rates-", today(), ".gif"), width = 800, height = 620, delay = 75, restart.delay = 75)

# to convert the gif to a useful, working MPEG file!
# update the date in the filenames below!!
# launch Terminal.app
# cd "Dropbox/Rocinante Research/VOALA MBPro/confirmed_cases/movies"
# ffmpeg -i lac_cases-2020-05-12.gif -pix_fmt yuv420p lac_cases-2020-05-12.mp4
# ffmpeg -i lac_rates-2020-05-12.gif -pix_fmt yuv420p lac_rates-2020-05-12.mp4



# if you copy the images into the target folder and rename them in the finder...
# cd "Dropbox/Rocinante Research/VOALA MBPro/confirmed_cases/curves/old counts graphs/case counts"
# ffmpeg -r 4 -i pic%05d.jpg -vcodec libx264 -crf 25 -s 700x510 -pix_fmt yuv420p test.mp4