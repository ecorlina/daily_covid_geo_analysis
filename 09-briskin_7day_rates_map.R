

# map with BRISKIN marker ----

briskin <- read_xlsx("./data/briskin.xlsx", sheet = "Sheet1")
briskin_location <- st_as_sf(briskin, coords = c("longitude", "latitude"), crs = 4326) %>%
   st_transform(6423)
st_crs(briskin_location)

rate_map_smooth_briskin <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   dashboard_basemap_rates_smooth +
   dashboard_basemap_rates_7day_smooth +
   tm_shape(briskin_location) + 
   tm_symbols(
      size = 0.1, shape.showNA = F,
      col = "#000099",
      alpha = 0.75,
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      id = "site",
      popup.vars = c("site", "address_given"),
      group = "Briskin Elementary School") +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()), main.title.size = 1.1)

# interactive
tmap_leaflet(
   rate_map_smooth_briskin,
   mode = "view"
) %>%
   leaflet::hideGroup(str_c("COVID-19 cumulative case rate, ", latest_date))


rate_map_7day_smooth_briskin <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   dashboard_basemap_rates_7day_smooth +
   tm_shape(briskin_location) + 
   tm_symbols(
      size = 0.1, shape.showNA = F,
      col = "#000099",
      alpha = 0.75,
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      id = "site",
      popup.vars = c("site", "address_given"),
      group = "Briskin Elementary School") +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()), main.title.size = 1.1)

static_rate_map_7day_smooth_briskin <- la_county_topomap + rate_map_7day_smooth_briskin + map_styling

tmap_save(static_rate_map_7day_smooth_briskin, filename = str_c(paste0("../covid_analysis_output/dashboard_output/other_maps/rates_map_7day_smooth_briskin-", latest_date,".png")), width = 6.5, height = NA, units = "in")

# tmap_save(static_rate_map_7day_smooth_briskin, filename = str_c(paste0("../covid_analysis_output/dashboard_output/other_maps/rates_map_7day_smooth_briskin-", latest_date,".pdf")), width = 6.5, height = NA, units = "in")



# change in 7-day case rate between latest and previous updates ----

lac_covid_long_csa_7day_change = left_join(csa_map_simplified, case_rate_7day_change, by = c("label" = "city_community"))

CHANGE_UPPER_LIMIT_THRESHOLD <- 350
CHANGE_LOWER_LIMIT_THRESHOLD <- -350

lac_covid_long_csa_7day_change <- lac_covid_long_csa_7day_change %>%
   mutate(change_in_past_week = ifelse(is.na(change_in_past_week), 0, change_in_past_week)) %>%
   mutate(change_in_past_week = ifelse(change_in_past_week > CHANGE_UPPER_LIMIT_THRESHOLD, CHANGE_UPPER_LIMIT_THRESHOLD, change_in_past_week)) %>%
   mutate(change_in_past_week = ifelse(change_in_past_week < CHANGE_LOWER_LIMIT_THRESHOLD, CHANGE_LOWER_LIMIT_THRESHOLD, change_in_past_week))


dashboard_basemap_rates_7day_change_smooth <-
   tm_shape(lac_covid_long_csa_7day_change) +
   tm_fill(col = "change_in_past_week",
           palette = "-RdYlGn",
           style = "cont",
           id = "label",
           title = "Case Rate (per 100k)",
           group = str_c("COVID-19 7-day case rate change, ", latest_date)) +
   tm_borders(lwd = 0.2)

rate_map_7day_change_smooth <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   dashboard_basemap_rates_7day_change_smooth +
   tm_shape(voala_locations_other) + 
   tm_symbols(
      size = 0.1, shape.showNA = F,
      col = "#66A61E",
      alpha = 0.5,
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "All other programs (green)") +
   tm_shape(voala_locations_shelterres) + 
   tm_symbols(
      size = 0.1, shape.showNA = F,
      col = "#E7298A",
      alpha = 0.5,
      border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
      jitter = 0.075,
      id = "ProgramNameFormatted",
      popup.vars = c("BudgetCat", "ProgramName", "StandardizedAddress"),
      group = "Residential and shelter (pink)") +
   tm_layout(main.title = str_c("COVID-19 in LA County (change in 7-day rate over past week), ", today()), main.title.size = 1.1)

static_rate_map_7day_change_smooth <- la_county_topomap + rate_map_7day_change_smooth + map_styling
static_rate_map_7day_change_smooth

