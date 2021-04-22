# run the setup code ----
# only need to do this when starting with an empty environment

# source("02-lacounty_covid_analysis_setup_v8.R")

# run the data import code ----
# first, double-check that dph_count_data and dph_rate_data are up to date through 'yesterday'
# this gets run every day to do final prep for map-making

source("03-lacounty_covid_analysis_data_download_v11.R")

test_sum

# if things look ok at this point, save the tables to disk ----

write_rds(dph_count_data, "./data/dph_count_data.rds")
write_rds(dph_deaths_data, "./data/dph_deaths_data.rds")
write_rds(dph_rate_data, "./data/dph_rate_data.rds")
write_rds(dph_deaths_rate_data, "./data/dph_deaths_rate_data.rds")

write_csv(dph_count_data, file = str_c("../other_data/tabulated daily report data/", "dph_covid_counts.csv"))
write_csv(dph_deaths_data, file = str_c("../other_data/tabulated daily report data/", "dph_covid_deaths.csv"))
write_csv(dph_rate_data, file = str_c("../other_data/tabulated daily report data/", "dph_covid_count_rates.csv"))
write_csv(dph_deaths_rate_data, file = str_c("../other_data/tabulated daily report data/", "dph_covid_death_rates.csv"))


# convert data to long ----

dph_data_a <- dph_count_data %>% pivot_longer(-city_community, names_to = "date", values_to = "total_confirmed_cases")

dph_data_b <- dph_rate_data %>% pivot_longer(-city_community, names_to = "date", values_to = "total_confirmed_cases_rate")

dph_data <- left_join(dph_data_a, dph_data_b)

dph_data <- dph_data %>%
   dplyr::filter(city_community != "Los Angeles")

# I don't know why, because it seems fine, but this table isn't actually used for anything.
# there's a convoluted loop process below that gets the same result as these few simple commands.


# dph_data %>% filter(city_community != "Los Angeles") %>%
#    ggplot(mapping = aes(x = date, y = total_confirmed_cases, color = city_community)) +
#    geom_point(show.legend = F)

# dph_data %>% filter(city_community != "Los Angeles") %>%
#    ggplot(mapping = aes(x = date, y = total_confirmed_cases_rate, color = city_community)) +
#    geom_point(show.legend = F)


dph_count_data_long <- NULL

for (i in 2:ncol(dph_count_data)) {
   these_counts <- dph_count_data[, c(1, i)]
   this_date <- colnames(these_counts)[2]
   these_counts <- these_counts %>%
      setNames(c("city_community", "count"))
   these_data <- these_counts %>%
      mutate(date = this_date)
   dph_count_data_long <- bind_rows(dph_count_data_long, these_data)
}

dph_count_data_long <- dph_count_data_long %>%
   dplyr::filter(city_community != "Los Angeles")

# dph_count_data_long %>%
#    ggplot(mapping = aes(x = date, y = count, color = city_community)) +
#    geom_point(show.legend = F)


dph_rate_data_long <- NULL

for (i in 2:ncol(dph_rate_data)) {
   these_rates <- dph_rate_data[, c(1, i)]
   this_date <- colnames(these_rates)[2]
   these_rates <- these_rates %>%
      setNames(c("city_community", "rate"))
   these_rate_data <- these_rates %>%
      mutate(date = this_date)
   dph_rate_data_long <- bind_rows(dph_rate_data_long, these_rate_data)
}

dph_rate_data_long <- dph_rate_data_long %>%
   dplyr::filter(city_community != "Los Angeles")

# dph_rate_data_long %>%
#    ggplot(mapping = aes(x = date, y = rate, color = city_community)) +
#    geom_point(show.legend = F)


dph_data_long = left_join(dph_count_data_long, dph_rate_data_long)

lac_covid_long_csa = left_join(csa_map_simplified, dph_data_long, by = c("label" = "city_community"))
lac_covid_long_csa <- lac_covid_long_csa %>% filter(!is.na(date))


# THIS VERSION OF THE SCRIPT NO LONGER SUPPORTS ANIMATIONS ----
# You need v8 of this script for animations, but the heatmap scales really need to be fixed


# focus on Long Beach ----

long_beach <- dph_count_data_long %>%
   dplyr::filter(city_community == "City of Long Beach") %>%
   dplyr::arrange(date) %>%
   mutate(count_lag_1 = lag(count, 1),
          new_cases = count - count_lag_1) %>%
   mutate(new_cases_lag_1 = lag(new_cases, 1),
          new_cases_lag_2 = lag(new_cases, 2),
          new_cases_lag_3 = lag(new_cases, 3),
          new_cases_lag_4 = lag(new_cases, 4),
          new_cases_lag_5 = lag(new_cases, 5),
          new_cases_lag_6 = lag(new_cases, 6)) %>%
   rowwise() %>%
   mutate(ma_7_day = mean(c(new_cases,
                            new_cases_lag_1,
                            new_cases_lag_2,
                            new_cases_lag_3,
                            new_cases_lag_4,
                            new_cases_lag_5,
                            new_cases_lag_6), na.rm = T)) %>%
   mutate(date = as_date(date))

ggplot(data = long_beach) +
   geom_col(mapping = aes(x = date, y = new_cases), fill = "#0099CC", show.legend = F) +
   geom_line(mapping  = aes(x = date, y = ma_7_day, group = 1), color = "#990000") +
   scale_x_date(name = "Date", date_breaks = "4 weeks") +
   scale_y_continuous(name = "New cases (calculated from cumulative counts)") +
   labs(title = "Daily new case counts with 7-day moving average, Long Beach")

ggsave(filename = str_c("../covid_analysis_output/geo_analysis_output/output_main/", "long_beach_cases-", today(), ".png"), width = 8, height = 5, units = "in", dpi = 150)

# focus on Pasadena ----

pasadena <- dph_count_data_long %>%
   dplyr::filter(city_community == "City of Pasadena") %>%
   dplyr::arrange(date) %>%
   mutate(count_lag_1 = lag(count, 1),
          new_cases = count - count_lag_1) %>%
   mutate(new_cases_lag_1 = lag(new_cases, 1),
          new_cases_lag_2 = lag(new_cases, 2),
          new_cases_lag_3 = lag(new_cases, 3),
          new_cases_lag_4 = lag(new_cases, 4),
          new_cases_lag_5 = lag(new_cases, 5),
          new_cases_lag_6 = lag(new_cases, 6)) %>%
   rowwise() %>%
   mutate(ma_7_day = mean(c(new_cases,
                            new_cases_lag_1,
                            new_cases_lag_2,
                            new_cases_lag_3,
                            new_cases_lag_4,
                            new_cases_lag_5,
                            new_cases_lag_6), na.rm = T)) %>%
   mutate(date = as_date(date))

ggplot(data = pasadena) +
   geom_col(mapping = aes(x = date, y = new_cases), fill = "#00CC33", show.legend = F) +
   geom_line(mapping  = aes(x = date, y = ma_7_day, group = 1), color = "#990000") +
   scale_x_date(name = "Date", date_breaks = "4 weeks") +
   scale_y_continuous(name = "New cases (calculated from cumulative counts)") +
   labs(title = "Daily new case counts with 7-day moving average, Pasadena")

ggsave(filename = str_c("../covid_analysis_output/geo_analysis_output/output_main/", "pasadena_cases-", today(), ".png"), width = 8, height = 5, units = "in", dpi = 150)


# FOCUS ON POMONA ----

pomona <- dph_count_data_long %>%
   dplyr::filter(city_community == "City of Pomona") %>%
   dplyr::arrange(date) %>%
   mutate(count_lag_1 = lag(count, 1),
          new_cases = count - count_lag_1) %>%
   mutate(new_cases_lag_1 = lag(new_cases, 1),
          new_cases_lag_2 = lag(new_cases, 2),
          new_cases_lag_3 = lag(new_cases, 3),
          new_cases_lag_4 = lag(new_cases, 4),
          new_cases_lag_5 = lag(new_cases, 5),
          new_cases_lag_6 = lag(new_cases, 6)) %>%
   rowwise() %>%
   mutate(ma_7_day = mean(c(new_cases,
                            new_cases_lag_1,
                            new_cases_lag_2,
                            new_cases_lag_3,
                            new_cases_lag_4,
                            new_cases_lag_5,
                            new_cases_lag_6), na.rm = T)) %>%
   mutate(date = as_date(date))

ggplot(data = pomona) +
   geom_col(mapping = aes(x = date, y = new_cases), fill = "#0099CC", show.legend = F) +
   geom_line(mapping  = aes(x = date, y = ma_7_day, group = 1), color = "#990000") +
   scale_x_date(name = "Date", date_breaks = "4 weeks") +
   scale_y_continuous(name = "New cases (calculated from cumulative counts)") +
   labs(title = "Daily new case counts with 7-day moving average, Pomona")

ggsave(filename = str_c("../covid_analysis_output/geo_analysis_output/output_main/", "pomona_cases-", today(), ".png"), width = 8, height = 5, units = "in", dpi = 150)


# district graph ----

source("04-lacounty_covid_analysis_supdist_charts.R")


# and finally, make the map ----

# this is simplified setup for the new (as of Sep 2020) dashboard map;
# full map code picks up around line 234

n_cols_rate <- ncol(dph_rate_data)

latest_rates <- dph_rate_data[, c(1, n_cols_rate)] %>% dplyr::filter(city_community != "Los Angeles")
latest_date <- colnames(latest_rates)[2]

latest_rates <- latest_rates %>%
   setNames(c("city_community", "rate"))

csa_map_counts <- left_join(csa_map_simplified, latest_rates, by = c("label" = "city_community"))
csa_map_counts <- csa_map_counts %>%
   mutate(rate = ifelse(is.na(rate), 0, rate))

dashboard_basemap_rates <-
   tm_shape(csa_map_counts) +
   tm_fill(col = "rate",
           style = "fixed",
           id = "label",
           breaks = rate_quintiles_csa,  # a la DPH dashboard; quintiles
           title = "Case Rate (per 100k)",
           group = str_c("COVID-19 case rate, ", latest_date)) +
   tm_borders(lwd = 0.2)


# quantiles rate map for dashboard ----
map_styling <- tm_layout(
   #main.title = str_c("COVID-19 in LA County"),
   main.title.size = 1.25,
   legend.position = c("left", "bottom"),
   legend.frame = TRUE,
   legend.title.size = 0.8,
   legend.text.size = 0.6
)

the_rate_map_binary_dashboard <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   dashboard_basemap_rates +
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
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County (cumulative), ", today()), main.title.size = 1.1)

static_rate_map_binary_dashboard <- la_county_topomap + the_rate_map_binary_dashboard + map_styling

tmap_save(static_rate_map_binary_dashboard, filename = str_c(paste0("../covid_analysis_output/dashboard_output/maps/rates_map-", latest_date,".pdf")), width = 6.5, height = NA, units = "in")

# tmap_save(static_rate_map_binary_dashboard, filename = str_c(paste0("../covid_analysis_output/dashboard_output/maps/rates_map-", latest_date,".png")), width = 6.5, height = NA, units = "in")


# smooth gradient case rate heatmap ----

csa_map_counts_limited <- csa_map_counts %>%
   mutate(rate_limited = case_when(rate > 20000 ~ 20000,
                                   T ~ rate))

dashboard_basemap_rates_smooth <-
   tm_shape(csa_map_counts_limited) +
   tm_fill(col = "rate_limited",
           style = "cont",
           id = "label",
           title = "Case Rate (per 100k)",
           group = str_c("COVID-19 cumulative case rate, ", latest_date)) +
   tm_borders(lwd = 0.2)

the_rate_map_binary_dashboard_smooth <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   dashboard_basemap_rates_smooth +
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
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County (cumulative), ", today()), main.title.size = 1.1)

the_rate_map_binary_dashboard_smooth

static_rate_map_binary_dashboard_smooth <- la_county_topomap + the_rate_map_binary_dashboard_smooth + map_styling

tmap_save(static_rate_map_binary_dashboard_smooth, filename = str_c(paste0("../covid_analysis_output/dashboard_output/other_maps/rates_map_smooth-", latest_date,".pdf")), width = 6.5, height = NA, units = "in")


# 7-day total smooth gradient case rate heatmap ----

# REQUIRES that lacounty_supdist_charts.R has already been run!!

dph_7day_rate_csa_sd_grp_rank

data_date_7day_map <- as.character(dph_7day_rate_csa_sd_grp_rank$date[1])

lac_covid_long_csa_7day = left_join(csa_map_simplified, dph_7day_rate_csa_sd_grp_rank, by = c("label" = "city_community"))
lac_covid_long_csa_7day <- lac_covid_long_csa_7day %>% filter(!is.na(date))

# UPPER_LIMIT_THRESHOLD reduced from 1500 to 1000 on 2020-02-02 to reflect lower case rates countywide
# UPPER_LIMIT_THRESHOLD reduced from 1000 to 350 on 2020-02-16 to reflect lower case rates countywide
UPPER_LIMIT_THRESHOLD <- 80

lac_covid_long_csa_7day <- lac_covid_long_csa_7day %>%
   mutate(case_7day_rate = ifelse(is.na(case_7day_rate), 0, case_7day_rate)) %>%
   mutate(case_7day_rate = ifelse(case_7day_rate > UPPER_LIMIT_THRESHOLD, UPPER_LIMIT_THRESHOLD, case_7day_rate))

dashboard_basemap_rates_7day_smooth <-
   tm_shape(lac_covid_long_csa_7day) +
   tm_fill(col = "case_7day_rate",
           style = "cont",
           id = "label",
           title = "7-day Case Rate (per 100k)",
           group = str_c("COVID-19 7-day case rate, ", latest_date)) +
   tm_borders(lwd = 0.2)

rate_map_7day_smooth <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   dashboard_basemap_rates_7day_smooth +
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
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County (7-day total), ", data_date_7day_map), main.title.size = 1.1)

static_rate_map_7day_smooth <- la_county_topomap + rate_map_7day_smooth + map_styling

static_rate_map_7day_smooth

tmap_save(static_rate_map_7day_smooth, filename = str_c(paste0("../covid_analysis_output/dashboard_output/other_maps/rates_map_7day_smooth-", data_date_7day_map,".pdf")), width = 6.5, height = NA, units = "in")

# interactive
tmap_leaflet(
   rate_map_7day_smooth,
   mode = "view"
)


# =================================================================================
# THIS IS HOW FAR YOU NEED TO GO TO HAVE THE MAPS FOR THE NEW DASHBOARD REPORT ----
# =================================================================================

# PRE-DASHBOARD FUNCTIONALITY IS IN V9 OF THIS SCRIPT (in the "old R code" folder)


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

Sys.Date()
