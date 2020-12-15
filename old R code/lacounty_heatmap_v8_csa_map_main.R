# run the setup code ----
# only need to do this when starting with an empty environment

source("lacounty_heatmap_v8_setup.R")

# run the data import code ----
# first, double-check that dph_count_data and dph_rate_data are up to date through 'yesterday'
# this gets run every day to do final prep for map-making

source("lacounty_heatmap_v8_DPH_data.R")

# if things look ok at this point, save the tables to disk ----

write_rds(dph_count_data, "dph_count_data.rds")
write_rds(dph_deaths_data, "dph_deaths_data.rds")
write_rds(dph_rate_data, "dph_rate_data.rds")
write_rds(dph_deaths_rate_data, "dph_deaths_rate_data.rds")
write_rds(dph_shelter_tbl, "dph_shelter_tbl.rds")

write_csv(dph_count_data, path = str_c("./data/tabulated daily report data/", "dph_covid_counts.csv"))
write_csv(dph_deaths_data, path = str_c("./data/tabulated daily report data/", "dph_covid_deaths.csv"))
write_csv(dph_rate_data, path = str_c("./data/tabulated daily report data/", "dph_covid_count_rates.csv"))
write_csv(dph_deaths_rate_data, path = str_c("./data/tabulated daily report data/", "dph_covid_death_rates.csv"))


# and finally, make the maps ----

# tm_shape(csa_map) + tm_polygons()
# csa_map <- simplify_shape(csa_map, sys = T)
# tm_shape(csa_map) + tm_polygons()


# make all the basemaps ----

for (i in 2:ncol(dph_count_data)) {
   these_counts <- dph_count_data[, c(1, i)] %>% dplyr::filter(city_community != "Los Angeles")
   these_rates <- dph_rate_data[, c(1, i)] %>% dplyr::filter(city_community != "Los Angeles")
   this_date <- colnames(these_counts)[2]
   these_counts <- these_counts %>%
      setNames(c("city_community", "count"))
   these_rates <- these_rates %>%
      setNames(c("city_community", "rate"))
   csa_map_counts <- left_join(csa_map, these_counts, by = c("label" = "city_community"))
   csa_map_counts <- left_join(csa_map_counts, these_rates, by = c("label" = "city_community"))
   csa_map_counts <- csa_map_counts %>%
      mutate(count = ifelse(is.na(count), 0, count),
             rate = ifelse(is.na(rate), 0, rate))
   if (i == ncol(dph_count_data)) {
      basemap_counts <- 
         tm_shape(csa_map_counts) +
         tm_fill(col = "count",
                 style = "fixed",
                 id = "label",
                 #breaks = c(0, seq(200, 2800, by = 200)),  # mine; natural
                 #breaks = count_quintiles_csa,  # a la DPH dashboard; quintiles
                 breaks = count_deciles_csa,  # a la DPH dashboard; deciles
                 title = "Confirmed Cases",
                 group = str_c("COVID-19 case count")) +
         tm_borders(lwd = 0.2)
      basemap_rates <- 
         tm_shape(csa_map_counts) +
         tm_fill(col = "rate",
                 style = "fixed",
                 id = "label",
                 #breaks = c(0, 10, 32, 100, 316, 1000, 3162, 10000),  # mine; 10^(0.5x)
                 #breaks = c(0, 422, 935, 1914, 3226, 6326),  # whatever DPH is using on their dashboard today
                 breaks = rate_quintiles_csa,  # a la DPH dashboard; quintiles
                 title = "Confirmed Case Rates (per 100k)",
                 group = str_c("COVID-19 case rate, ", this_date)) +
         tm_borders(lwd = 0.2)
   }
   map_layer_counts <-
      tm_shape(csa_map_counts) +
      tm_fill(col = "count",
              style = "fixed",
              id = "label",
              #breaks = c(0, seq(200, 2800, by = 200)),  # mine; natural
              #breaks = count_quintiles_csa,  # a la DPH dashboard; quintiles
              breaks = count_deciles_csa,  # a la DPH dashboard; deciles
              title = "Confirmed Cases",
              group = str_c("COVID-19 case count, ", this_date),
              legend.show = ifelse(i == 2, T, F)) +
      tm_borders(lwd = 0.2)
   assign(str_c("basemap_", i-1), map_layer_counts)
   map_layer_rates <-
      tm_shape(csa_map_counts) +
      tm_fill(col = "rate",
              style = "fixed",
              id = "label",
              #breaks = c(0, 10, 32, 100, 316, 1000, 3162, 10000),  # mine; 10^(0.5x)
              breaks = rate_quintiles_csa,  # a la DPH dashboard; quintiles
              title = "Confirmed Case Rates (per 100k)",
              group = str_c("COVID-19 case rate, ", this_date),
              legend.show = ifelse(i == 2, T, F)) +
      tm_borders(lwd = 0.2)
   assign(str_c("basemap_rates_", i-1), map_layer_rates)
}


# # new map: deaths by neighborhood

for (i in 2:ncol(dph_deaths_data)) {
   these_deaths <- dph_deaths_data[, c(1, i)]
   this_date <- colnames(these_deaths)[2]
   these_deaths <- these_deaths %>%
      setNames(c("city_community", "count"))
   csa_map_deaths <- left_join(csa_map, these_deaths, by = c("label" = "city_community"))
   csa_map_deaths <- csa_map_deaths %>%
      mutate(count = ifelse(is.na(count), 0, count))
   map_layer_counts <-
      tm_shape(csa_map_deaths) +
      tm_fill(col = "count",
              style = "fixed",
              id = "label",
              breaks = c(0,1,3,10,32,100,316), # mine, logarithmic
              #breaks = c(0,1,2,4,6,8,13,20,74), # mine, based on deciles
              #breaks = c(0, seq(10, 100, by = 10)),  # mine; natural
              #breaks = deaths_quintiles_csa,  # a la DPH dashboard; quintiles/deciles
              #breaks = deaths_deciles_csa,  # a la DPH dashboard; quintiles/deciles
              title = "Deaths",
              group = str_c("Deaths attributed to COVID-19, ", this_date),
              legend.show = ifelse(i == 2, T, F)) +
      tm_borders(lwd = 0.5)
   assign(str_c("basemap_deaths_", i-1), map_layer_counts)
   if (i == ncol(dph_deaths_data)) {
      basemap_deaths <-
         tm_shape(csa_map_deaths) +
         tm_fill(col = "count",
                 style = "fixed",
                 id = "label",
                 breaks = c(0,1,3,10,32,100,316), # mine, logarithmic
                 #breaks = c(0,1,2,4,6,8,13,20,74), # mine, based on deciles
                 #breaks = c(0, seq(10, 100, by = 10)),  # mine; natural
                 #breaks = deaths_quintiles_csa,  # a la DPH dashboard; quintiles/deciles
                 #breaks = deaths_deciles_csa,  # a la DPH dashboard; quintiles/deciles
                 title = "Deaths",
                 group = str_c("Deaths attributed to COVID-19, ", this_date)) +
         tm_borders(lwd = 0.5)
   }
   if (i - 25 >= ncol(dph_deaths_rate_data)) {
      j = i - 25
      these_deaths_rates <- dph_deaths_rate_data[, c(1, j)]
      these_deaths_rates <- these_deaths_rates %>%
         setNames(c("city_community", "rate"))
      csa_map_deaths <- left_join(csa_map_deaths, these_deaths_rates, by = c("label" = "city_community"))
      csa_map_deaths <- csa_map_deaths %>%
         mutate(rate = ifelse(is.na(rate), 0, rate))
      map_layer_rates <-
         tm_shape(csa_map_deaths) +
         tm_fill(col = "rate",
                 style = "fixed",
                 id = "label",
                 #breaks = c(0, 10, 32, 100, 316, 1000, 3162, 10000),  # mine; 10^(0.5x)
                 breaks = deaths_rate_quintiles_csa,  # a la DPH dashboard; quintiles
                 title = "Deaths Rate (per 100k)",
                 group = str_c("COVID-19 deaths rate, ", this_date),
                 legend.show = ifelse(i == 2, T, F)) +
         tm_borders(lwd = 0.2)
      assign(str_c("basemap_death_rates_", i-1), map_layer_rates)
      if (j == ncol(dph_deaths_rate_data)) {
         basemap_deaths_rate <-
            tm_shape(csa_map_deaths) +
            tm_fill(col = "rate",
                    style = "fixed",
                    id = "label",
                    #breaks = c(0,1,3,10,32,100,316), # mine, logarithmic
                    #breaks = c(0,1,2,4,6,8,13,20,74), # mine, based on deciles
                    #breaks = c(0, seq(10, 100, by = 10)),  # mine; natural
                    breaks = deaths_rate_quintiles_csa,  # a la DPH dashboard; quintiles/deciles
                    #breaks = deaths_deciles_csa,  # a la DPH dashboard; quintiles/deciles
                    title = "Deaths Rate (per 100k)",
                    group = str_c("Deaths attributed to COVID-19, ", this_date)) +
            tm_borders(lwd = 0.2)
      }
   }
}


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
   basemap_29 +
   basemap_57 +
   basemap_85 +
   basemap_114 +
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
   basemap_rates_29 +
   basemap_rates_57 +
   basemap_rates_85 +
   basemap_rates_114 +
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
   basemap_counts +
   basemap_rates +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   # tm_shape(shelter_counts_locations) +
   # tm_symbols(
   #    #shape = "setting_type.x",
   #    shape = 16,
   #    size = "confirmed_residents",
   #    shape.showNA = F,
   #    col = "#ADFF2F",
   #    border.lwd = 0.5, border.col = "black", border.alpha = 0.5,
   #    jitter = 0.075,
   #    id = "setting_name",
   #    popup.vars = c("confirmed_staff", "confirmed_residents", "total_deaths"),
   #    group = "DPH Homeless Cases") +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()), main.title.size = 1.1) +
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
      group = "Residential and shelter (pink)")

# interactive
tmap_leaflet(
   the_count_map_binary_with_rate,
   mode = "view"
   ) %>%
   leaflet::hideGroup("COVID-19 case count") %>%
   leaflet::hideGroup("DPH Homeless Cases")


# rate map with district boundaries ----

the_count_map_binary_rate_sup <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_rates +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_shape(lac_supervisorial) +
   tm_borders(lwd = 2.0,
              col = "#000000")

tmap_leaflet(
   the_count_map_binary_rate_sup,
   mode = "view"
)

static_sup_map <- la_county_topomap + the_count_map_binary_rate_sup + map_styling
tmap_save(static_sup_map, filename = str_c("./maps/lac_covid_rates_sup-", today(), ".pdf"), width = 6.5, height = NA, units = "in")

tmap_save(static_sup_map, filename = str_c("./maps/lac_covid_rates_sup-", today(), ".png"), width = 6.5, height = NA, units = "in")


# deaths map ----

the_deaths_map_binary_by_day <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_deaths_rate +
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
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()-1), main.title.size = 1.1)

# interactive
tmap_leaflet(
   the_deaths_map_binary_by_day,
   mode = "view"
)

# static_deaths_map_binary <- la_county_topomap + the_deaths_map_binary_by_day + map_styling
# tmap_save(static_deaths_map_binary, filename = str_c("./maps/lac_covid_deaths_2cat-", today(), ".pdf"), width = 6.5, height = NA, units = "in")


# animation of spread (without VOALA dots) ----

dph_data_a <- dph_count_data %>%
   pivot_longer(-city_community, names_to = "date", values_to = "total_confirmed_cases")

dph_data_b <- dph_rate_data %>%
   pivot_longer(-city_community, names_to = "date", values_to = "total_confirmed_cases_rate")

dph_data <- left_join(dph_data_a, dph_data_b)

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

lac_covid_long_csa = left_join(csa_map, dph_data_long, by = c("label" = "city_community"))
lac_covid_long_csa <- lac_covid_long_csa %>% filter(!is.na(date))

# JUMP DOWN TO LONG BEACH HERE IF YOU'RE NOT DOING ANIMATIONS TODAY.

hist(lac_covid_long_csa$count)
count_pctiles <- c(0, 0.5, 0.75, 0.90, 0.95, 1.0)
count_breaks <- quantile(lac_covid_long_csa$count, count_pctiles, na.rm = T)
count_breaks
count_labels <- c("0-38", "39-146", "147-388", "389-659", ">659")

# small cases map
lac_cases <-
   la_county_topomap +
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   tm_shape(lac_covid_long_csa) +
   tm_borders(lwd = 0.5) +
   tm_fill(col = "count",
           style = "quantile",
           #style = "fixed",
           #breaks = count_quintiles_csa,
           #breaks = count_breaks,
           #labels = count_labels,
           naturaltitle = "Confirmed Cases") +
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


hist(lac_covid_long_csa$rate)
rate_pctiles <- c(0, 0.50, 0.75, 0.90, 0.95, 1.0)
rate_breaks <- quantile(lac_covid_long_csa$rate, rate_pctiles, na.rm = T)
rate_breaks
rate_labels <- c("0-253", "254-517", "518-950", "951-1279", ">1279")

# small rates map
lac_rates <-
   la_county_topomap +
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   tm_shape(lac_covid_long_csa) +
   tm_borders(lwd = 0.5) +
   tm_fill(col = "rate",
           style = "log10",
           #style = "fixed",
           #breaks = rate_quintiles_csa,
           #breaks = rate_breaks,
           #labels = rate_labels
           ) +
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



 # export gifs ----

# lac_cases

tmap_animation(lac_cases, filename = str_c("./movies/", "lac_cases-", today(), ".gif"), width = 800, height = 620, delay = 50, restart.delay = 75)


# lac_rates

tmap_animation(lac_rates, filename = str_c("./movies/", "lac_rates-", today(), ".gif"), width = 800, height = 620, delay = 50, restart.delay = 75)



# to convert the gif to a useful, working MPEG file!
# update the date in the filenames below!!
# launch Terminal.app
# cd "Dropbox/Rocinante Research/VOALA MBPro/confirmed_cases/movies"
# ffmpeg -i lac_cases-2020-07-12.gif -pix_fmt yuv420p lac_cases-2020-07-12.mp4
# ffmpeg -i lac_rates-2020-07-09.gif -pix_fmt yuv420p lac_rates-2020-07-09.mp4


# if you copy the images into the target folder and rename them in the finder...
# cd "Dropbox/Rocinante Research/VOALA MBPro/confirmed_cases/curves/old counts graphs/case counts"
# ffmpeg -r 4 -i pic%05d.jpg -vcodec libx264 -crf 25 -s 700x510 -pix_fmt yuv420p test.mp4



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
   scale_x_date(name = "Date", date_breaks = "2 weeks") +
   scale_y_continuous(name = "New cases (calculated from cumulative counts)") +
   labs(title = "Daily new case counts with 7-day moving average, Long Beach")

ggsave(filename = str_c("./curves/", "long_beach_cases-", today(), ".png"), width = 8, height = 6, units = "in", dpi = 150)

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
   scale_x_date(name = "Date", date_breaks = "2 weeks") +
   scale_y_continuous(name = "New cases (calculated from cumulative counts)") +
   labs(title = "Daily new case counts with 7-day moving average, Pasadena")

ggsave(filename = str_c("./curves/", "pasadena_cases-", today(), ".png"), width = 8, height = 6, units = "in", dpi = 150)


# district graph ----

source("lacounty_supdist_charts.R")

                                          # disparity maps ----

disparity_map_styling <- tm_layout(
   main.title = str_c("COVID-19 in LA County"),
   main.title.size = 1.75,
   legend.position = c("left", "bottom"),
   legend.frame = TRUE,
   legend.title.size = 1.0,
   legend.text.size = 0.8
)

disparity_map_styling_alt <- tm_layout(
   main.title.size = 1.25,
   legend.position = c("left", "bottom"),
   legend.frame = TRUE,
   legend.title.size = 1.0,
   legend.text.size = 0.8
)


# HH INCOME

local_median_hh_income <- read_xlsx("./disparity/NDSC-median household income.xlsx", sheet = "Employment & Income") %>%
   mutate(`Weighted Average` = as.numeric(`Weighted Average`))

ndsc_map_hh_income <- left_join(ndsc_map, local_median_hh_income, by = c("name" = "Shape Name")) %>%
   rename(`Median Household Income` = `Weighted Average`)

hh_income_quintiles <- ceiling(unname(quantile(
   ndsc_map_hh_income$`Median Household Income`,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))

basemap_hh_income <-
   tm_shape(ndsc_map_hh_income) +
   tm_fill(col = "Median Household Income",
           style = "fixed",
           # breaks = c(0, 65200, 98300, 143600, 250000), # NDSC, source map
           breaks = hh_income_quintiles, # my quintile calc
           palette = "YlGn",
           title = "Median Household Income, 2018",
           group = str_c("Median Household Income, 2018")) +
   tm_borders(lwd = 0.2)


# BLACK POPULATION

local_black_population <- read_xlsx("./disparity/NDSC-black population.xlsx", sheet = "Demography") %>%
   mutate(Percentage = str_remove(Percentage, "%")) %>%
   mutate(Percentage = as.numeric(Percentage))

ndsc_map_black_pop <- left_join(ndsc_map, local_black_population, by = c("name" = "Shape Name")) %>%
   rename(`Percent Population Black` = Percentage)

black_pop_quintiles <- ceiling(unname(quantile(
   ndsc_map_black_pop$`Percent Population Black`,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))

basemap_black_pop <-
   tm_shape(ndsc_map_black_pop) +
   tm_fill(col = "Percent Population Black",
           style = "fixed",
           breaks = c(0, 6, 12, 18, 24, 100), # NDSC, source map
           #breaks = black_pop_quintiles, # my quintile calc
           palette = "BuPu",
           title = "Percent Population Black, 2018",
           group = str_c("Percent Population Black, 2018")) +
   tm_borders(lwd = 0.2)


# HISPANIC POPULATION

local_hispanic_population <- read_xlsx("./disparity/NDSC-hispanic population.xlsx", sheet = "Demography") %>%
   mutate(Percentage = str_remove(Percentage, "%")) %>%
   mutate(Percentage = as.numeric(Percentage))

ndsc_map_hispanic_pop <- left_join(ndsc_map, local_hispanic_population, by = c("name" = "Shape Name")) %>%
   rename(`Percent Population Hispanic` = Percentage)

hispanic_pop_quintiles <- ceiling(unname(quantile(
   ndsc_map_hispanic_pop$`Percent Population Hispanic`,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))

basemap_hispanic_pop <-
   tm_shape(ndsc_map_hispanic_pop) +
   tm_fill(col = "Percent Population Hispanic",
           style = "fixed",
           breaks = c(0, 20, 40, 60, 80, 100), # NDSC, source map
           #breaks = hispanic_pop_quintiles, # my quintile calc
           palette = "PuBu",
           title = "Percent Population Hispanic, 2018",
           group = str_c("Percent Population Hispanic, 2018")) +
   tm_borders(lwd = 0.2)


# basemap_rates
static_sup_map
basemap_hh_income
basemap_black_pop
basemap_hispanic_pop


# ----
# NOTE 2020-10-23 ----
# SOME OF THIS CODE HAS BEEN UPDATED TO HELP SAM AND CAITLIN WITH A MAP
# IT REQUIRES THAT CODE BE RUN IN THE heatmap_v10 FILE AND THE hpi_map FILE
# UNLESS YOU REMOVE NEW LAYER REFERENCES AND RESTORE ORIGINAL LAYER REFERENCES

covid_disparity_map <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   hpi_qt_map +
   basemap_hispanic_pop +
   basemap_black_pop +
   basemap_hh_income +
   the_rate_map_binary_dashboard +
   # tm_shape(lac_supervisorial) +
   # tm_borders(group = "LA County Districts",
   #            lwd = 1.5,
   #            col = "#000000") +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()))

# interactive
tmap_leaflet(
   covid_disparity_map,
   mode = "view"
   ) %>%
   leaflet::hideGroup("Healthy Places Index Quartiles, 2020") %>%
   leaflet::hideGroup("Percent Population Hispanic, 2018") %>%
   leaflet::hideGroup("Percent Population Black, 2018") %>%
   leaflet::hideGroup("Median Household Income, 2018")


# static disparity maps ----

covid_map <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_rates +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_layout(main.title = str_c("COVID-19 in Los Angeles County, ", today()))

disparity_map_static <- la_county_topomap + covid_map + disparity_map_styling_alt

tmap_save(disparity_map_static, filename = str_c("./maps/lac_covid_disparity_rates-", today(), ".png"), width = 6.5, height = NA, units = "in")


hh_inc_map <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_hh_income +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_layout(main.title = str_c("Median Household Income in Los Angeles County, ", "2018"))

hh_inc_map_static <- la_county_topomap + hh_inc_map + disparity_map_styling_alt

tmap_save(hh_inc_map_static, filename = str_c("./maps/lac_med_hh_inc-2018", ".png"), width = 6.5, height = NA, units = "in")


black_pop_map <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_black_pop +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_layout(main.title = str_c("Black Population Rate in Los Angeles County, ", "2018"))

blackpop_map_static <- la_county_topomap + black_pop_map + disparity_map_styling_alt

tmap_save(blackpop_map_static, filename = str_c("./maps/lac_black_population_rates-2018", ".png"), width = 6.5, height = NA, units = "in")


hispanic_pop_map <-
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   basemap_hispanic_pop +
   tm_layout(legend.show = T, legend.frame = T, legend.bg.alpha = 0.85) +
   tm_layout(main.title = str_c("Hispanic Population Rate in Los Angeles County, ", "2018"))

hisppop_map_static <- la_county_topomap + hispanic_pop_map + disparity_map_styling_alt

tmap_save(hisppop_map_static, filename = str_c("./maps/lac_hispanic_population_rates-2018", ".png"), width = 6.5, height = NA, units = "in")


# rates disparity map to match the above static map ----

lac_rates_quintiles <-
   la_county_topomap +
   tm_shape(lacounty_mainland) +
   tm_borders(group = "LA County border") +
   tm_shape(lac_covid_long_geo) +
   tm_borders(lwd = 0.5) +
   tm_fill(col = "rate_comp",
           style = "fixed",
           breaks = rate_quintiles,
           title = "Cases per 100k") +
   tm_layout(scale = 0.5, legend.position = c("right", "top")) +
   tm_facets(along = "date", free.coords = F, free.scales = F, nrow = 1, ncol = 1) +
   tm_layout(main.title.size = 0.6, legend.outside = T)


# lac_rates

tmap_animation(lac_rates_quintiles, filename = str_c("./movies/", "lac_rates_quintiles-", today(), ".gif"), width = 800, height = 620, delay = 75, restart.delay = 75)

# to convert the gif to a useful, working MPEG file!
# update the date in the filenames below!!
# launch Terminal.app
# cd "Dropbox/Rocinante Research/VOALA MBPro/confirmed_cases/movies"
# ffmpeg -i lac_rates_quintiles-2020-06-05.gif -pix_fmt yuv420p lac_rates_quintiles-2020-06-05.mp4



# City of Malibu ----

malibu <- dph_count_data_long %>%
   dplyr::filter(city_community == "City of Malibu") %>%
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

ggplot(data = malibu) +
   geom_col(mapping = aes(x = date, y = new_cases), fill = "#0099CC", show.legend = F) +
   geom_line(mapping  = aes(x = date, y = ma_7_day, group = 1), color = "#990000") +
   scale_x_date(name = "Date", date_breaks = "2 weeks") +
   scale_y_continuous(name = "New cases (calculated from cumulative counts)") +
   labs(title = "Daily new case counts with 7-day moving average, Malibu")

ggsave(filename = str_c("./curves/", "malibu_cases-", today(), ".png"), width = 8, height = 6, units = "in", dpi = 150)


# City of Compton ----

compton <- dph_count_data_long %>%
   dplyr::filter(city_community == "City of Compton") %>%
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

ggplot(data = compton) +
   geom_col(mapping = aes(x = date, y = new_cases), fill = "#0099CC", show.legend = F) +
   geom_line(mapping  = aes(x = date, y = ma_7_day, group = 1), color = "#990000") +
   scale_x_date(name = "Date", date_breaks = "2 weeks") +
   scale_y_continuous(name = "New cases (calculated from cumulative counts)") +
   labs(title = "Daily new case counts with 7-day moving average, Compton")

ggsave(filename = str_c("./curves/", "compton_cases-", today(), ".png"), width = 8, height = 6, units = "in", dpi = 150)

# City of Pico Rivera ----

pico_rivera <- dph_count_data_long %>%
   dplyr::filter(city_community == "City of Pico Rivera") %>%
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

ggplot(data = pico_rivera) +
   geom_col(mapping = aes(x = date, y = new_cases), fill = "#0099CC", show.legend = F) +
   geom_line(mapping  = aes(x = date, y = ma_7_day, group = 1), color = "#990000") +
   scale_x_date(name = "Date", date_breaks = "2 weeks") +
   scale_y_continuous(name = "New cases (calculated from cumulative counts)") +
   labs(title = "Daily new case counts with 7-day moving average, Pico Rivera")

ggsave(filename = str_c("./curves/", "pico_rivera_cases-", today(), ".png"), width = 8, height = 6, units = "in", dpi = 150)
