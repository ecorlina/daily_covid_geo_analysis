
schldist_map <- read_sf("../data/School_District_Boundaries - FIXED")

schldist_map$LABEL[duplicated(schldist_map$LABEL)]

schldist_map <- schldist_map %>%
   dplyr::arrange(LABEL)

tm_shape(csa_by_schldist_map_pops) +
   tm_polygons()

today_counts_csa
today_rates_csa
today_deaths_csa
today_deaths_rates_csa


# trying to examine local case rates...

today_counts_csa

schldist_counts_map <- left_join(csa_by_schldist_map_pops, today_counts_csa, by = c("label" = "city_community"))

schldist_counts_map <- schldist_counts_map %>%
   mutate(split_count = area_pct * count)

schldist_counts_pre <- schldist_counts_map %>%
   st_drop_geometry() %>%
   dplyr::select(label, LABEL_2, area_pct, population, split_pop, count, split_count)

vars <- names(schldist_counts_pre)
vars <- vars[ vars != "label" ]
vars <- vars[ vars != "LABEL_2" ]
vars <- vars[ vars != "area_pct" ]
vars <- vars[ vars != "population" ]
vars <- vars[ vars != "count" ]

schldist_counts <- schldist_counts_pre %>%
   dplyr::select(-area_pct, -population, -count) %>%
   group_by(LABEL_2) %>%
   summarize_at(vars, sum, na.rm = T) %>%
   dplyr::filter(!is.na(LABEL_2)) %>%
   mutate(raw_rate_comp = 100000 * split_count / split_pop)


schldist_map_w_counts <- left_join(schldist_map, schldist_counts, by = c("LABEL" = "LABEL_2"))

sum(schldist_map_w_counts$split_pop)
sum(schldist_map_w_counts$split_count)
# THESE NUMBERS ARE COMPLETELY WRONG. NOT EVEN CLOSE. NEED TO RETHINK, RESTART.
# It's because there are (at least) six districts that comprise (at least) two polygons, so when the left join happens, the counts and population numbers for those districts are (at least) doubled. One such district is LAUSD, so that is having a huge impact on the overall info.


schldist_map_w_counts %>%
   tm_shape() +
   tm_borders() +
   tm_fill(col = "raw_rate_comp")
