library(ggrepel)


supdist_labels <- tribble(
   ~supdist, ~supdistlabel, ~supdistlabel_rep,
   #——————--|———————-------|-----------------
   1, "District 1", "District 1 - H. Solis",
   2, "District 2", "District 2 - H. Mitchell",
   3, "District 3", "District 3 - S. Kuehl",
   4, "District 4", "District 4 - J. Hahn",
   5, "District 5", "District 5 - K. Barger"
)

csa_by_supdist_pops <- csa_by_supdist_map_pops %>%
   st_drop_geometry() %>%
   dplyr::select(label, supdist, area_pct, population, split_pop)

# case counts ----

dph_count_data_by_csa_supdist <- left_join(dph_count_data, csa_by_supdist_pops, by = c("city_community" = "label"))

setdiff(dph_count_data$city_community, csa_by_supdist_pops$label)

vars <- names(dph_count_data_by_csa_supdist)
vars <- vars[ vars != "city_community" ]
vars <- vars[ vars != "supdist" ]
vars <- vars[ vars != "supdistlabel" ]
vars <- vars[ vars != "supdistlabel_rep" ]
vars <- vars[ vars != "area_pct" ]
vars <- vars[ vars != "population" ]


dph_count_data_by_supdist <- dph_count_data_by_csa_supdist %>%
   mutate_at(.vars = vars(!c(city_community,supdist:split_pop)),
          .funs = list(~ (. * area_pct))) %>%
   dplyr::select(-area_pct, -population) %>%
   group_by(supdist) %>%
   summarize_at(vars, sum, na.rm = T) %>%
   dplyr::filter(!is.na(supdist))

dph_count_data_by_supdist

supdist_counts <- dph_count_data_by_supdist %>%
   dplyr::select(-split_pop) %>%
   pivot_longer(-supdist, names_to = "date", values_to = "total_confirmed_cases")

supdist_case_counts <- left_join(supdist_counts, supdist_labels) %>%
   mutate(supdistlabel = as_factor(supdistlabel)) %>%
   dplyr::filter(date != "2020-07-03") %>%
   dplyr::filter(date != "2020-07-04") %>%
   mutate(date = as_date(date))

supdist_case_counts %>% ggplot() +
   geom_line(mapping = aes(x = date, y = total_confirmed_cases, color = supdistlabel)) +
   scale_color_muted() +
   scale_x_date(name = "Date", date_breaks = "4 weeks") +
   scale_y_continuous(name = "Confirmed cases (aggregated from DPH community data)") +
   labs(title = "Total Confirmed Covid-19 Cases") +
   theme(legend.title = element_blank(),
         legend.position = "bottom")

ggsave(filename = str_c("../covid_analysis_output/geo_analysis_output/output_main/", "cases_by_district-", today(), ".png"), width = 8, height = 5, units = "in", dpi = 150)

supdist_case_counts %>%
   filter(date == as.Date("2020-07-01"))



# CUMULATIVE case rates ----

as_of_date <- names(dph_rate_data)[ncol(dph_rate_data)]

dph_rate_data_csa_sd_grp_rank <- dph_rate_data %>%
   dplyr::select(c(1, ncol(dph_rate_data))) %>%
   rename("case_rate" = 2) %>%
   left_join(csa_by_supdist_pops, by = c("city_community" = "label")) %>%
   left_join(supdist_labels, by = c("supdist" = "supdist")) %>%
   dplyr::filter(area_pct > 0.5) %>%
   mutate(add_label = 0) %>%
   group_by(supdist) %>%
   dplyr::arrange(supdist, desc(case_rate)) %>%
   #mutate(rank = rank(desc(case_rate), ties.method = "first"))
   mutate(rank = percent_rank(desc(case_rate)))

#dots_to_label <- c("City of Pomona")

dph_rate_data_csa_sd_grp_rank <- dph_rate_data_csa_sd_grp_rank %>%
   mutate(dotlabel = case_when(city_community == "City of Compton" ~ "Compton",
                               city_community == "City of Pomona" ~ "Pomona",
                               city_community == "City of Downey" ~ "Downey",
                               city_community == "City of El Monte" ~ "El Monte",
                               city_community == "City of Palmdale" ~ "Palmdale",
                               city_community == "City of Lancaster" ~ "Lancaster",
                               city_community == "City of Monrovia" ~ "Monrovia",
                               city_community == "City of Santa Monica" ~ "Santa Monica",
                               city_community == "City of Torrance" ~ "Torrance",
                               city_community == "City of Long Beach" ~ "Long Beach",
                               city_community == "City of Pasadena" ~ "Pasadena",
                               city_community == "Unincorporated - East Los Angeles" ~ "East Los Angeles",
                               city_community == "Los Angeles - Echo Park" ~ "Echo Park",
                               city_community == "Los Angeles - North Hollywood" ~ "North Hollywood",
                               city_community == "Los Angeles - Hollywood Hills" ~ "Hollywood Hills",
                               city_community == "City of Inglewood" ~ "Inglewood",
                               city_community == "Los Angeles - Koreatown" ~ "Koreatown",
                               city_community == "Los Angeles - Van Nuys" ~ "Van Nuys",
                               city_community == "Los Angeles - Wholesale District" ~ "Wholesale District",
                               city_community == "Unincorporated - Castaic" ~ "Castaic",
                               city_community == "City of Arcadia" ~ "Arcadia",
                               city_community == "City of Monterey Park" ~ "Monterey Park",
                               city_community == "City of Culver City" ~ "Culver City",
                               city_community == "Los Angeles - Pacific Palisades" ~ "Pacific Palisades",
                               city_community == "Los Angeles - Pacoima" ~ "Pacoima",
                               city_community == "Los Angeles - Sylmar" ~ "Sylmar",
                               city_community == "City of Lynwood" ~ "Lynwood",
                                T ~ NA_character_),
          hjust_amt = case_when(!is.na(dotlabel) & rank <= 50 ~ 0,
                                !is.na(dotlabel) & rank > 50 ~ 1,
                                T ~ NA_real_),
          nudge_x_amt = case_when(!is.na(dotlabel) & rank <= 50 ~ rank + 10,
                                  !is.na(dotlabel) & rank > 50 ~ rank - 10,
                                  T ~ NA_real_)) %>%
   dplyr::filter(!is.na(supdist))

ggplot(data = dph_rate_data_csa_sd_grp_rank,
       mapping = aes(x = rank, y = case_rate, label = dotlabel)) +
   geom_point(mapping = aes(size = population,
                            fill = as_factor(supdist)),
              colour = ifelse(is.na(dph_rate_data_csa_sd_grp_rank$dotlabel),"White", "Black"),
              alpha = 0.75,
              shape = 21) +
   scale_y_continuous(name = "Case rate (per 100k residents, cumulative, unadjusted)",
                      limits = c(0, 25000)) +
   scale_x_continuous(name = "Rank of case rate",
                      labels = NULL,
                      breaks = scales::breaks_extended(n = 4)) +
   geom_text_repel(data = subset(dph_rate_data_csa_sd_grp_rank, rank <= 0.5),
                   hjust = 0,
                   nudge_x = 0.2,
                   size = 2.5) +
   geom_text_repel(data = subset(dph_rate_data_csa_sd_grp_rank, rank > 0.5),
                   hjust = 1,
                   nudge_x = -0.2,
                   size = 2.5) +
   facet_wrap(~ supdistlabel_rep,
              nrow = 1,
              scales = "free_x") +
   theme_bw() +
   theme(legend.position = "none") +
   labs(title = paste("How case rates compare across LA County cities/communities, as of", as_of_date),
        subtitle = "Grouped by primary Supervisorial District; circle size represents city/community population estimate",
        caption = "(Several small communities with outlier case rates are not shown on this chart.)")

ggsave(filename = str_c("../covid_analysis_output/geo_analysis_output/output_main/", "csa_by_district_cumulative_case_rates-", today(), ".png"), width = 10, height = 5, units = "in", dpi = 150)



# 7-DAY TOTAL NEW cases rates ----

# I sadly and embarassingly just figured out that the table used for this analysis does NOT report the 7-day average case rate,
# It reports what it says it reports the 7-day case rate, i.e., the 7-day *total* case rate.
# Column "cases_7day" gives the total number of cases in a community for the 7-day period ending on a given row's "ep_date".
# Column "cases_7day_rate" is that total number divded by that community's population times 100k.
# It's the total case rate for the seven-days ending on that row's ep_date.

folders_there <- list.files("../daily_dashboard/data") %>%
   purrr::discard(str_detect(., "readme"))

# USE THE LATEST OR SPECIFY ONE OF THE PAST DOWNLOADS
datatable_to_use <- last(folders_there)
# datatable_to_use <- "2020-12-05"

wk_tot_case_rate_csa_all <- readr::read_csv(paste("../daily_dashboard/data", datatable_to_use, "LA_County_Covid19_CSA_7day_case_death_table.csv", sep = "/")) %>%
   dplyr::select(ep_date, geo_merge, case_7day_rate) %>%
   mutate(ep_date = as_date(ep_date))


# USE THE LATEST OR SPECIFY A DATE IN THE PAST
date_to_plot <- last(folders_there)
# date_to_plot <- "2020-12-11"

wk_tot_case_rate_csa <- wk_tot_case_rate_csa_all %>%
   dplyr::filter(ep_date == ifelse(ep_date[1] < as_date(date_to_plot), ep_date[1], as_date(date_to_plot))) %>%
   dplyr::select(city_community = geo_merge,
                 date = ep_date,
                 case_7day_rate)



dph_7day_rate_csa_sd_grp_rank <- wk_tot_case_rate_csa %>%
   left_join(csa_by_supdist_pops, by = c("city_community" = "label")) %>%
   left_join(supdist_labels, by = c("supdist" = "supdist")) %>%
   dplyr::filter(area_pct > 0.5) %>%
   mutate(add_label = 0) %>%
   group_by(supdist) %>%
   dplyr::arrange(supdist, desc(case_7day_rate)) %>%
   #mutate(rank = rank(desc(case_7day_rate), ties.method = "first"))
   mutate(rank = percent_rank(desc(case_7day_rate)))

dph_7day_rate_csa_sd_grp_rank <- dph_7day_rate_csa_sd_grp_rank %>%
   mutate(dotlabel = case_when(city_community == "City of Compton" ~ "Compton",
                               city_community == "City of Pomona" ~ "Pomona",
                               city_community == "City of Downey" ~ "Downey",
                               city_community == "City of El Monte" ~ "El Monte",
                               city_community == "City of Palmdale" ~ "Palmdale",
                               city_community == "City of Lancaster" ~ "Lancaster",
                               city_community == "City of Monrovia" ~ "Monrovia",
                               city_community == "City of Santa Monica" ~ "Santa Monica",
                               city_community == "City of Torrance" ~ "Torrance",
                               city_community == "City of Long Beach" ~ "Long Beach",
                               city_community == "City of Pasadena" ~ "Pasadena",
                               city_community == "Unincorporated - East Los Angeles" ~ "East Los Angeles",
                               city_community == "Los Angeles - Echo Park" ~ "Echo Park",
                               city_community == "Los Angeles - North Hollywood" ~ "North Hollywood",
                               city_community == "Los Angeles - Hollywood Hills" ~ "Hollywood Hills",
                               city_community == "City of Inglewood" ~ "Inglewood",
                               city_community == "Los Angeles - Koreatown" ~ "Koreatown",
                               city_community == "Los Angeles - Van Nuys" ~ "Van Nuys",
                               city_community == "Los Angeles - Wholesale District" ~ "Wholesale District",
                               city_community == "Unincorporated - Castaic" ~ "Castaic",
                               city_community == "City of Arcadia" ~ "Arcadia",
                               city_community == "City of Monterey Park" ~ "Monterey Park",
                               city_community == "City of Culver City" ~ "Culver City",
                               city_community == "Los Angeles - Pacific Palisades" ~ "Pacific Palisades",
                               city_community == "Los Angeles - Pacoima" ~ "Pacoima",
                               city_community == "Los Angeles - Sylmar" ~ "Sylmar",
                               city_community == "City of Lynwood" ~ "Lynwood",
                               # added for the moms
                               city_community == "City of West Hollywood" ~ "West Hollywood",
                               city_community == "City of Beverly Hills" ~ "Beverly Hills",
                               city_community == "Los Angeles - Hancock Park" ~ "Hancock Park",
                               T ~ NA_character_)) %>%
   dplyr::filter(!is.na(supdist))

sort(dph_7day_rate_csa_sd_grp_rank$case_7day_rate)[312]

ggplot(data = dph_7day_rate_csa_sd_grp_rank,
       mapping = aes(x = rank, y = case_7day_rate, label = dotlabel)) +
   geom_point(mapping = aes(size = population,
                            fill = as_factor(supdist)),
              colour = ifelse(is.na(dph_7day_rate_csa_sd_grp_rank$dotlabel),"White", "Black"),
              alpha = 0.75,
              shape = 21) +
   scale_y_continuous(name = "Case rate (per 100k, 7-day total, unadjusted)",
                      limits = c(0, 300)) +
   scale_x_continuous(name = "Rank of case rate",
                      labels = NULL,
                      breaks = scales::breaks_extended(n = 4)) +
   geom_text_repel(data = subset(dph_7day_rate_csa_sd_grp_rank, rank <= 0.5),
                   hjust = 0,
                   nudge_x = 0.2,
                   size = 2.5) +
   geom_text_repel(data = subset(dph_7day_rate_csa_sd_grp_rank, rank > 0.5),
                   hjust = 1,
                   nudge_x = -0.2,
                   size = 2.5) +
   facet_wrap(~ supdistlabel_rep,
              nrow = 1,
              scales = "free_x") +
   theme_bw() +
   theme(legend.position = "none") +
   labs(title = paste("How case rates compare across LA County cities/communities, as of", dph_7day_rate_csa_sd_grp_rank$date[1]),
        subtitle = "Grouped by primary Supervisorial District; circle size represents city/community population estimate",
        caption = "(Several small communities with outlier case rates are not shown on this chart.)")

ggsave(filename = str_c("../covid_analysis_output/geo_analysis_output/output_main/", "csa_by_district_7daytot_case_rates-", dph_7day_rate_csa_sd_grp_rank$date[1], ".png"), width = 10, height = 5, units = "in", dpi = 150)


# change in 7-day case rates ----

folders_there

case_rate_7day_now <- readr::read_csv(paste("../daily_dashboard/data", folders_there[length(folders_there)], "LA_County_Covid19_CSA_7day_case_death_table.csv", sep = "/")) %>%
   dplyr::select(ep_date, geo_merge, case_7day_rate) %>%
   mutate(ep_date = as_date(ep_date)) %>%
   dplyr::filter(ep_date == ep_date[1]) %>%
   dplyr::select(city_community = geo_merge,
                 latest_rate = case_7day_rate)

case_rate_7day_weekago <- readr::read_csv(paste("../daily_dashboard/data", folders_there[length(folders_there) - 8], "LA_County_Covid19_CSA_7day_case_death_table.csv", sep = "/")) %>%
   dplyr::select(ep_date, geo_merge, case_7day_rate) %>%
   mutate(ep_date = as_date(ep_date)) %>%
   dplyr::filter(ep_date == ep_date[1]) %>%
   dplyr::select(city_community = geo_merge,
                 week_prior_rate = case_7day_rate)

case_rate_7day_change = left_join(case_rate_7day_now, case_rate_7day_weekago) %>%
   mutate(change_in_past_week = latest_rate - week_prior_rate)


# 7-DAY NEW cases ----

folders_there <- list.files("../daily_dashboard/data") %>%
   purrr::discard(str_detect(., "readme"))

# USE THE LATEST OR SPECIFY ONE OF THE PAST DOWNLOADS
datatable_to_use <- last(folders_there)
# datatable_to_use <- "2020-12-05"

wk_tot_cases_csa <- readr::read_csv(paste("../daily_dashboard/data", datatable_to_use, "LA_County_Covid19_CSA_7day_case_death_table.csv", sep = "/")) %>%
   dplyr::select(ep_date, geo_merge, cases_7day)

csa_by_supdist <- csa_by_supdist_map_pops %>%
   st_drop_geometry() %>%
   dplyr::select(label, supdist, area_pct, population)

supdist_pops <- csa_by_supdist %>%
   mutate(population_allocated = population * area_pct) %>%
   group_by(supdist) %>%
   summarize(population_supdist = sum(population_allocated, na.rm = T))

wk_rate_cases_csa <- left_join(wk_tot_cases_csa, csa_by_supdist, by = c("geo_merge" = "label")) %>%
   dplyr::filter(geo_merge != "Los Angeles County") %>%
   mutate(cases_7day_allocated = cases_7day * area_pct) %>%
   group_by(ep_date, supdist) %>%
   summarize(cases_7day_supdist = sum(cases_7day_allocated)) %>%
   left_join(supdist_pops) %>%
   mutate(cases_7day_rate_supdist = cases_7day_supdist * 100000 / population_supdist) %>%
   left_join(supdist_labels)

# USE THE LATEST OR SPECIFY A DATE IN THE PAST
# date_to_plot <- last(folders_there))
date_to_plot <- max(wk_rate_cases_csa$ep_date) - 3

wk_rate_cases_csa %>%
   dplyr::filter(ep_date <= date_to_plot) %>%
   ggplot() +
   geom_line(mapping = aes(x = ep_date, y = cases_7day_rate_supdist, color = supdistlabel)) +
   scale_color_muted() +
   scale_x_date(name = "Date", date_breaks = "4 weeks", date_labels = "%m-%d") +
   scale_y_continuous(name = "Confirmed 7-day case rate (per 100,000") +
   labs(title = "Confirmed Covid-19 Cases, 7-day rate") +
   theme(legend.title = element_blank(),
         legend.position = "bottom")

ggsave(filename = str_c("../covid_analysis_output/geo_analysis_output/output_main/", "case_rate_history_by_district-", today(), ".png"), width = 8, height = 5, units = "in", dpi = 150)
