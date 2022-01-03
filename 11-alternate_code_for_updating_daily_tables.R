
# alternative data if the web table hasn't been updated but the dashboard has and you're impatient
folders_there <- list.files("../daily_dashboard/data") %>%
   purrr::discard(str_detect(., "readme"))
datatable_to_use <- last(folders_there)
case_death_table_csa_all <- readr::read_csv(paste("../daily_dashboard/data", datatable_to_use, "LA_County_Covid19_CSA_case_death_table.csv", sep = "/"), show_col_types = F) %>%
   dplyr::select(geo_merge, cases_final, case_rate_final, deaths_final, death_rate_final)
new_col_name <- as.character(as_date(datatable_to_use) + 1)

dph_count_data
case_death_table_csa_all %>%
   dplyr::select(geo_merge, cases_final) %>%
   rename(!!new_col_name := cases_final) %>%
   left_join(dph_count_data, ., by = c("city_community" = "geo_merge"))

dph_deaths_data
case_death_table_csa_all %>%
   dplyr::select(geo_merge, deaths_final) %>%
   rename(!!new_col_name := deaths_final) %>%
   left_join(dph_deaths_data, ., by = c("city_community" = "geo_merge"))

dph_rate_data
case_death_table_csa_all %>%
   dplyr::select(geo_merge, case_rate_final) %>%
   rename(!!new_col_name := case_rate_final) %>%
   left_join(dph_rate_data, ., by = c("city_community" = "geo_merge"))

dph_deaths_rate_data
case_death_table_csa_all %>%
   dplyr::select(geo_merge, death_rate_final) %>%
   rename(!!new_col_name := death_rate_final) %>%
   left_join(dph_deaths_rate_data, ., by = c("city_community" = "geo_merge"))

