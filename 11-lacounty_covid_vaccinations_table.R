# import vaccination data by CSA

folders_there <- list.files("../daily_dashboard/data") %>%
   purrr::discard(str_detect(., "readme"))

# USE THE LATEST OR SPECIFY ONE OF THE PAST DOWNLOADS
datatable_to_use <- last(folders_there)
# datatable_to_use <- "2020-12-05"

vaccinations_csa_all <- readr::read_csv(paste("../daily_dashboard/data", datatable_to_use, "LAC_Vaccine_City_Data.csv", sep = "/"), show_col_types = F) %>%
   mutate(Date = as_date(Date, format = "%m/%d/%y"))

# USE THE LATEST OR SPECIFY A DATE IN THE PAST
date_to_plot <- last(folders_there)
# date_to_plot <- "2020-12-11"

vaccinations_csa <- vaccinations_csa_all %>%
   dplyr::filter(Date == ifelse(last(Date) < as_date(date_to_plot), last(Date), as_date(date_to_plot))) %>%
   dplyr::select(city_community = Community,
                 date = Date,
                 five_plus_pct_fully = `Cumulative Percentage 5+ (Fully vaccinated)`) %>%
   mutate(five_plus_pct_fully = str_replace(five_plus_pct_fully, "Unreliable Data", NA_character_)) %>%
   mutate(five_plus_pct_fully = str_replace(five_plus_pct_fully, "%", "")) %>%
   mutate(five_plus_pct_fully = as.numeric(five_plus_pct_fully) / 100)
