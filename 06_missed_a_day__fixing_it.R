dph_count_data <- dph_count_data %>%
   mutate(`2020-11-27` = round(rowMeans(cbind(dph_count_data$`2020-11-26`, dph_count_data$`2020-11-28`)))) %>%
   dplyr::select(sort(current_vars())) %>%
   dplyr::select(city_community, everything())
   


dph_deaths_data <- dph_deaths_data %>%
   mutate(`2020-11-27` = round(rowMeans(cbind(dph_deaths_data$`2020-11-26`, dph_deaths_data$`2020-11-28`)))) %>%
   dplyr::select(sort(current_vars())) %>%
   dplyr::select(city_community, everything())
   


dph_rate_data <- dph_rate_data %>%
   mutate(`2020-11-27` = round(rowMeans(cbind(dph_rate_data$`2020-11-26`, dph_rate_data$`2020-11-28`)))) %>%
   dplyr::select(sort(current_vars())) %>%
   dplyr::select(city_community, everything())
   


dph_deaths_rate_data <- dph_deaths_rate_data %>%
   mutate(`2020-11-27` = round(rowMeans(cbind(dph_deaths_rate_data$`2020-11-26`, dph_deaths_rate_data$`2020-11-28`)))) %>%
   dplyr::select(sort(current_vars())) %>%
   dplyr::select(city_community, everything())
   




# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# THE FOLLOWING CODE IS JUST TO FIX SOME WEIRD MISSING DATA FROM AUGUST 4.
# IT WILL ONLY BE RUN ONCE AND WON'T WORK OR BE USEFUL AGAIN UNLESS A SIMILAR GLITCH HAPPENS.

# cities/communities with missing counts on AUGUST 4
dph_count_data$city_community[is.na(dph_count_data$`2020-08-04`)]

# cities/communities with missing counts on AUGUST 3
dph_count_data$city_community[is.na(dph_count_data$`2020-08-03`)]

# cities/communities with missing counts on AUGUST 5
dph_count_data$city_community[is.na(dph_count_data$`2020-08-05`)]

setdiff(dph_count_data$city_community[is.na(dph_count_data$`2020-08-04`)],
        dph_count_data$city_community[is.na(dph_count_data$`2020-08-03`)])

setdiff(dph_count_data$city_community[is.na(dph_count_data$`2020-08-05`)],
        dph_count_data$city_community[is.na(dph_count_data$`2020-08-03`)])

# So there were missing values for 5 locations on the 3rd and the 4th, but for 16 locations on the 4th

# it's the same for the case rate data
dph_rate_data$city_community[is.na(dph_rate_data$`2020-08-03`)]
dph_rate_data$city_community[is.na(dph_rate_data$`2020-08-04`)]
dph_rate_data$city_community[is.na(dph_rate_data$`2020-08-05`)]

# it's the same for the death data
dph_deaths_data$city_community[is.na(dph_deaths_data$`2020-08-03`)]
dph_deaths_data$city_community[is.na(dph_deaths_data$`2020-08-04`)]
dph_deaths_data$city_community[is.na(dph_deaths_data$`2020-08-05`)]

setdiff(dph_deaths_data$city_community[is.na(dph_deaths_data$`2020-08-04`)],
        dph_deaths_data$city_community[is.na(dph_deaths_data$`2020-08-03`)])


# it's the same for the death rate data
dph_deaths_rate_data$city_community[is.na(dph_deaths_rate_data$`2020-08-03`)]
dph_deaths_rate_data$city_community[is.na(dph_deaths_rate_data$`2020-08-04`)]
dph_deaths_rate_data$city_community[is.na(dph_deaths_rate_data$`2020-08-05`)]

# but...

setdiff(dph_count_data$city_community[is.na(dph_count_data$`2020-08-04`)],
        dph_rate_data$city_community[is.na(dph_rate_data$`2020-08-04`)])

setdiff(dph_count_data$city_community[is.na(dph_count_data$`2020-08-04`)],
        dph_deaths_data$city_community[is.na(dph_deaths_data$`2020-08-04`)])

setdiff(dph_count_data$city_community[is.na(dph_count_data$`2020-08-04`)],
        dph_deaths_rate_data$city_community[is.na(dph_deaths_rate_data$`2020-08-04`)])

setdiff(dph_deaths_data$city_community[is.na(dph_deaths_data$`2020-08-04`)],
        dph_deaths_rate_data$city_community[is.na(dph_deaths_rate_data$`2020-08-04`)])

# I CAN FIX THIS, but not right now.

# the 11 communities with inexplicable missing values on that date are:
counts_fix_list <- c("City of Commerce", "City of Lancaster", "City of Lynwood", "City of San Dimas", "Los Angeles - Boyle Heights", "Los Angeles - Downtown", "Los Angeles - San Pedro","Los Angeles - Wholesale District", "Unincorporated - Castaic", "Unincorporated - La Verne", "Unincorporated - Santa Monica Mountains")
# NOTE, THE ACTUAL SOLUTION DOESN'T DEPEND ON KNOWING THIS


# fix dph_count_data ----
# testing a solution based on using rowMeans; update: IT WORKS

sum(is.na(dph_count_data$`2020-08-03`))
sum(is.na(dph_count_data$`2020-08-04`))
sum(is.na(dph_count_data$`2020-08-05`))

selector_missing_cases <- !is.na(dph_count_data$`2020-08-03`) &
   is.na(dph_count_data$`2020-08-04`) &
   ! is.na(dph_count_data$`2020-08-05`)

dph_count_data$`2020-08-03`[selector_missing_cases]
dph_count_data$`2020-08-04`[selector_missing_cases]
dph_count_data$`2020-08-05`[selector_missing_cases]

dph_count_data$`2020-08-04`[selector_missing_cases]

dph_count_data$`2020-08-04`[selector_missing_cases] <-
   dph_count_data[c("2020-08-03", "2020-08-05")] %>%
   dplyr::filter(selector_missing_cases) %>%
   rowMeans() %>% round()

dph_count_data$`2020-08-04`[selector_missing_cases]

dph_count_data[dph_count_data$city_community %in% counts_fix_list, ] %>%
   dplyr::select(c(`2020-08-03`, `2020-08-04`, `2020-08-05`))


# so dph_count_data is fixed. now to fix the others

# fix dph_rate_data ----

sum(is.na(dph_rate_data$`2020-08-03`))
sum(is.na(dph_rate_data$`2020-08-04`))
sum(is.na(dph_rate_data$`2020-08-05`))

selector_missing_rates <- !is.na(dph_rate_data$`2020-08-03`) &
   is.na(dph_rate_data$`2020-08-04`) &
   ! is.na(dph_rate_data$`2020-08-05`)

sum(selector_missing_rates)

dph_rate_data$`2020-08-03`[selector_missing_rates]
dph_rate_data$`2020-08-04`[selector_missing_rates]
dph_rate_data$`2020-08-05`[selector_missing_rates]

dph_rate_data$`2020-08-04`[selector_missing_rates]

dph_rate_data$`2020-08-04`[selector_missing_rates] <-
   dph_rate_data[c("2020-08-03", "2020-08-05")] %>%
   dplyr::filter(selector_missing_rates) %>%
   rowMeans() %>% round()

dph_rate_data$`2020-08-04`[selector_missing_rates]

dph_rate_data$`2020-08-03`[selector_missing_rates]
dph_rate_data$`2020-08-04`[selector_missing_rates]
dph_rate_data$`2020-08-05`[selector_missing_rates]



# fix dph_deaths_data ----

sum(is.na(dph_deaths_data$`2020-08-03`))
sum(is.na(dph_deaths_data$`2020-08-04`))
sum(is.na(dph_deaths_data$`2020-08-05`))

selector_missing_deaths <- !is.na(dph_deaths_data$`2020-08-03`) &
   is.na(dph_deaths_data$`2020-08-04`) &
   ! is.na(dph_deaths_data$`2020-08-05`)

sum(selector_missing_deaths)

dph_deaths_data$`2020-08-03`[selector_missing_deaths]
dph_deaths_data$`2020-08-04`[selector_missing_deaths]
dph_deaths_data$`2020-08-05`[selector_missing_deaths]

dph_deaths_data$`2020-08-04`[selector_missing_deaths]

dph_deaths_data$`2020-08-04`[selector_missing_deaths] <-
   dph_deaths_data[c("2020-08-03", "2020-08-05")] %>%
   dplyr::filter(selector_missing_deaths) %>%
   rowMeans() %>% round()

dph_deaths_data$`2020-08-04`[selector_missing_deaths]

dph_deaths_data$`2020-08-03`[selector_missing_deaths]
dph_deaths_data$`2020-08-04`[selector_missing_deaths]
dph_deaths_data$`2020-08-05`[selector_missing_deaths]



# fix dph_deaths_rate_data ----

sum(is.na(dph_deaths_rate_data$`2020-08-03`))
sum(is.na(dph_deaths_rate_data$`2020-08-04`))
sum(is.na(dph_deaths_rate_data$`2020-08-05`))

selector_missing_deaths_rates <- !is.na(dph_deaths_rate_data$`2020-08-03`) &
   is.na(dph_deaths_rate_data$`2020-08-04`) &
   ! is.na(dph_deaths_rate_data$`2020-08-05`)

sum(selector_missing_deaths_rates)

dph_deaths_rate_data$`2020-08-03`[selector_missing_deaths_rates]
dph_deaths_rate_data$`2020-08-04`[selector_missing_deaths_rates]
dph_deaths_rate_data$`2020-08-05`[selector_missing_deaths_rates]

dph_deaths_rate_data$`2020-08-04`[selector_missing_deaths_rates]

dph_deaths_rate_data$`2020-08-04`[selector_missing_deaths_rates] <-
   dph_deaths_rate_data[c("2020-08-03", "2020-08-05")] %>%
   dplyr::filter(selector_missing_deaths_rates) %>%
   rowMeans() %>% round()

dph_deaths_rate_data$`2020-08-04`[selector_missing_deaths_rates]

dph_deaths_rate_data$`2020-08-03`[selector_missing_deaths_rates]
dph_deaths_rate_data$`2020-08-04`[selector_missing_deaths_rates]
dph_deaths_rate_data$`2020-08-05`[selector_missing_deaths_rates]






# ONE TIME ONLY ----

dph_count_data <- dph_count_data %>%
   mutate(`2020-07-03` = NA) %>%
   mutate(`2020-07-04` = NA) %>%
   dplyr::select(sort(current_vars())) %>%
   dplyr::select(city_community, everything())



dph_deaths_data <- dph_deaths_data %>%
   mutate(`2020-07-03` = NA) %>%
   mutate(`2020-07-04` = NA) %>%
   dplyr::select(sort(current_vars())) %>%
   dplyr::select(city_community, everything())



dph_rate_data <- dph_rate_data %>%
   mutate(`2020-07-03` = NA) %>%
   mutate(`2020-07-04` = NA) %>%
   dplyr::select(sort(current_vars())) %>%
   dplyr::select(city_community, everything())



dph_deaths_rate_data <- dph_deaths_rate_data %>%
   mutate(`2020-07-03` = NA) %>%
   mutate(`2020-07-04` = NA) %>%
   dplyr::select(sort(current_vars())) %>%
   dplyr::select(city_community, everything())






# 
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# downloaded yesterday's data this morning, need to rename the columns


dph_count_data <- dph_count_data %>%
   rename(`2020-11-17` = `2020-11-18`)

dph_rate_data <- dph_rate_data %>%
   rename(`2020-11-17` = `2020-11-18`)

dph_deaths_data <- dph_deaths_data %>%
   rename(`2020-11-17` = `2020-11-18`)

dph_deaths_rate_data <- dph_deaths_rate_data %>%
   rename(`2020-11-17` = `2020-11-18`)


