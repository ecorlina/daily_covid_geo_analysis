# import count data from DPH, add to data history, and merge each day with geo data ----

# get the latest data from the web

# PICK ONE OR THE OTHER OF THE FOLLOWING DEPENDING ON IF SITE IS WORKING, 2020-09-17
dphpage <- read_html("http://publichealth.lacounty.gov/media/Coronavirus/locations.htm")
#dphpage <- read_html("./dph_locations_html/locations.htm")

tbl <- dphpage %>% html_nodes("table") %>% .[1] %>% html_table() %>% .[[1]]

lb_pas_data_start_row <- which(str_detect(tbl$X1, "Laboratory Confirmed Cases")) + 2
lb_pas_data_end_row <- which(str_detect(tbl$X1, "Laboratory Confirmed Cases")) + 3

lb_pas_deaths_data_start_row <- which(str_detect(tbl$X1, "Deaths"))[2] + 2
lb_pas_deaths_data_end_row <- which(str_detect(tbl$X1, "Deaths"))[2] + 3


tbl2 <- dphpage %>% html_nodes("table") %>% .[2] %>% html_table(fill = T) %>% .[[1]]

# tbl2 <- tbl2[1:5]   # one-time code addition to deal with badly formatted html tables
# tbl2 <- tbl2 %>% slice_head(n = 342)   # one-time code addition to deal with badly formatted html tables


# THIS CODE DOESN'T WORK AFTER THE PAGE REDESIGN ON 2020-07-31
# neighborhood_data_start_row <- which(str_detect(tbl$X1, "CITY/COMMUNITY")) + 1

# USE THE FOLLOWING CODE INSTEAD
neighborhood_data_start_row <- 1
neighborhood_data_end_row <- max(which(str_detect(tbl2$`CITY/COMMUNITY**`, "Under [Ii]nvestigation"))) - 1


# new_count_data$city_community <- str_remove(new_count_data$city_community, "\\*")
# new_rate_data$city_community <- str_remove(new_rate_data$city_community, "\\*")


lb_pas_data <- tibble(tbl[lb_pas_data_start_row:lb_pas_data_end_row,]) %>%
   rename(city_community = X1,
          count = X2) %>%
   mutate(city_community = str_replace(city_community, "-+ ", "City of ")) %>%
   mutate(count = ifelse(count == "--", NA, count)) %>%
   mutate(count = as.numeric(count)) %>%
   mutate(count_rate = NA_real_)

lb_pas_deaths_data <- tibble(tbl[lb_pas_deaths_data_start_row:lb_pas_deaths_data_end_row,]) %>%
   rename(city_community = X1,
          deaths = X2) %>%
   mutate(city_community = str_replace(city_community, "-+ ", "City of ")) %>%
   mutate(deaths = ifelse(deaths == "--", NA, deaths)) %>%
   mutate(deaths = as.numeric(deaths)) %>%
   mutate(deaths_rate = NA_real_)

lb_pas_data <- left_join(lb_pas_data, lb_pas_deaths_data)

pep_2018_long_beach <- 465865  # from US Census Population Estimates Program
pep_2018_pasadena <- 140906  # from US Census Population Estimates Program

lb_pas_data[[1,1]]
lb_pas_data[[1,3]] <- round((lb_pas_data[[1,2]]/pep_2018_long_beach) * 100000, digits = 2)
lb_pas_data[[1,5]] <- round((lb_pas_data[[1,4]]/pep_2018_long_beach) * 100000, digits = 2)

lb_pas_data[[2,1]]
lb_pas_data[[2,3]] <- round((lb_pas_data[[2,2]]/pep_2018_pasadena) * 100000, digits = 2)
lb_pas_data[[2,5]] <- round((lb_pas_data[[2,4]]/pep_2018_pasadena) * 100000, digits = 2)

citycomm_data <- tibble(tbl2[neighborhood_data_start_row:neighborhood_data_end_row,]) %>%
   rename(city_community = `CITY/COMMUNITY**`,
          count = Cases,
          count_rate = `Case Rate1`,
          deaths = Deaths,
          deaths_rate = `Death Rate2`) %>%
   mutate(city_community = str_remove(city_community, "\\*")) %>%
   mutate(count = ifelse(count == "--", NA, count)) %>%
   mutate(count = as.numeric(count)) %>%
   mutate(count_rate = ifelse(count == "--", NA, count_rate)) %>%
   mutate(count_rate = as.numeric(count_rate)) %>%
   mutate(deaths = ifelse(count == "--", NA, deaths)) %>%
   mutate(deaths = as.numeric(deaths)) %>%
   mutate(deaths_rate = ifelse(count == "--", NA, deaths_rate)) %>%
   mutate(deaths_rate = as.numeric(deaths_rate))

new_data <- bind_rows(citycomm_data, lb_pas_data)

test <- left_join(dph_count_data[,c(1,ncol(dph_count_data))], new_data[,1:2]) %>%
   setNames(c("place", "last_import", "current_page")) %>%
   mutate(change = current_page - last_import)

test_sum <- sum(test$change, na.rm = T)
test_sum


# the following because on 2020-11-20 they updated the data but they did so very, very incorrectly.
# sum(new_data$count, na.rm = T) - sum(dph_count_data$`2020-11-19`, na.rm = T)

if (sum(test$change, na.rm = T) != 0) {
   new_count_data <- new_data[, c("city_community", "count")] %>% setNames(c("city_community", as.character(today())))
   dph_count_data <- left_join(dph_count_data, new_count_data)
   new_rate_data <- new_data[, c("city_community", "count_rate")] %>% setNames(c("city_community", as.character(today())))
   dph_rate_data <- left_join(dph_rate_data, new_rate_data)
   new_deaths_data <- new_data[, c("city_community", "deaths")] %>% setNames(c("city_community", as.character(today())))
   dph_deaths_data <- left_join(dph_deaths_data, new_deaths_data)
   new_deaths_rate_data <- new_data[, c("city_community", "deaths_rate")] %>% setNames(c("city_community", as.character(today())))
   dph_deaths_rate_data <- left_join(dph_deaths_rate_data, new_deaths_rate_data)
}


# compute quantiles ----

# for manual autoscaling as the counts grow
# breaks = c(0, 1, seq(25, 300, by = 25)
# breaks = c(0, 1, seq(50, 750, by = 50)
# breaks = c(0, seq(100, 800, by = 100)


# THIS IS FOR MAPPING TO THE LA TIMES NEIGHBORHOODS ----

# remove LA from data since it's covered by all the neighborhoods

omega <- ncol(dph_count_data)
count_quintiles_overall <- round(unname(quantile(dph_count_data[,omega], probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)))

today_counts <- dph_count_data[, c(1, ncol(dph_count_data))] %>%
   setNames(c("city_community", "count")) %>%
   mutate(map_to = vlookup(.$city_community, neighborhoods_crosswalk, "city_community", "map_to"))
today_data <- today_counts %>%
   group_by(map_to) %>%
   summarize(count = sum(count, na.rm = T))

ndsc_map_today <- left_join(ndsc_map, today_data, by = c("name" = "map_to")) %>%
   mutate(rate_comp = round((count / as.numeric(Total)) * 100000, digits = 2) )

count_quintiles <- ceiling(unname(quantile(
   ndsc_map_today$count,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))
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

today_deaths <- dph_deaths_data[, c(1, ncol(dph_deaths_data))] %>%
   setNames(c("city_community", "deaths")) %>%
   mutate(map_to = vlookup(.$city_community, neighborhoods_crosswalk, "city_community", "map_to"))
today_deaths_data <- today_deaths %>%
   group_by(map_to) %>%
   summarize(deaths = sum(deaths, na.rm = T))
ndsc_deaths_today <- left_join(ndsc_map, today_deaths_data, by = c("name" = "map_to")) %>%
   mutate(rate_comp = round((deaths / as.numeric(Total)) * 100000, digits = 3) )
deaths_deciles <- ceiling(unname(quantile(
   ndsc_deaths_today$deaths,
   probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
   na.rm = T
)))
deaths_quintiles <- ceiling(unname(quantile(
   ndsc_deaths_today$deaths,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))

deaths_rate_quintiles <- ceiling(unname(quantile(
   ndsc_deaths_today$rate_comp,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))


count_deciles
rate_quintiles

deaths_deciles
deaths_quintiles
deaths_rate_quintiles



# THIS IS FOR MAPPING TO THE COUNTYWIDE STATISTICAL AREAS (CSA) NEIGHBORHOODS ----

# remove LA from data since it's covered by all the neighborhoods

# omega <- ncol(dph_count_data)
# count_quintiles_csa <- round(unname(quantile(dph_count_data[,omega], probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = T)))

today_counts_csa <- dph_count_data[, c(1, ncol(dph_count_data))] %>%
   setNames(c("city_community", "count")) %>%
   dplyr::filter(city_community != "Los Angeles")
today_rates_csa <- dph_rate_data[, c(1, ncol(dph_rate_data))] %>%
   setNames(c("city_community", "rate")) %>%
   dplyr::filter(city_community != "Los Angeles")

count_quintiles_csa <- ceiling(unname(quantile(
   today_counts_csa$count,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))

count_deciles_csa <- ceiling(unname(quantile(
   today_counts_csa$count,
   probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
   na.rm = T
)))

rate_quintiles_csa <- ceiling(unname(quantile(
   today_rates_csa$rate,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))

rate_deciles_csa <- ceiling(unname(quantile(
   today_rates_csa$rate,
   probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
   na.rm = T
)))

today_deaths_csa <- dph_deaths_data[, c(1, ncol(dph_deaths_data))] %>%
   setNames(c("city_community", "deaths")) %>%
   dplyr::filter(city_community != "Los Angeles")
today_deaths_rates_csa <- dph_deaths_rate_data[, c(1, ncol(dph_deaths_rate_data))] %>%
   setNames(c("city_community", "deaths_rate")) %>%
   dplyr::filter(city_community != "Los Angeles")

deaths_deciles_csa <- ceiling(unname(quantile(
   today_deaths_csa$deaths,
   probs = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1),
   na.rm = T
)))

deaths_quintiles_csa <- ceiling(unname(quantile(
   today_deaths_csa$deaths,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))

deaths_rate_quintiles_csa <- ceiling(unname(quantile(
   today_deaths_rates_csa$deaths_rate,
   probs = c(0, 0.2, 0.4, 0.6, 0.8, 1),
   na.rm = T
)))


count_quintiles_csa
count_deciles_csa
rate_quintiles_csa
rate_deciles_csa

deaths_quintiles_csa
deaths_deciles_csa
deaths_rate_quintiles_csa


# extraction of homeless data was removed from lacounty_heatmap_v9_DPH_data and saved as this v11 ----
