library(rvest)

# Excel-equivalent vlookup function courtesy of a tweet by Jenny Bryan of the RStudio team
vlookup <- function(this, data, key, value) {
   m <- match(this, data[[key]])
   data[[value]][m]
}


# get the original crosswalk table
sheets_here <- excel_sheets("./data/dph_city_community_counts.xlsx")
n_days <- length(sheets_here)

crosswalk <- read_xlsx("./data/dph_city_community_counts.xlsx", sheet = sheets_here[n_days]) %>% .[, 1:2]

# write_rds(crosswalk, "neighborhoods_crosswalk.rds")




# need to transfter all the old data into a single tibble
# that can be the new source for generating the maps entirely within R

# already have sheets_here and n_days

# need to cycle through the existing sheets,
# rename columns,
# join them to a single table,
# and save that result to disk so it can be imported and updated every day

# thinx: save the original rate numbers for reference,
# but drop them in the working code and replace it with computed rates.

# dph_count_data <- crosswalk %>% group_by(map_to) %>% summarize() %>% filter(!is.na(map_to))
# dph_rate_data <- crosswalk %>% group_by(map_to) %>% summarize() %>% filter(!is.na(map_to))

dph_count_data <- NULL
dph_rate_data <- NULL

for (i in n_days:1) {
   this_sheet <- sheets_here[i]
   local_counts <- read_xlsx("./data/dph_city_community_counts.xlsx", sheet = this_sheet) %>%
      dplyr::select(-map_to)
   if (is.null(dph_count_data)) {
      new_counts <- local_counts %>% dplyr::select(-rate)
      dph_count_data <- new_counts %>% setNames(c("city_community", this_sheet))
      new_rates <- local_counts %>% dplyr::select(-count)
      dph_rate_data <- new_rates %>% setNames(c("city_community", this_sheet))
   } else {
      new_counts <- local_counts %>% dplyr::select(-rate) %>% setNames(c("city_community", this_sheet))
      dph_count_data <- left_join(dph_count_data, new_counts, by = "city_community")
      new_rates <- local_counts %>% dplyr::select(-count) %>% setNames(c("city_community", this_sheet))
      dph_rate_data <- left_join(dph_rate_data, new_rates, by = "city_community")
   }
}


write_rds(dph_count_data, "dph_count_data.rds")
write_rds(dph_rate_data, "dph_rate_data.rds")




# get the latest data
dphpage <- read_html("http://publichealth.lacounty.gov/media/Coronavirus/locations.htm")

tbl <- dphpage %>% html_nodes("table") %>% .[1] %>% html_table() %>% .[[1]]

lb_pas_data_start_row <- which(str_detect(tbl$X1, "(LCC)")) + 2
lb_pas_data_end_row <- which(str_detect(tbl$X1, "(LCC)")) + 3

neighborhood_data_start_row <- which(str_detect(tbl$X1, "CITY/COMMUNITY")) + 1
neighborhood_data_end_row <- max(which(str_detect(tbl$X1, "Under [Ii]nvestigation"))) - 1

lb_pas_data <- tibble(tbl[lb_pas_data_start_row:lb_pas_data_end_row,]) %>%
   rename(city_community = X1,
          count = X2,
          rate = X3) %>%
   mutate(city_community = str_replace(city_community, "- ", "City of ")) %>%
   mutate(count = ifelse(count == "--", NA, count)) %>%
   mutate(count = as.numeric(count)) %>%
   mutate(rate = ifelse(count == "--", NA, rate)) %>%
   mutate(rate = as.numeric(rate))

citycomm_data <- tibble(tbl[neighborhood_data_start_row:neighborhood_data_end_row,]) %>%
   rename(city_community = X1,
          count = X2,
          rate = X3) %>%
   mutate(count = ifelse(count == "--", NA, count)) %>%
   mutate(count = as.numeric(count)) %>%
   mutate(rate = ifelse(count == "--", NA, rate)) %>%
   mutate(rate = as.numeric(rate))

new_data <- bind_rows(citycomm_data, lb_pas_data)

test <- left_join(dph_count_data[,c(1,ncol(dph_count_data))], new_data[,1:2]) %>%
   setNames(c("place", "last_import", "current_page")) %>%
   mutate(change = current_page - last_import)

if (sum(test$change, na.rm = T) != 0) {
   new_count_data <- new_data[, c("city_community", "count")] %>% setNames(c("city_community", this_sheet))
   dph_count_data <- left_join(dph_count_data, new_count_data)
   new_rate_data <- new_data[, c("city_community", "rate")] %>% setNames(c("city_community", this_sheet))
   dph_rate_data <- left_join(dph_rate_data, new_rate_data)
   write_rds(dph_count_data, "dph_count_data.rds")
   write_rds(dph_rate_data, "dph_rate_data.rds")
}



nonexistent_foo %>%
   mutate(map_to = vlookup(.$city_community, crosswalk, "city_community", "map_to"))

# SO, now I can import the new data directly (and quickly) from the web page

# but what I want to actually do is to download the new data,
# compare it to the last-added column from the data on disk,
# stop if it's the same,
# update the master data if it's new,
# then do the summarizing of the counts data

# if this is 0, then the data have not been updated. stop and wait.
