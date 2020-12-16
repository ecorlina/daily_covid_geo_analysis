# TIMELINE HEATMAP ----

# CODE BASED ON "English LA Heatmaps.R"
# https://github.com/VictimOfMaths/COVID-19/blob/master/Heatmaps/readme.md

# load packages ----
library(tidyverse)
library(curl)
library(forcats)
library(RcppRoll)
library(data.table)
library(readxl)
library(cowplot)
library(zoo)
library(sf)
library(rmapshaper)
library(gganimate)
library(paletteer)
library(ggtext)
library(lubridate)
library(rvest)
library(tidyxl)
library(unpivotr)
library(hablar)
library(janitor)
library(busdater)
library(khroma)


# import 7-DAY AVG case rates data as if it's not already done ----

data_folders_there <- list.files("../daily_voala_dashboard/data") %>%
   purrr::discard(str_detect(., "readme"))

# make the chart ----

date_to_plot <- last(data_folders_there)
# date_to_plot <- "2020-11-21"

wk_avg_case_rate_csa <- readr::read_csv(paste("../daily_voala_dashboard/data", date_to_plot, "LA_County_Covid19_CSA_7day_case_death_table.csv", sep = "/")) %>%
   dplyr::select(ep_date, geo_merge, cases_7day, case_7day_rate, adj_case_7day_rate) %>%
   mutate(ep_date = as_date(ep_date)) %>%
   dplyr::select(city_community = geo_merge,
                 everything()) %>%
   dplyr::filter(!is.na(ep_date),
                 city_community != "Los Angeles County")

wk_avg_case_rate_csa$city_community <- as_factor(wk_avg_case_rate_csa$city_community)

wk_avg_case_rate_csa <- wk_avg_case_rate_csa %>% complete(city_community, ep_date, fill = list(cases_7day = 0, case_7day_rate = 0, adj_case_7day_rate = 0))

cumulative_case_rate_csa <- readr::read_csv(paste("../daily_voala_dashboard/data", date_to_plot, "LA_County_Covid19_CSA_case_death_table.csv", sep = "/")) %>%
   dplyr::select(geo_merge, cases_final, case_rate_final, adj_case_rate_final) %>%
   dplyr::select(city_community = geo_merge,
                 everything())

cumulative_case_rate_csa$city_community <- as_factor(cumulative_case_rate_csa$city_community)

wk_avg_case_rate_csa <- left_join(wk_avg_case_rate_csa, cumulative_case_rate_csa, by = c("city_community" = "city_community"))

heatmap <- wk_avg_case_rate_csa %>%
   #dplyr::select(city_community, ep_date, case_7day_rate) %>%
   group_by(city_community) %>%
   mutate(max_case_rate = max(case_7day_rate),
          max_case_day = ep_date[which(case_7day_rate == max_case_rate)][1],
          max_adj_case_rate = max(adj_case_7day_rate),
          max_adj_case_day = ep_date[which(adj_case_7day_rate == max_adj_case_rate)][1])

heatmap$max_case_prop <- heatmap$case_7day_rate / heatmap$max_case_rate
heatmap$max_adj_case_prop <- heatmap$adj_case_7day_rate / heatmap$max_adj_case_rate

plot_from <- "2020-03-01"
plot_to <- max(heatmap$ep_date)


casetiles <- ggplot(heatmap,
                    aes(x = ep_date, y = fct_reorder(city_community, max_case_day),
                        fill = max_case_prop)) +
   geom_tile(color = "White",
             show.legend = F) +
   theme_classic() +
   #scale_fill_distiller(palette = "Spectral") +
   #scale_fill_viridis_c(option = "C") +
   scale_fill_YlOrBr() +
   scale_y_discrete(name = "",
                    expand = c(0, 0)) +
   scale_x_date(name = "Date",
                limits = as.Date(c(plot_from, plot_to)),
                expand = c(0, 0)) +
   labs(title="Timelines for COVID-19 cases in Los Angeles Countywide Statistical Areas",
        subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalized to the maximum value within the CSA.\nCSAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each CSA.\nData updated to ", plot_to, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
        caption="Data from LA County Department of Public Health | Plot inspired by @VictimOfMaths") +
   theme(axis.line.y=element_blank(),
         plot.subtitle=element_text(size = rel(0.78)),
         plot.title.position="plot",
         axis.text.y=element_text(colour = "Black", size = rel(0.75)),
         plot.title=element_text(size = rel(2.3)))

casebars <- ggplot(subset(heatmap, ep_date == max_case_day),
                   aes(x = cases_final, y = fct_reorder(city_community, max_case_day), fill = cases_final)) +
   geom_col(show.legend = F) +
   theme_classic() +
   #scale_fill_distiller(palette = "Spectral") +
   #scale_fill_viridis_c(option = "C") +
   scale_fill_YlOrBr() +
   scale_x_continuous(name = "Total confirmed cases",
                      breaks=c(0, 2000, 4000, 6000, 8000, 10000)) +
   theme(axis.title.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(),
         axis.ticks.y = element_blank(), axis.text.x = element_text(colour = "Black"))

png(paste0(geo_analysis_output_path, "/timeline_heatmaps/csa_cases_heatmap-", plot_to, ".png"),
    units = "in", width = 16, height = 32, res = 500)
plot_grid(casetiles, casebars, align = "h", rel_widths = c(1,0.2))
dev.off()




# STOP HERE, BELOW IS EXPLORATORY CODE FOR EXTRACTING A SUB-RANGE FROM THE LATEST UPDATE ----
# make chart for any given day ----

date_to_plot <- last(data_folders_there)  # this determines the folder from which the data table is loaded
plot_from <- "2020-03-01"
plot_to_windowed <- "2020-12-02"

wk_avg_case_rate_csa <- readr::read_csv(paste("../daily_voala_dashboard/data", date_to_plot, "LA_County_Covid19_CSA_7day_case_death_table.csv", sep = "/")) %>%
   dplyr::select(ep_date, geo_merge, cases_7day, case_7day_rate, adj_case_7day_rate) %>%
   mutate(ep_date = as_date(ep_date)) %>%
   dplyr::select(city_community = geo_merge,
                 everything()) %>%
   dplyr::filter(!is.na(ep_date),
                 city_community != "Los Angeles County") %>%
   dplyr::filter(ep_date <= as_date(plot_to_windowed))

wk_avg_case_rate_csa$city_community <- as_factor(wk_avg_case_rate_csa$city_community)

wk_avg_case_rate_csa <- wk_avg_case_rate_csa %>% complete(city_community, ep_date, fill = list(cases_7day = 0, case_7day_rate = 0, adj_case_7day_rate = 0))

cumulative_case_rate_csa <- readr::read_csv(paste("../daily_voala_dashboard/data", plot_to_windowed, "LA_County_Covid19_CSA_case_death_table.csv", sep = "/")) %>%
   dplyr::select(geo_merge, cases_final, case_rate_final, adj_case_rate_final) %>%
   dplyr::select(city_community = geo_merge,
                 everything())

cumulative_case_rate_csa$city_community <- as_factor(cumulative_case_rate_csa$city_community)

wk_avg_case_rate_csa <- left_join(wk_avg_case_rate_csa, cumulative_case_rate_csa, by = c("city_community" = "city_community"))

heatmap <- wk_avg_case_rate_csa %>%
   #dplyr::select(city_community, ep_date, case_7day_rate) %>%
   group_by(city_community) %>%
   mutate(max_case_rate = max(case_7day_rate),
          max_case_day = ep_date[which(case_7day_rate == max_case_rate)][1],
          max_adj_case_rate = max(adj_case_7day_rate),
          max_adj_case_day = ep_date[which(adj_case_7day_rate == max_adj_case_rate)][1])

heatmap$max_case_prop <- heatmap$case_7day_rate / heatmap$max_case_rate
heatmap$max_adj_case_prop <- heatmap$adj_case_7day_rate / heatmap$max_adj_case_rate

casetiles <- ggplot(heatmap,
                    aes(x = ep_date, y = fct_reorder(city_community, max_case_day),
                        fill = max_case_prop)) +
   geom_tile(color = "White",
             show.legend = F) +
   theme_classic() +
   #scale_fill_distiller(palette = "Spectral") +
   #scale_fill_viridis_c(option = "C") +
   scale_fill_YlOrBr() +
   scale_y_discrete(name = "",
                    expand = c(0, 0)) +
   scale_x_date(name = "Date",
                limits = as.Date(c(plot_from, plot_to_windowed)),
                expand = c(0, 0)) +
   labs(title="Timelines for COVID-19 cases in Los Angeles Countywide Statistical Areas",
        subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalized to the maximum value within the CSA.\nCSAs are ordered by the date at which they reached their peak number of new cases. Bars on the right represent the absolute number of cases in each CSA.\nData updated to ", plot_to_windowed, ". Data for most recent days is provisional and may be revised upwards as additional tests are processed."),
        caption="Data from LA County Department of Public Health | Plot inspired by @VictimOfMaths") +
   theme(axis.line.y=element_blank(),
         plot.subtitle=element_text(size = rel(0.78)),
         plot.title.position="plot",
         axis.text.y=element_text(colour = "Black", size = rel(0.75)),
         plot.title=element_text(size = rel(2.3)))

casebars <- ggplot(subset(heatmap, ep_date == max_case_day),
                   aes(x = cases_final, y = fct_reorder(city_community, max_case_day), fill = cases_final)) +
   geom_col(show.legend = F) +
   theme_classic() +
   #scale_fill_distiller(palette = "Spectral") +
   #scale_fill_viridis_c(option = "C") +
   scale_fill_YlOrBr() +
   scale_x_continuous(name = "Total confirmed cases",
                      breaks=c(0, 2000, 4000, 6000, 8000, 10000)) +
   theme(axis.title.y = element_blank(), axis.line.y = element_blank(), axis.text.y = element_blank(),
         axis.ticks.y = element_blank(), axis.text.x = element_text(colour = "Black"))

png(paste0(geo_analysis_output_path, "/timeline_heatmaps/csa_cases_heatmap_windowed-", plot_to_windowed, ".png"),
    units = "in", width = 16, height = 32, res = 500)
plot_grid(casetiles, casebars, align = "h", rel_widths = c(1,0.2))
dev.off()





# STOP HERE. BELOW IS EXPERIMENTAL CODE STILL ----
# countywide timeline heatmap ----

# rates based on county population estimate of 10,257,557
lacounty_population_est <- 10257557

date_to_plot <- last(data_folders_there)
# date_to_plot <- "2020-11-21"

wk_avg_case_rate_lac <- readr::read_csv(paste("../daily_voala_dashboard/data", date_to_plot, "LA_County_Covid19_cases_deaths_date_table.csv", sep = "/")) %>%
   dplyr::select(date_use, total_cases, new_case, avg_cases) %>%
   mutate(date_use = as_date(date_use)) %>%
   dplyr::filter(!is.na(date_use))


wk_avg_case_rate_lac <- wk_avg_case_rate_lac %>%
   mutate(case_7day_rate = 100000 * (avg_cases / lacounty_population_est))

heatmap_lac <- wk_avg_case_rate_lac %>%
   mutate(max_case_rate = max(case_7day_rate, na.rm = T),
          max_case_day = date_use[which(case_7day_rate == max_case_rate)][1])

heatmap_lac$max_case_prop <- heatmap_lac$case_7day_rate / heatmap_lac$max_case_rate

plot_from_lac <- "2020-03-01"
plot_to_lac <- max(heatmap_lac$date_use)


casetiles <- ggplot(heatmap_lac,
                    aes(x = date_use, y = 1,
                        fill = max_case_prop)) +
   geom_tile(color = "White",
             show.legend = F) +
   theme_classic() +
   #scale_fill_distiller(palette = "Spectral") +
   #scale_fill_viridis_c(option = "C") +
   scale_fill_YlOrBr() +
   scale_y_discrete(name = "",
                    expand = c(0, 0)) +
   scale_x_date(name = "Date",
                limits = as.Date(c(plot_from_lac, plot_to_lac)),
                expand = c(0, 0)) +
   labs(title="Timelines for COVID-19 cases in Los Angeles County",
        subtitle=paste0("The heatmap represents the 7-day rolling average of the number of new confirmed cases, normalized to the maximum value within the County updated to ", plot_to_lac, ".\nData for most recent days is provisional and may be revised upwards as additional tests are processed."),
        caption="Data from LA County Department of Public Health | Plot inspired by @VictimOfMaths") +
   theme(axis.line.y=element_blank(),
         plot.subtitle=element_text(size = rel(1)),
         plot.title.position="plot",
         axis.text.y=element_text(colour = "Black", size = rel(0.75)),
         plot.title=element_text(size = rel(2.3)))

png(paste0(geo_analysis_output_path, "/timeline_heatmaps/lac_cases_heatmap-", plot_to_lac, ".png"),
    units = "in", width = 16, height = 1.75, res = 500)
plot_grid(casetiles, NULL, align = "h", rel_widths = c(1,0.2))
dev.off()



