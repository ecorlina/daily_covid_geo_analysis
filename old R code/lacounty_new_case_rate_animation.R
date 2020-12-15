# Want to make an interactive map or a gif of case rate data
# Want it to go all the way back to the beginning
# Want to be able to specify exactly which date are included
# Want all layers to use the CSA map
# Want each day to be shown on a scale that's optimized for that day's data,
# so patterns are visible from the beginning.

# Thinx: working with only the dph_rate_data tibble
# each column, except the first, is a date
# each row is a community

# Step 1: Make a new tibble. 
# each column, except the first, is a date
# each row is a cutoff for a five point quintile scale
# BETTER WAY
# make the dph_rate_data tidy by making it long
# group_by the date column
# summarize by quantile -- see here for (old) summary:
# https://www.tidyverse.org/blog/2020/03/dplyr-1-0-0-summarise/


quibble <- function(x, q = c(0, 0.2, 0.4, 0.6, 0.8, 1)) {
   tibble(cutoffs = quantile(x, q, na.rm = T), q = q)
}

dph_rate_data_long <- dph_rate_data %>%
   dplyr::filter(city_community != "Los Angeles") %>%
   pivot_longer(cols = -city_community,
                names_to = "date",
                values_to = "rate")

dph_rate_quintiles_long <- dph_rate_data_long %>%
   group_by(date) %>%
   summarize(quibble(rate))

dph_rate_quintiles <- dph_rate_quintiles_long %>%
   pivot_wider(names_from = date,
               values_from = cutoffs)

# don't think I can reuse the old code that exports a stacked facet as an animated gif
# this is because I want each layer to have it's own, optimized scale
# and not one master scale that applies to all dates

# so I need to iterate across all the dates (columns) in the tables,
# and for each date, create and export an image
# for clarity, omit the legend entirely

for (i in 2:ncol(dph_rate_data)) {
   these_rates <- dph_rate_data[, c(1, i)] %>% dplyr::filter(city_community != "Los Angeles")
   these_quintiles <- dph_rate_quintiles[, c(1, i)]
   this_date <- colnames(these_rates)[2]
   these_rates <- these_rates %>%
      setNames(c("city_community", "rate"))
   these_quintiles <- these_quintiles %>%
      setNames(c("q", "cutoff"))
   csa_map_rates <- left_join(csa_map, these_rates, by = c("label" = "city_community"))
   csa_map_rates <- csa_map_rates %>%
      mutate(rate = ifelse(is.na(rate), 0, rate)) # WANT TO CHANGE THIS; SEE NOTE BELOW
   map_layer_rates <-
      tm_shape(lacounty_mainland) +
      tm_borders(group = "LA County border") +
      tm_shape(csa_map_rates) +
      tm_fill(col = "rate",
              style = "fixed",
              id = "label",
              breaks = these_quintiles$cutoff,  # a la DPH dashboard; quintiles
              title = "Confirmed Case Rates (per 100k)",
              group = str_c("COVID-19 case rate, ", this_date),
              legend.show = F) +
      tm_layout(scale = 0.5,
                main.title = this_date,
                main.title.size = 1,
                legend.position = c("right", "top"),
                legend.outside = T) +
      tm_borders(lwd = 0.2)
   #assign(str_c("animated_rates_map_", i-1), map_layer_rates)
   tmap_save(map_layer_rates,
             filename = str_c("../movies/new_animation/lac_new_rate_animation_", i-1, ".jpg"),
             width = 6.5, height = NA, units = "in", dpi = 150)
}

# want to change this to replace missing with average of day before and day after



# go and rename the images in the finder, then...
# cd "Dropbox/Rocinante Research/VOALA MBPro/confirmed_cases/movies/new_animation"
# ffmpeg -r 4 -i map%05d.jpg -vcodec libx264 -crf 25 -s 700x510 -pix_fmt yuv420p test.mp4

