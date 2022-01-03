# interactive side-by-side comparison map ----

vax_bins <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
vax_pal <- colorBin("Spectral",
                    domain = lac_covid_vax_csa$five_plus_pct_fully,
                    bins = vax_bins)

case_bins <- c(0, 31.25, 62.5, 93.75, 125, 156.25, 187.5, 218.75, 250)
case_pal <- colorBin("YlOrBr",
                     domain = lac_covid_long_csa_7day_limited$case_7day_rate,
                     bins = case_bins)


leaflet() %>%
   setView(-118.24, 34.27, 9) %>%
   addMapPane("left", zIndex = 0) %>%
   addMapPane("right", zIndex = 0) %>%
   addProviderTiles(providers$CartoDB.Positron, group = "vaccinations", layerId = "Vaccinations",
                    options = pathOptions(pane = "left")
   ) %>%
   addProviderTiles(providers$CartoDB.Positron, group = "case rate", layerId = "Case Rate",
                    options = pathOptions(pane = "right")
   ) %>%
   addPolygons(data = lac_covid_vax_csa,
               stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.8, 
               fillColor = ~vax_pal(five_plus_pct_fully), color = "black", weight = 0.5,
               group = "Vaccinations",
               options = pathOptions(pane = "left")
   ) %>%
   addPolygons(data = lac_covid_long_csa_7day_limited,
               stroke = TRUE, smoothFactor = 0.2, fillOpacity = 0.8, 
               fillColor = ~case_pal(case_7day_rate), color = "black", weight = 0.5,
               group = "Case Rate",
               options = pathOptions(pane = "right")
   ) %>%
   addLayersControl(position = "topright", options = layersControlOptions(collapsed = FALSE, overlayGroups = c("Vaccinations", "Case Rate"))) %>%
   addSidebyside(layerId = "sidecontrols",
                 rightId = "Case Rate",
                 leftId = "Vaccinations"
   ) %>%
   addResetMapButton()
   
