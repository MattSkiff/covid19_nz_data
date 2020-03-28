#library(readr) # to nicely read in data

# actionButton(inputId = "mohLink",
#              label = "MoH Data",
#              onclick = moh_open_link)
#menuItem("COVI", tabName = "about", icon = icon("address-card")) 

# output$mapDHBnorm <- renderLeaflet({
#     
#     covid_dhb.df <- covid_loc.df()
#     
#     dhb.sdf <- dhb.sdf()
#     
#     leaflet(data = dhb.sdf,
#             options = leafletOptions(minZoom = min_zoom, maxZoom = max_zoom,preferCanvas = T)) %>%
#         addPolygons(fillColor = ~virpal(dhb.sdf@data$value),
#                     weight = 1,
#                     opacity = 1,
#                     color = "white",
#                     dashArray = "3",
#                     fillOpacity = 0.7,
#                     highlight = highlightOptions(
#                         weight = 5,
#                         color = "#666",
#                         dashArray = "",
#                         fillOpacity = 0.7,
#                         bringToFront = TRUE),
#                     label = paste0(dhb.sdf$REGC2020_2,":\n",dhb.sdf$value),
#                     labelOptions = labelOptions(
#                         style = list("font-weight" = "normal", padding = "3px 8px"),
#                         textsize = "15px",
#                         direction = "auto")) %>% 
#         addLegend(pal = virpal, values = ~dhb.sdf$value, opacity = 0.7, title = "COVID19 Cases by DHB",
#                   position = "bottomright") %>%
#         setView(lng = lng_init, 
#                 lat = lat_init, 
#                 zoom = zoom_level_init) 
#     #addControl(map_title, position = "topleft")
# })


# covid_rc.df$DHB <- fct_recode(covid_rc.df$DHB, 
#                                    "Auckland Region" = "Auckland",
#                                    "Auckland Region" = "Waitemata",
#                                    "Wellington Region" = "Wellington",
#                                    "Marlborough Region" = "Marlborough",
#                                    "Bay of Plenty Region" = "Bay of Plenty",
#                                    "Otago Region" = "Dunedin",
#                                    "Canterbury Region" = "Canterbury",
#                                    "Otago Region" = "Wanaka",
#                                    "Otago Region" = "Queenstown",
#                                    "Waikato Region" = "Hamilton",
#                                    "Canterbury Region" = "Christchurch",
#                                    "Manawatu-Whanganui Region" = "New Plymouth",
#                                    "Wellington Region" = "Wairarapa",
#                                    "Wellington Region" = "Kapiti Coast", 
#                                    "Hawke's Bay Region" = "Hawkes Bay",
#                                    "Nelson Region" = "Nelson",
#                                    "Waikato Region" = "Waikato",
#                                    "Otago Region" = "Otago",
#                                    "Canterbury Region" = "Waitaki",
#                                    "Wellington Region" = "Upper Hutt",
#                                    "Tasman Region" = "Tasman",
#                                    "Manawatu-Whanganui Region" = "Manawatu",
#                                    "Northland Region" = "Northland",
#                                    "Taranaki Region" = "Taranaki",
#                                    "Taupo Region" = "Taupo",
#                                    "Auckland Region" = "Southern DHB",
#                                    "Bay of Plenty Region" = "Rotorua",
#                                    "Southland Region" = "Invercargill",
#                                    "Marlborough Region" = "Blenheim") 

# covid_rc_tally.df <- as.data.frame(table(covid_rc.df$Location))

# Trick file date creation update
# onStop(function() {
#     
#     # File name
#     p <- paste0(getwd(), "/app.R")
#     
#     # Update file 'date creation'
#     Sys.setFileTime(p, now())
#     
# }) # onStop
}



# covid.ls <- url %>%
# 	read_html() %>%
# 	# html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
# 	html_table()
# 
# covid.df <- covid.ls[[1]]
# nrow(covid.df)

# Run the application 
## Map TA -------------------
# output$mapTA <- renderLeaflet({
#     covid_ta.df <- covid.df # covid.df()
#     covid_ta.df$Location <- fct_recode(covid_ta.df$Location, 
#                                        "Auckland District" = "Auckland",
#                                        "Auckland District" = "Waitemata",
#                                        "Wellington District" = "Wellington",
#                                        "Marlborough District" = "Marlborough",
#                                        "Western Bay of Plenty District" = "Bay of Plenty",
#                                        "Dunedin District" = "Dunedin",
#                                        "Canterbury Region" = "Canterbury",
#                                        "Otago Region" = "Wanaka",
#                                        "Otago Region" = "Queenstown",
#                                        "Waikato Region" = "Hamilton",
#                                        "Canterbury Region" = "Christchurch",
#                                        "Manawatu-Whanganui Region" = "New Plymouth",
#                                        "Wellington Region" = "Wairarapa",
#                                        "Wellington Region" = "Kapiti Coast", 
#                                        "Hawke's Bay Region" = "Hawkes Bay",
#                                        "Nelson Region" = "Nelson",
#                                        "Waikato Region" = "Waikato",
#                                        "Otago Region" = "Otago",
#                                        "Canterbury Region" = "Waitaki",
#                                        "Wellington Region" = "Upper Hutt",
#                                        "Tasman Region" = "Tasman",
#                                        "Manawatu-Whanganui Region" = "Manawatu",
#                                        "Northland Region" = "Northland",
#                                        "Taranaki Region" = "Taranaki",
#                                        "Taupo Region" = "Taupo",
#                                        "Auckland Region" = "Southern DHB",
#                                        "Bay of Plenty Region" = "Rotorua",
#                                        "Southland Region" = "Invercargill",
#                                        "Marlborough Region" = "Blenheim") 
#     
#     covid_ta_tally.df <- as.data.frame(table(covid_ta_tally.df$Location))
#     colnames(covid_ta_tally.df)[1] <- "REGC2020_2"
#     
#     ta.sdf <- readOGR(dsn = "ta", layer = "territorial-authority-2020-clipped-generalised")
#     
#     virpal <- colorNumeric( palette = "viridis", domain = ta.sdf@data$Freq, na.color = "transparent")
#     
#     ta.sdf@data <- left_join(x = ta.sdf@data,
#                              y = covid_ta_tally.df,
#                              by = "REGC2020_2")
#     
#     ta.sdf@data$Freq[is.na(ta.sdf@data$Freq)] <- 0
#     
#     leaflet(data = ta.sdf) %>%
#         addPolygons(fillColor = ~virpal(ta.sdf@data$Freq),
#                     weight = 1,
#                     opacity = 1,
#                     color = "white",
#                     dashArray = "3",
#                     fillOpacity = 0.7,
#                     highlight = highlightOptions(
#                         weight = 5,
#                         color = "#666",
#                         dashArray = "",
#                         fillOpacity = 0.7,
#                         bringToFront = TRUE),
#                     label = paste0(ta.sdf$REGC2020_2,":\n",ta.sdf$Freq),
#                     labelOptions = labelOptions(
#                         style = list("font-weight" = "normal", padding = "3px 8px"),
#                         textsize = "15px",
#                         direction = "auto")) %>% 
#         addLegend(pal = virpal, values = ~Freq, opacity = 0.7, title = "COVID19 Cases",
#                   position = "bottomright") %>%
#         setView(lng = 172.8859711, lat = -39.9005585, zoom = 6)
# })

#coords.df <- read_csv(file = "loc.csv")
#coords.df$lat <- as.numeric(coords.df$lat)
#covid_marker_tally.df <- as.data.frame(table(covid_marker.df$Location))
#colnames(covid_marker_tally.df)[1] <- "Location"
#coords.df <- left_join(coords.df,covid_marker_tally.df,by = "Location")
#coords.df

# map_title <- tags$div(
#     HTML('<img border="0" alt="ImageTitle" src="/www/ImageR.jpeg" width="300" height="100">')
# )  

# url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
# covid_ts.df <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
# covid_ts.df <- covid_ts.df %>% filter(`Country/Region` == "New Zealand") %>% select(-c(Lat,Long,`Province/State`)) 
# covid_ts.df <- covid_ts.df %>% rename(Country = `Country/Region`)
# coivd_ts.df <- melt(covid_ts.df)
# covid_ts.df <- coivd_ts.df %>% filter(value != 0)

#save(covid_ts.df,file = "covid_ts.df")
#load("covid_ts.df")
#covid_ts.df$variable <- as.character(covid_ts.df$variable)
#covid_ts.df$value[25] <- 102
#covid_ts.df <- rbind(covid_ts.df,c("New Zealand","3/24/20",155))
#
#    	# save(dhb.sdf,file = "dhb_spatial.rds")
# load("dhb_spatial.rds")
# 
# #Optimise geometry for load times
# 
# sg <- rgeos::gSimplify(dhb.sdf,tol=0.01, topologyPreserve=TRUE)
# dhb_simple.sdf <- SpatialPolygonsDataFrame(sg, data=dhb.sdf@data)
# save(dhb_simple.sdf,file = "dhb_spatial_simple.rds")
#load("dhb_spatial.rds")
#dhb.sdf <- dhb_simple.sdf