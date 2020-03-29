# output$new_cases_plot <- renderPlotly({
# 	
# 	covid.df <- covid_loc.df()
# 	covid.df %<>%	na.omit()
# 	
# 	nc.g <- ggplot(data = covid.df) +
# 		geom_col(mapping = aes(x = DHB,y = value,fill = variable,stat = 'identity'),position = 'dodge') + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
# 		labs(title = "NZ COVID19 cases - Age and Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Age",y = "Number of cases") +
# 		scale_fill_viridis(discrete = T) +
# 		theme_light(base_size = text_size) + theme(legend.position = "bottom") +
# 		theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
# 	
# 	nc.g %>% 
# 		ggplotly(tooltip = c("Cases","n")) %>% 
# 		config(displayModeBar = F) %>% 
# 		layout(title = list(text = paste0('NZ COVID19 Cases: Total',
# 																			'<br>',
# 																			'<sup>',
# 																			date_stamp,
# 																			'</sup>')),
# 					 uniformtext=list(minsize=plotly_text_size, mode='hide')) 
# })

# Source Time Series data: <a href = "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv">John Hopkins University Centre for Systems Science and Engineering - Time Series Data Source</a><br>')  

#fct_infreq(covid.df$Age, ordered = NA)                                  
#write.csv(covid.df,"covid19.csv")

#	covid_p.df$Case <- paste(covid_p.df$Case,"probable")

# tab-time-series
# tabItem(tabName = "time_series",
#         fluidRow(
#             tags$br(),
#             

#tabPanel("MoH Map",imageOutput("moh_map"),width = 12),width = 12)

#column(6,h5(a("Check covid19.govt.nz for key information"),href = "https:\\covid19.govt.nz"),align = "center", style="color:yellow")
#column(6,h5())

# tabBox(
# title = "Choropleth: COVID19 by DHB",
#     id = "mapTabs",
#     height = "600px",
#     tabPanel("Map & Time Series",

## main dash board tab - maps & time series --------------------------------------
# fluidRow(
#     #box(h3("Cases by dhb Council"),width = 6),
#     #box(h3("Cases by DHB"),width = 12)
# ),

# fluidRow(
# 	infoBox("Total Cases", total_cases , icon = icon("arrow-up"),
# 					width = 2,color = "red"),
# 	infoBox("Total 24Hrs", total_cases_new_24,icon = icon("exclamation-triangle"),
# 					width = 2,color = "red"),
# 	infoBox("Confirmed", confirmed_cases , icon = icon("clock"),
# 					width = 2,color = "orange"),
# 	infoBox("Confirmed 24Hrs", confirmed_cases_new_24  , icon = icon("user-clock"),
# 					width = 2,color = "orange"),
# 	infoBox("Probable Cases", probable_cases, icon = icon("question"),
# 					width = 2,color = "aqua"),
# 	infoBox("Probable 24Hrs", probable_cases_new_24, icon = icon("user-clock"),
# 					width = 2,color = "aqua")
# ),
# fluidRow(
# 	infoBox("Recovered Cases", recovered_cases, icon = icon("walking"),
# 					width = 3,color = "green"),
# 	infoBox("Recovered 24Hrs", recovered_cases_new_24 , icon = icon("accessible-icon"),
# 					width = 3,color = "green"),
# 	infoBox("In Hospital", in_hospital, icon = icon("hospital"),
# 					width = 3,color = "maroon"),
# 	infoBox("Alert Level", alert_level, icon = icon("bell"),
# 					width = 3,color = "black")
# ),

# joined.df %<>% 
# 	na.omit()

#menuItem("Time Series", tabName = "time_series", icon = icon("clock")),
#menuItem("Total Confirmed", tabName = "new", icon = icon("external-link-square-alt")),

# keep.df <- rbind(
# 	joined.df %>% filter(`Last City before NZ` == "Cairo" & country == "Egypt"),
# 	joined.df %>% filter(`Last City before NZ` == "Dublin" & country == "Ireland"),
# 	joined.df %>% filter(`Last City before NZ` == "Geneva" & admin_name == "Switzerland"),
# 	joined.df %>% filter(`Last City before NZ` == "London" & admin_name == "United Kingdom"),
# 	joined.df %>% filter(`Last City before NZ` == "Los Angeles" & admin_name == "California"),
# 	joined.df %>% filter(`Last City before NZ` == "Manchester" & country == "United Kingdom"),
# 	joined.df %>% filter(`Last City before NZ` == "Melbourne" & country == "Australia"),
# 	joined.df %>% filter(`Last City before NZ` == "New York" & country == "United States"),
# 	joined.df %>% filter(`Last City before NZ` == "Perth" & country == "Australia"),
# 	joined.df %>% filter(`Last City before NZ` == "Paris" & country == "France"),
# 	joined.df %>% filter(`Last City before NZ` == "San Francisco" & admin_name == "California"),
# 	joined.df %>% filter(`Last City before NZ` == "Southampton" & country == "United Kingdom"),
# 	joined.df %>% filter(`Last City before NZ` == "Sydney" & country == "Australia")
# )
# 
# joined.df %<>% filter(
# 	`Last City before NZ` %nin% c(
# 		"Cairo",
# 		"Dublin",
# 		"Geneva",
# 		"London",
# 		"Los Angeles",
# 		"Manchester",
# 		"Melbourne",
# 		"New York",
# 		"Perth",
# 		"Paris",
# 		"San Francisco",
# 		"Southampton",
# 		"Sydney"))


# remove everything after comma
#geo.df$`Last City before NZ` <- gsub(",.*","",geo.df$`Last City before NZ`)

#geo.df[geo.df$`Last City before NZ` == "California",]$`Last City before NZ`  <- "Los Angeles"

# uniformtext=list(minsize=plotly_text_size, mode='hide')

## MoH DHB Map Image---------
# output$moh_map <- renderImage({
#     #download.file(url = moh_map_url,
#     #              destfile = "www/moh_map.jpg")
#     list(src = "www/moh_map.jpg",
#          contentType = 'image/jpg',
#          width = 1142/2.5,
#          height = 1454/2.5,
#          alt = "MoH Map")
# },deleteFile = F)
# 
# covid_ts.df <- read.csv(file = "covid_ts.csv",header = T)
# covid_ts.df$variable <- as.character(covid_ts.df$variable)
# covid_ts.df <- rbind(covid_ts.df,c("New Zealand","3/25/20",205))
# covid_ts.df <- rbind(covid_ts.df,c("New Zealand","3/26/20",283))
# covid_ts.df <- rbind(covid_ts.df,c("New Zealand","3/27/20",368))
# covid_ts.df <- rbind(covid_ts.df,c("New Zealand","3/28/20",451))
# 
# covid_ts.df$variable <- as.factor(covid_ts.df$variable)
# covid_ts.df$value <- as.numeric(covid_ts.df$value)
# covid_ts.df
# 
# covid.lag <- lag(covid_ts.df$value,1)
# covid.lag[is.na(covid.lag)] <- 0
# 
# covid_ts.df$new_cases <- covid_ts.df$value - covid.lag

# covid_ts.df # write.csv(x = covid_ts.df,file = "covid_ts.csv",quote = F,row.names = F)

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
# }

#geom_line(mapping = aes(x = variable,y = new_cases,group = 1)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
#
#		#geom_text(data = tail(nc.df),aes(x = variable,y = new_cases + max(new_cases)/20,label = new_cases))
#scale_x_date(breaks = ts.df$variable[seq(1, length(ts.df$variable), by = 3)])
#
##annotate(geom = "text", x = 1, y = max(ts.df$value)/2, label = paste0("N = ",nrow(ts.df)),color = "black") +

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