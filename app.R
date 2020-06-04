# Author: Matthew Skiffington
# Purpose: Server Code for COVID19 Data Explorer - runs a shiny app

# Define UI for application 
## UI -----------

excel_file <- "moh_data.xlsx"
source("global.R")
ui <- dashboardPage(
	dashboardHeader(title = app_title),
	
	## UI: Side bar content ----------------
	dashboardSidebar(
	  
	  tags$head(
	    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
	  ),
	  
		sidebarMenu(
			menuItem("Figures & Maps", tabName = "dashboard", icon = icon("dashboard")),
			menuItem("World Map", tabName = "world_map", icon = icon("globe")),
			menuItem("Age", tabName = "age", icon = icon("birthday-cake")),
			menuItem("Transmission", tabName = "transmission", icon = icon("user-friends")),
			#menuItem("Ethnicity", tabName = "ethnicity", icon = icon("user-friends")),
			menuItem("DHB Level Info",tabName = "dhb_splits",
							 #menuItem("Ethnicity", tabName = "ethnicity", icon = icon("user-friends")),
							 menuSubItem("Time Series", tabName = "time_dhb", icon = icon("chart-line")),
							 menuSubItem("Cases", tabName = "dhb", icon = icon("arrows-alt")),
							 menuSubItem("Travel over Time", tabName = "dhb_travel_time", icon = icon("chart-line")),
							 menuSubItem("International Travel", tabName = "dhb_travel", icon = icon("arrows-alt-h")),
							 # irrelevant now
							# menuSubItem("Hospitalisations", tabName = "dhb_hospital", icon = icon("hospital")),
							 menuSubItem("Gender", tabName = "dhb_gender", icon = icon("bookmark")),
							 menuSubItem("Age", tabName = "dhb_age", icon = icon("bookmark"))
			),
			menuItem("Gender", tabName = "gender", icon = icon("venus-mars")),
			menuItem("Age & Gender", tabName = "age_gender", icon = icon("bookmark")),
			menuItem("Raw Data Table", tabName = "raw_table", icon = icon("table")),
			menuItem("Clusters", tabName = "cluster_table", icon = icon("table")),
			menuItem("Additional Tables", tabName = "additional_tables", icon = icon("plus-square")),
			menuItem("Downloads",tabName = "downloads",icon = icon("download")),
			menuItem("About", tabName = "about", icon = icon("address-card")),
			menuItem(actionButton(inputId = "covidLink",
									 label = "covid19.govt.nz",
									 onclick = covid_open_link)),
			menuItem(actionButton(inputId = "moh_dash",
									 label = "MoH Dashboard",
									 onclick = moh_dash_link))
		)
	),
	## UI: Dasboard body ---------------------
	dashboardBody(
		tags$head(
			tags$link(rel = "shortcut icon", type ="image/x-icon", href="http://icons.iconarchive.com/icons/gosquared/flag/64/New-Zealand-flat-icon.png"),
			# Facebook OpenGraph tags
			tags$meta(property = "og:title", content = share$title),
			tags$meta(property = "og:type", content = "website"),
			tags$meta(property = "og:url", content = share$url),
			tags$meta(property = "og:image", content = share$image),
			tags$meta(property = "og:description", content = share$description),
			
			# Twitter summary cards
			tags$meta(name = "twitter:card", content = "summary"),
			tags$meta(name = "twitter:site", content = paste0("@", share$twitter_user)),
			tags$meta(name = "twitter:creator", content = paste0("@", share$twitter_user)),
			tags$meta(name = "twitter:title", content = share$title),
			tags$meta(name = "twitter:description", content = share$description),
			tags$meta(name = "twitter:image", content = share$image)
		),
		# Change Theme
		shinyDashboardThemes(
			theme = "grey_dark"
		),
		# tab-dashboard
		tabItems(
			tabItem(tabName = "dashboard",
							## UI: - Map & Core Stats ---------------------------
							fluidRow(
								box(DT::dataTableOutput("core_stats_table"),width = 12)
							),
							fluidRow(
								column(12,h5(app_status,align = "center"))#,
							),
							fluidRow(
								box(leafletOutput("mapDHB",height = 600),width = 4),
								box(plotlyOutput("time_series_cumulative_plot", height = 600),width = 4),
								box(plotlyOutput("time_series_new_plot", height = 600),width = 4)#),
							)
			),
			## UI: Tabs - Plotly Plots ----------------------------------------------------
			tabItem(tabName = "world_map",
							fluidRow(
								box(plotlyOutput("world_map", height = 800),width = 12)
							)),
			# tab-time_dhb
			tabItem(tabName = "time_dhb",
							fluidRow(
								box(plotlyOutput("time_dhb", height = 800),width = 12)
							)),
			# tab-dhb_hospital
			tabItem(tabName = "dhb_hospital",
							fluidRow(
								box(plotlyOutput("hospital_dhb", height = 800),width = 12)
							)),
			# tab-age
			tabItem(tabName = "age",
							fluidRow(
								box(plotlyOutput("age_plot", height = 800),width = 12)
							)),
			# tab-transmission
			tabItem(tabName = "transmission",
							fluidRow(
								box(plotlyOutput("transmission_plot", height = 800),width = 12)
							)),
			# tab-dhb-travel
			tabItem(tabName = "dhb_travel",
							fluidRow(
								box(plotlyOutput("dhb_travel", height = 800),width = 12)
							)),
			# tab-dhb-travel
			tabItem(tabName = "dhb_travel_time",
							fluidRow(
								box(plotOutput("dhb_travel_time", height = 1000),width = 12)
							)),
			# tab-ethnicity
			# tab-ethnicity
			# tabItem(tabName = "ethnicity",
			# 				fluidRow(
			# 					box(plotlyOutput("ethnicity_plot", height = 800),width = 12),
			# 					helpText("MLAA = Middle Eastern, Latin America.")
			# 				)),
			# tab-dhb
			tabItem(tabName = "dhb",
							fluidRow(
								box(plotlyOutput("dhb_plot", height = 800),width = 12) #plotlyOutput
							)),
			# tab-gender
			tabItem(tabName = "gender",
							fluidRow(
								box(plotlyOutput("gender_plot", height = 800),width = 12)
							)),
			# tab-age-gender
			tabItem(tabName = "age_gender",
							fluidRow(
								box(plotlyOutput("age_gender_plot", height = 800),width = 12)
							)),
			# tab-dhb-gender
			tabItem(tabName = "dhb_gender",
							fluidRow(
								tags$br(),
								box(plotlyOutput("dhb_gender_plot", height = 800),width = 12)
							)),
			# tab-dhb-age
			tabItem(tabName = "dhb_age",
							fluidRow(
								box(plotlyOutput("dhb_age_plot", height = 800),width = 12)
							)),
			
			# tab-raw-tables
			tabItem(tabName = "raw_table",
							fluidRow(
								box(DT::dataTableOutput("raw_table"),width = 12)
							)),
			# tab-cluster-table
			tabItem(tabName = "cluster_table",
							fluidRow(
								box(DT::dataTableOutput("cluster_table"),width = 12)
							)),
			# tab-additional-tables
			tabItem(tabName = "additional_tables",
							fluidRow(
								box(DT::dataTableOutput("dhb_table"),width = 12)
							),
							fluidRow(
								box(DT::dataTableOutput("age_table"),width = 12)
							),
							fluidRow(
								box(DT::dataTableOutput("gender_table"),width = 12)
							)),
			# tab-about
			tabItem(tabName = "about",
							fluidRow(
								tags$br(),
								box(uiOutput("about", height = 800)))
			),
			# tab-downloads
			tabItem(tabName = "downloads",
							fluidRow(
								box(downloadButton(outputId = "download",
																	 label = "Download Raw Case Data"),width = 6),
								box(actionButton(inputId = "mohLink",
																 label = "Ministry of Health Cases Page",
																 onclick = moh_open_link),width = 6)
								
							), 
							fluidRow(
								box(downloadButton(outputId = "download_ts",
																	 label = "Download Raw Time Series Data"),width = 6),
								box(downloadButton(outputId = "download_loc",
																	 label = "Download DHB Data"),width = 6)
							)
			))
	)
)

# Define server logic 
server <- function(input, output,session) {
	
	rv <- reactiveValues()
	rv$run <- 0
	## Time Series Data Frame Creation ------------
	covid_ts.df <- eventReactive(eventExpr = c(input$updateButton,rv),
															 valueExpr = {
															   covid_ts.df <- read_excel(excel_file, sheet = 1, col_names = TRUE, na = "", skip = 3) %>%
															     rename(variable = `Date notified of potential case`) %>%
															     mutate(variable = as_date(parse_date_time(variable, orders = c("d m y", "d B Y", "dmy")))) %>%
															     group_by(variable) %>%
															     tally(name = "value") %>%
															     mutate(value = cumsum(value)) %>%
															     arrange(variable) %>%
															     mutate(Country = rep("New Zealand",nrow(.))) %>% #format(X$newdate, "%Y-%m-%d")
															     select(Country,variable,value)
															   
															  # covid_ts.df$variable <- as.character(format(covid_ts.df$variable,"%d/%m/%Y"))
															   covid_ts.df$value <- as.numeric(covid_ts.df$value)
															   covid_ts.df
															   
															   covid.lag <- lag(covid_ts.df$value,1)
															   covid.lag[is.na(covid.lag)] <- 0
															   
															   covid_ts.df$new_cases <- covid_ts.df$value - covid.lag
															   
															   covid_ts.df
															 })
	
	## Core Data Frame Creation ------------
	covid.df <- eventReactive(eventExpr = c(input$updateButton,rv),
														valueExpr = {
															
															covid.df <- read_excel(excel_file, sheet = 1, col_names = TRUE, na = "", skip = 3)
															covid_p.df <- read_excel(excel_file, sheet = 2, col_names = TRUE, na = "", skip = 3)
															
															covid_p.df %<>% rename(`Report Date` = `Date notified of potential case`)
															covid.df %<>% rename(`Report Date` = `Date notified of potential case`)
															
															
															covid.df <- rbind(covid.df,covid_p.df)
															
															covid.df$Gender <- covid.df$Sex
															covid.df$Gender[covid.df$Gender == ""] <- "Not Reported"
															covid.df$Gender <- as.factor(covid.df$Gender) 
															
															levels(covid.df$Gender)[levels(covid.df$Gender) == ""] <- "Not Reported"
															levels(covid.df$DHB)[levels(covid.df$DHB) == ""] <- "Not Reported"
															covid.df$DHB[covid.df$DHB == "TBC"] <- "Not Reported"
															
															covid.df$Age <- as.character(covid.df$`Age group`)
															covid.df$Age[covid.df$Age == ""] <- "Not Reported"
															covid.df$Age[covid.df$Age == "Teen"] <- "Teens"
															
															covid.df$Age[covid.df$Age == "Unknown"] <- "Not Reported"
															covid.df$Age <- as.factor(covid.df$Age) 
															
															levels(covid.df$Age)[levels(covid.df$Age) == "Not provided"] <- "Not Reported"
															
															
															covid.df <- covid.df %>%  mutate(Gender = recode(Gender, 
																																							 `Male` = "M",
																																							 `Female` = "F",
																																							 `Not provided` = "Not Reported"))
															
															
															# sort levels by frequency of DHB
															#	covid.df$DHB <- fct_recode(covid.df$DHB, c("Hawkes Bay" = "Hawke’s Bay")) 
															covid.df$DHB <- fct_infreq(covid.df$DHB, ordered = NA)
															#	covid.df$Age <- fct_recode(covid.df$Age, c("60s" = "64")) #fct_infreq(covid.df$Age, ordered = NA)     
															
															#write.csv(covid.df,"covid19.csv")
															#write.csv(covid.df,"covid19.csv")
															covid.df$Age <- fct_relevel(covid.df$Age, c("<1",
																																					"1 to 4",
																																					"5 to 9",
																																					"10 to 14",
																																					"15 to 19",
																																					"20 to 29",
																																					"30 to 39",
																																					"40 to 49",
																																					"50 to 59",
																																					"60 to 69",
																																					"70+",
																																					"Not Reported")) 
															
															covid.df$`Report Date` <- as_date(parse_date_time(covid.df$`Report Date`, 
															                                                  orders = c("d m y", "d B Y", "dmy")))
															covid.df
														})
	## DHB Spatial Reactive -------------------
	dhb.sdf <- reactive({
		dhb.sdf <- readOGR(dsn = "dhb", layer = "district-health-board-2015-2")
		dhb.sdf
	})
	## Map DHB -------------------
	output$mapDHB <- renderLeaflet({
		covid_dhb.df <- covid.df()
		
		covid_dhb.df %<>% 
			group_by(DHB) %>%
			tally()
		
		covid_dhb.df %<>% 
			mutate(value = as.numeric(n)) %>% 
			select(-n)
	
		colnames(covid_dhb.df)[1] <- "DHB2015_Na"
		dhb.sdf <- dhb.sdf()
		dhb.sdf <- dhb.sdf[dhb.sdf$DHB2015_Na != "Area outside District Health Board",]
		virpal <- colorNumeric( palette = "viridis", domain = dhb.sdf@data$value, na.color = "transparent")
		
		covid_dhb.df$DHB2015_Na <- as.character(covid_dhb.df$DHB2015_Na)
		covid_dhb.df$DHB2015_Na[covid_dhb.df$DHB2015_Na == "Waitematā"] <- "Waitemata"
		covid_dhb.df$DHB2015_Na[covid_dhb.df$DHB2015_Na == "Tairāwhiti"] <- "Tairawhiti"
		
		dhb.sdf@data <- left_join(x = dhb.sdf@data,
															y = covid_dhb.df,
															by = "DHB2015_Na")
		
		#dgb_centres <- gCentroid(dhb.sdf,byid = TRUE)
		#save(dgb_centres,file = "dgb_centres.rds")
		load("dgb_centres.rds")
		
		dhb.sdf@data$value[is.na(dhb.sdf@data$value)] <- 0
		
		leaflet(data = dhb.sdf,
						options = leafletOptions(minZoom = min_zoom, maxZoom = max_zoom,preferCanvas = T)) %>%
			addPolygons(fillColor = ~virpal(dhb.sdf@data$value),
									weight = 1,
									opacity = 1,
									color = "white",
									dashArray = "3",
									fillOpacity = 0.7,
									highlight = highlightOptions(
										weight = 5,
										color = "#666",
										dashArray = "",
										fillOpacity = 0.7,
										bringToFront = TRUE),
									label = paste0(dhb.sdf$DHB2015_Na,":\n",dhb.sdf$value),
									labelOptions = labelOptions(
										style = list("font-weight" = "normal", 
																 padding = "2px 2px"),
										textsize = "12px",
										direction = "auto")) %>% 
			addLegend(pal = virpal, values = ~dhb.sdf$value, opacity = 0.7, title = "COVID19 Cases by DHB",
								position = "bottomright") %>%
			addMarkers(
				lng = dgb_centres@coords[1:20,1], lat = dgb_centres@coords[1:20,2],
				label = paste(dhb.sdf$DHB2015_Na,": ",dhb.sdf$value),
				labelOptions = labelOptions(noHide = T, direction = "bottom",
																		style = list(
																			"color" = "red",
																			"font-family" = "serif",
																			"font-style" = "italic",
																			"box-shadow" = "3px 3px rgba(0,0,0,0.25)",
																			"font-size" = "10px",
																			"border-color" = "rgba(0,0,0,0.5)"
																		))) %>%
			setView(lng = lng_init, 
							lat = lat_init, 
							zoom = zoom_level_init) 
		#addControl(map_title, position = "topleft")
	})
	
	## COVID LOC Reactive -------
	covid_loc.df <- reactive({
		url <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases"
		#<- url %>%
		
		covid.ls <- read_html(url) %>% # "23_03_2020.html" # for static 
			html_table()
		
		covid_dhb.df <- melt(covid.ls[[2]])
		covid_dhb.df
		
	})
	## Time Series Plots -------------------
	output$time_series_cumulative_plot <- renderPlotly({
		
		## Cumulative Time Series ---------------
		ts.df <- covid_ts.df()
		
		# recode dates
		ts.df$variable <- as.Date(as.character(ts.df[,2]$variable),"%Y-%m-%d")
		
		ts.g <- ggplot(data = ts.df) +
			geom_line(mapping = aes(x = variable,y = value,group = 1)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
			geom_point(mapping = aes(x = variable,y = value,group = 1),size = 0.5) +
			labs(title = "COVID19 Cases (Cumulative)",
			     subtitle = paste0(date_stamp,data_note_1),
			     x = "Date of Report",
			     y = "Cumulative number of cases") +
			theme_bw() +
			#annotate(geom = "text", x = 1, y = max(ts.df$value)/2, label = paste0("N = ",nrow(ts.df)),color = "black") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = text_size)) +
			scale_x_date(breaks = seq(min(ts.df$variable), max(ts.df$variable), by = "4 day"), minor_breaks = "2 day",date_labels = "%d/%m") #+
		#geom_text(data = tail(ts.df),aes(x = variable - 0.5,y = value + max(new_cases)/20,label = value))
		#scale_x_date(breaks = ts.df$variable[seq(1, length(ts.df$variable), by = 3)])
		
		ts.g %>% 
			ggplotly() %>% #tooltip = c("Number of cases")
			config(displayModeBar = F) %>% 
			layout(title = list(text = paste0('COVID19 Cases (Cumulative)',
																				'<br>',
																				'<sup>',
																				date_stamp,data_note_1,
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'), 
						 margin = m)
	})
	
	## New Cases Time Series -------------------
	output$time_series_new_plot <- renderPlotly({
		nc.df <- covid_ts.df()
		
		# recode dates
		nc.df$variable <- as.Date(as.character(nc.df[,2]$variable),"%Y-%m-%d") #as.Date(nc.df[,2],		format = "%d/%m/%y")
		
		nc.g <- ggplot(data = nc.df) +
			#geom_line(mapping = aes(x = variable,y = new_cases,group = 1)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
			geom_col(mapping = aes(x = variable,y = new_cases,group = 1)) +
			labs(title = 'COVID19 Cases: New Cases',
			     subtitle = paste0(date_stamp,data_note_1),
			     x = "Date of Report",
			     y = "Number of new cases") +
			theme_bw() +
			#annotate(geom = "text", x = 1, y = max(ts.df$value)/2, label = paste0("N = ",nrow(ts.df)),color = "black") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = text_size)) +
			scale_x_date(breaks = seq(min(nc.df$variable), max(nc.df$variable), by = "4 day"), minor_breaks = "2 day",date_labels = "%d/%m") #+
		#geom_text(data = tail(nc.df),aes(x = variable,y = new_cases + max(new_cases)/20,label = new_cases))
		#scale_x_date(breaks = ts.df$variable[seq(1, length(ts.df$variable), by = 3)])
		
		nc.g %>%
			ggplotly() %>% #tooltip = c("Number of cases")
			config(displayModeBar = F) %>%
			layout(title = list(text = paste0('COVID19 Cases: New Cases',
																				'<br>',
																				'<sup>',
																				date_stamp,data_note_1,
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'), 
						 margin = m)
	})
	# ## Time Series by DHB -------------
	output$time_dhb <- renderPlotly({
		
		m <- list(
			l = 50,
			r = 50,
			b = 100,
			t = 100,
			pad = 4
		)
		
		ts_r.df <- covid.df()
		
		ts_rc.df <- ts_r.df %>% 
			group_by(DHB,`Report Date`) %>% 
			tally() %>% 
			mutate(nc = cumsum(n)) %>%
			ungroup()
		
		ts_rc.g <- ggplot(data = ts_rc.df) +
			geom_line(mapping = aes(x = `Report Date`,y = nc,group = DHB)) + 
			geom_point(mapping = aes(x = `Report Date`,y = nc,group = DHB),size = 0.75) +
			labs(title = "NZ COVID19: Cumulative cases by DHB",subtitle = "Using Report Date",x = "",y = "Cumulative number of cases") +
			theme_bw() +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = text_size)) +
			facet_wrap(~DHB) +
			scale_x_date(breaks = seq(min(ts_rc.df$`Report Date`), max(ts_rc.df$`Report Date`), by = "4 day"), minor_breaks = "2 day",date_labels = "%d/%m") #+
		
		ts_rc.g %>%
			ggplotly() %>% #tooltip = c("Number of cases")
			config(displayModeBar = F) %>%
			layout(title = list(text = paste0('NZ COVID19 Cases: Cumulative Cases by DHB and Report Date',
																				'<br>',
																				'<sup>',
																				date_stamp,data_note_1,
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'),margin = m)
	})
	# ## World Map - Links to NZ -------------------
	output$world_map <- renderPlotly({
		
		m_world <- list(
			l = 50,
			r = 50,
			b = 50,
			t = 150,
			pad = 2
		)
		
		geo.df <- covid.df()
		cases_no <- nrow(geo.df)
		
		geo.df %<>% 
			rename(Country = `Last country before return`) %>%
				filter(!is.na(Country))
		
		#cities.df <- read.csv('worldcities.csv')
		countries.df <- read.csv('countries.csv')
		
		geo.df$Country <- as.character(geo.df$Country)
		geo.df$Country[geo.df$Country == "Polynesia (excludes Hawaii)"] <- "Vanuatu"
		geo.df$Country[geo.df$Country == "Hong Kong (Special Administrative Region)"] <- "Hong Kong"
		geo.df$Country[geo.df$Country == "Viet Nam"] <- "Vietnam"
		geo.df$Country[geo.df$Country == "Northern America"] <- "United States of America"
		geo.df$Country[geo.df$Country == "Middle East"] <- "Qatar"
		geo.df$Country[geo.df$Country == "England"] <- "United Kingdom"
		
		countries.df$name <- as.character(countries.df$name)
		countries.df$name[countries.df$name == "United States"] <- "United States of America"
		
		countries.df %<>%  
			rename(lat = Latitude,lng = Longitude,Country = name) %>%
			select(lat,lng,Country) 
		
		joined.df <- left_join(geo.df,countries.df,by = "Country")
		
		joined.df %<>% 
			select(Country,lat,lng) %>%
			na.omit()
		
		tally.df <- joined.df %>% group_by(Country,lat,lng) %>% tally()
		
		nz_lat.vec <- rep(-39.095963,nrow(tally.df))
		nz_lng.vec <- rep(175.776594,nrow(tally.df))
		
		nz_orig.df <- data.frame(lat = nz_lat.vec,
														 lng = nz_lng.vec,
														 line = seq_len(nrow(tally.df)),
														 Country = rep("New Zealand",nrow(tally.df)),
														 id = seq_len(nrow(tally.df)))
		
		tally.df$id <- seq_len(nrow(tally.df))
		
		tally.df %<>% mutate(line = id) %>% select(lat,lng,line,Country,id,n) 
		
		nz_orig.df$n <- tally.df$n
		
		lines.df <- rbind(as.data.frame(tally.df),nz_orig.df)
		
		lines.df %<>% na.omit()
		
		overseas.df <- lines.df %>% filter(Country != "New Zealand")
		
		geo <- list(
			showland = TRUE,
			showlakes = TRUE,
			showcountries = TRUE,
			showocean = TRUE,
			countrywidth = 0.5,
			landcolor = toRGB("black"),
			lakecolor = toRGB("grey"),
			oceancolor = toRGB("grey"),
			projection = list(
				type = 'orthographic',
				rotation = list(
					lon = -100,
					lat = 40,
					roll = 0
				)
			),
			lonaxis = list(
				showgrid = TRUE,
				gridcolor = toRGB("gray40"),
				gridwidth = 0.5
			),
			lataxis = list(
				showgrid = TRUE,
				gridcolor = toRGB("gray40"),
				gridwidth = 0.5
			)
		)
		
		fig <- plot_geo(lines.df)
		fig <- fig %>% group_by(line)
		fig <- fig %>% add_lines(x = ~lng, y = ~lat,
														 hoverinfo = "text",
														 hovertext = paste("Country :", lines.df$Country,
														 									"<br> Longitude :", lines.df$lng,
														 									"<br> Longitude :", lines.df$lat,
														 									"<br> N :", lines.df$n),
														 size = I(1)) %>%
			add_markers(data = overseas.df,
									y=~lat, x=~lng, hoverinfo="text",
									color=~n, text=~n, size=~n, 
									hoverinfo = "text",
									hovertext = paste("Country :", overseas.df$Country,
																		"<br> Longitude :", overseas.df$lng,
																		"<br> Longitude :", overseas.df$lat,
																		"<br> N :", overseas.df$n),
									marker=list(sizeref=0.1, sizemode="area"))
		fig <- fig %>% layout(
			showlegend = FALSE, geo = geo,
			title = 'COVID19 Cases into NZ : Data from the Ministry of Health'
		) %>% 
			layout(title = list(text = paste0("COVID19 Cases into NZ : Data from the Ministry of Health",
																				'<br>',
																				'<sup>',
																				date_stamp," | Lines do not indicate flight paths | Includes confirmed & probable cases <br> 'Middle East' coded to Qatar | England coded to UK | 'Polynesia' coded to Tonga <br>",
																				cases_no - nrow(geo.df),"/",cases_no,
																				" people do not have prior overseas travel information",
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'), 
						 margin = m_world)  %>% 
			config(displayModeBar = F)
		
		fig
	})
	## Stacked Bar Charts ----------------
	## 
	#### DHB & Age ---------------
	output$dhb_age_plot <- renderPlotly({
		covid_main.df <- covid.df() %>%
			group_by(Age,DHB) %>%
			tally()
		
		main.g <- ggplot(data = covid_main.df) +
			geom_col(mapping = aes(x = DHB,y = n,fill = Age)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
			labs(title = "NZ COVID19 cases - DHB and Age",subtitle = paste(Sys.time(),Sys.timezone()),x = "DHB",y = "Number of cases") +
			scale_fill_viridis(discrete = T) +
			theme_light(base_size = text_size) + theme(legend.position = "bottom") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
		
		main.g %>% 
			ggplotly(tooltip = c("DHB","Age","n")) %>% 
			config(displayModeBar = F) %>% 
			layout(title = list(text = paste0('NZ COVID19 Cases: DHB and Age',
																				'<br>',
																				'<sup>',
																				date_stamp,data_note_1,
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'), 
						 margin = m) 
	})
	#### Age & Gender ---------------
	output$age_gender_plot <- renderPlotly({
		covid_main.df <- covid.df() %>%
			group_by(Age,Gender) %>%
			tally() %>% 
			na.omit()
		
		main.g <- ggplot(data = covid_main.df) +
			geom_col(mapping = aes(x = Age,y = n,fill = Gender)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
			labs(title = "NZ COVID19 cases: Age and Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Age",y = "Number of cases") +
			scale_fill_viridis(discrete = T) +
			theme_light(base_size = text_size) + theme(legend.position = "bottom") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
		
		main.g %>% 
			ggplotly(tooltip = c("Gender","n")) %>% 
			config(displayModeBar = F) %>% 
			layout(title = list(text = paste0('NZ COVID19: Cases by Age and Gender',
																				'<br>',
																				'<sup>',
																				date_stamp,data_note_1,
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'), 
						 margin = m) 
	}) 
	#### DHB & Gender ---------------
	output$dhb_gender_plot <- renderPlotly({
		covid_main.df <- covid.df() %>%
			group_by(DHB,Gender) %>%
			tally() %>% 
			na.omit()
		
		main.g <- ggplot(data = covid_main.df) +
			geom_col(mapping = aes(x = DHB,y = n,fill = Gender)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
			labs(title = "NZ COVID19 cases by DHB and Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "DHB",y = "Number of cases") +
			scale_fill_viridis(discrete = T) +
			theme_light(base_size = text_size) + theme(legend.position = "bottom") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
		
		main.g %>% 
			ggplotly(tooltip = c("Gender","n")) %>% 
			config(displayModeBar = F) %>% 
			layout(title = list(text = paste0('NZ COVID19: Cases by DHB and Gender',
																				'<br>',
																				'<sup>',
																				date_stamp,data_note_1,
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'), 
						 margin = m) 
	})
	## Plot - Age -------------------
	output$age_plot <- renderPlotly({
		covid_age.df <- covid.df() %>%
			group_by(Age) %>%
			tally() %>%
			na.omit()
		
		age.g <- ggplot(data = covid_age.df) +
			geom_col(mapping = aes(x = Age,y = n,fill = Age)) + # reorder(covid_age.df$Age, -n)
			labs(title = "NZ COVID19 cases - Age",subtitle = paste(Sys.time(),Sys.timezone()),x = "Age",y = "Number of cases") +
			scale_fill_viridis(discrete = T) +
			theme_light(base_size = text_size) + 
			theme(legend.position = "bottom") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
		
		age.g %>% ggplotly(tooltip = c("Age","n")) %>% 
			config(displayModeBar = F) %>% 
			layout(title = list(text = paste0('NZ COVID19: Cases by Age',
																				'<br>',
																				'<sup>',
																				date_stamp,data_note_1, " |",omission_string,
																				'</sup>')),
						 uniformtext = list(minsize=plotly_text_size, mode='hide'), 
						 margin = m) 
	})
	## Plot - Ethnicity -------------------
	# output$ethnicity_plot <- renderPlotly({
	# 	url <- main_moh_url
	# 	
	# 	covid.ls <- read_html(url) %>% # "23_03_2020.html" # for static 
	# 		html_table()
	# 	
	# 	ethnicity.df <- covid.ls[[1]]
	# 	
	# 	ethnicity.df$Ethnicity[ethnicity.df$Ethnicity == "Middle Eastern / Latin American / African"] <- "MLAA"
	# 	ethnicity.df$Ethnicity[ethnicity.df$Ethnicity == "European or Other"] <- "European / Other"
	# 	ethnicity.df$Ethnicity[ethnicity.df$Ethnicity == "Pacific People"] <- "Pacific"
	# 	
	# 	#covid.df$Ethnicity <- fct_infreq(covid.df$DHB, ordered = NA)
	# 	
	# 	age.g <- ggplot(data = ethnicity.df) +
	# 		geom_col(mapping = aes(x = fct_reorder(Ethnicity, -`No. of cases`),y = `No. of cases`,fill = Ethnicity)) + 
	# 		labs(title = "NZ COVID19 cases - Age",subtitle = paste(Sys.time(),Sys.timezone()),x = "Ethnicity",y = "Number of cases") +
	# 		scale_fill_viridis(discrete = T) +
	# 		theme_light(base_size = text_size) + 
	# 	  theme(legend.position = "bottom") +
	# 		scale_fill_viridis(discrete = T) +
	# 		theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
	# 	
	# 	age.g %>% ggplotly(tooltip = c("Age","n")) %>% 
	# 		config(displayModeBar = F) %>% 
	# 		layout(title = list(text = paste0('NZ COVID19: Cases by Ethnicity',
	# 																			'<br>',
	# 																			'<sup>',
	# 																			date_stamp,data_note_1,
	# 																			'</sup>')),
	# 					 uniformtext=list(minsize=plotly_text_size, mode='hide'), 
	# 					 margin = m) 
	# })
	# Plot - Transmission -------------------
	output$transmission_plot <- renderPlotly({
		url <- main_moh_url
		
		covid.ls <- read_html(url) %>% # "23_03_2020.html" # for static
			html_table()
		
		transmission.df <- covid.ls[[5]]
		#transmission.df %<>% rename(`Transmission type` = X1)
		#transmission.df %<>% rename(`% of cases` = X2)
		transmission.df$`% of cases` <- as.numeric(gsub(pattern = "%",
																										replacement = "",
																										x = transmission.df$`% of cases`))
		
		transmission.df <- transmission.df %>% 
			mutate(Proportion = "COVID19 NZ Transmission Type")
		
		# small rounding in MOH figures - scaled to 100
		#transmission.df$`% of cases` <- 100/sum(transmission.df$`% of cases`)*transmission.df$`% of cases`
		
		transmission.g <- ggplot(transmission.df, aes(x = Proportion, y = `% of cases`, fill = `Transmission type`)) +
			geom_col() +
			scale_fill_viridis(discrete = T) +
			theme_light(base_size = text_size) +
			labs(title = "NZ COVID19 cases - Transmission Type",subtitle = date_stamp,x = "",y = "(%) of cases") +
			#coord_flip() +
			theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust  = 2,size = text_size)) +
			scale_y_continuous(minor_breaks = seq(0 , 100, 10), breaks = seq(0, 100, 20)) +
			theme(legend.position = "bottom") + 
		  coord_flip()
		
		transmission.g %>% ggplotly(tooltip = c("Proportion","% of cases")) %>%
			config(displayModeBar = F) %>%
			layout(title = list(text = paste0('NZ COVID19 cases - Transmission Type',
																				'<br>',
																				'<sup>',
																				date_stamp,
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'),
						 margin = m)
	})
	## Plot - DHB -------------------
	output$dhb_plot <- renderPlotly({ #renderPlotly({
		
		dhb.df <- covid.df() #%>% filter(variable != "Total cases") #melt(covid.ls[[2]]) 
		
		dhb.df  %<>%
			group_by(DHB) %>%
			tally() %>% 
			filter(DHB != "Total") %>%
			na.omit()
		
		dhb.g <- ggplot(data = dhb.df) +
			geom_col(mapping = aes(x = reorder(dhb.df$DHB, -n),y = n,fill = DHB)) +
			labs(title = "NZ COVID19 cases - DHB",subtitle = paste(Sys.time(),Sys.timezone()),x = "Location",y = "Number of cases") +
			theme_light(base_size = text_size) +
			theme(legend.position = "bottom") +
			scale_fill_viridis(discrete = T) +
			theme(legend.position = "none") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
		
		dhb.g %>%
			ggplotly(tooltip = c("DHB","n")) %>%
			config(displayModeBar = F) %>%
			layout(title = list(text = paste0('NZ COVID19: Cases by DHB',
																				'<br>',
																				'<sup>',
																				date_stamp," unreported DHB cases omitted",
																				'</sup>')),
						 uniformtext = list(minsize = plotly_text_size, mode = 'hide'),
						 margin = m)
	})
	# ## Plot - Hospitalisations by DHB -------------------
	output$hospital_dhb <- renderPlotly({
		url <- main_moh_url
		
		covid.ls <- read_html(url) %>% # "23_03_2020.html" # for static
			html_table()
		
		hospital.df <- covid.ls[[3]]
		
		hospital.df %<>% filter(DHB != "Total")
		
		hospital.g <- ggplot(data = hospital.df) +
			geom_col(mapping = aes(x = fct_reorder(DHB, -`Total cases`),y = `Total cases`,fill = DHB)) +
			labs(title = "Hospitalisations by DHB",subtitle = "",x = "District Health Board",y = "Number of hospitalisations") +
			scale_fill_viridis(discrete = T) +
			theme_light(base_size = text_size) +
			theme(legend.position = "bottom") +
			scale_fill_viridis(discrete = T) +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
		
		hospital.g %>% ggplotly(tooltip = c("DHB","Number of Hospitalisations")) %>%
			config(displayModeBar = F) %>%
			layout(title = list(text = paste0('NZ COVID19: Current Hospitalisations by DHB',
																				'<br>',
																				'<sup>',
																				date_stamp,
																				'</sup>')),
						 uniformtext=list(minsize=plotly_text_size, mode='hide'),
						 margin = m)
	})
	## Plot - DHB / Travel -------------------
	output$dhb_travel <- renderPlotly({ #renderPlotly({

		dhb.df <- covid.df() #%>% filter(variable != "Total cases") #melt(covid.ls[[2]])

		dhb.df$`Overseas travel` <- fct_explicit_na(fct_explicit_na(dhb.df$`Overseas travel`, na_level = "Not Reported"))

		dhb.df  %<>%
			group_by(DHB,`Overseas travel`) %>%
			tally() %>%
			filter(DHB != "Total") %>%
			na.omit()

		dhb.g <- ggplot(data = dhb.df) +
			geom_col(mapping = aes(x = DHB,y = n,fill = `Overseas travel`,group = `Overseas travel`)) +
			labs(title = "NZ COVID19 cases - DHB / Travel",subtitle = "",x = "Location",y = "Number of cases") +
			theme_light(base_size = text_size) +
			theme(legend.position = "bottom") +
			scale_fill_viridis(discrete = T,name = "International\nTravel") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size)) 

		dhb.g %>%
			ggplotly(tooltip = c("DHB","n","Overseas travel")) %>%
			config(displayModeBar = F) %>%
			layout(title = list(text = paste0('NZ COVID19: Cases by DHB and Travel Type',
																				'<br>',
																				'<sup>',
																				date_stamp," unreported DHB cases omitted",
																				'</sup>')),
						 uniformtext = list(minsize = plotly_text_size, mode = 'hide'),
						 margin = m)
	})
	## Plot - DHB / Travel / Time -------------------
	output$dhb_travel_time <- renderPlot({ #renderPlotly({
		
		# m <- list(
		# 	l = 50,
		# 	r = 50,
		# 	b = 100,
		# 	t = 100,
		# 	pad = 4
		# )
		
		dhb.df <- covid.df() #%>% filter(variable != "Total cases") #melt(covid.ls[[2]])

		dhb.df$`Overseas travel` <- fct_explicit_na(
			fct_explicit_na(dhb.df$`Overseas travel`, na_level = "Not Reported"))

		dhb.df  %<>%
			group_by(DHB,`Overseas travel`,`Report Date`) %>%
			tally() %>%
			filter(DHB != "Total") %>%
			mutate(nc = cumsum(n)) %>%
			ungroup() %>%
			na.omit()

		dhb.g <- ggplot(data = dhb.df) +
			geom_line(mapping = aes(x = `Report Date`,y = nc,color = `Overseas travel`,group = `Overseas travel`)) +
				geom_point(mapping = aes(x = `Report Date`,y = nc,color = `Overseas travel`,group = `Overseas travel`),size = 0.75) +
			labs(title = "NZ COVID19 cases - DHB / Travel",subtitle = "",x = "",y = "Cumulative number of cases") +
			theme_bw(base_size = text_size + 2) +
			theme(legend.position = "bottom") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size)) +
			facet_wrap(~DHB) +
			scale_fill_viridis(discrete = T) 
		
		dhb.g

		# dhb.g %>%
		# 	ggplotly(tooltip = c("DHB","n")) %>%
		# 	config(displayModeBar = F) %>%
		# 	layout(title = list(text = paste0('NZ COVID19: Cumulative Cases by DHB and Travel Type, over time',
		# 																		'<br>',
		# 																		'<sup>',
		# 																		date_stamp," unreported DHB cases omitted",
		# 																		'</sup>')),
		# 				 uniformtext = list(minsize = plotly_text_size, mode = 'hide'),
		# 				 margin = m)
	})
	## Plot - Gender -------------------
	output$gender_plot <- renderPlotly({
		covid_gender.df <- covid.df() %>%
			group_by(Gender) %>%
			tally() %>% 
			na.omit()
		
		gender.g <- ggplot(data = covid_gender.df) +
			geom_col(mapping = aes(x = reorder(covid_gender.df$Gender, -n),y = n,fill = Gender)) +
			labs(title = "NZ COVID19 cases - Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Gender",y = "Number of cases") +
			scale_fill_viridis(discrete = T) +
			theme_light(base_size = text_size) + 
			theme(legend.position = "bottom") +
			theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
		
		gender.g %>% ggplotly(tooltip = c("Gender","n")) %>% 
			config(displayModeBar = F) %>%
			layout(title = list(text = paste0('NZ COVID19: Cases by Gender',
																				'<br>',
																				'<sup>',
																				date_stamp,data_note_1," | unreported gender cases omitted",
																				'</sup>')),
						 uniformtext = list(minsize = plotly_text_size, mode='hide'), 
						 margin = m) 
	})
	## Tables -------------------
	## 
	#### Core Stats Table --------------------------
	output$core_stats_table <- DT::renderDataTable({
		url <- main_moh_url
		
		covid.ls <- read_html(url) %>% # "23_03_2020.html" # for static 
			html_table()
		
		core_stats.df <- covid.ls[[1]]
		
		DT::datatable(core_stats.df,
									options = list(dom = "t"),colnames = c("Statistic","Total","Change in 24 hours")) 
	})
	#### Raw Data Table --------------------------
	output$raw_table <- DT::renderDataTable({
		df <- covid.df()
		
		DT::datatable(df,options = list(
			pageLength = 60)) %>% 
		  formatStyle(colnames(.),color = table_text_colour)
	})
	output$cluster_table = DT::renderDataTable({
		
		url <- cluster_moh_url
		
		covid.ls <- read_html(url) %>% # "23_03_2020.html" # for static 
			html_table()
		
		covid.df <- covid.ls[[1]]
		
		df <- covid.df
		
		DT::datatable(df,options = list(
			pageLength = 60)) %>% 
		  formatStyle(colnames(.),color = table_text_colour)
	})
	#### DHB Table --------------------------
	output$dhb_table = DT::renderDataTable({
		
		covid_dhb.df <- covid.df() %>%
			group_by(DHB) %>%
			tally()
		
		DT::datatable(covid_dhb.df,options = list(
			pageLength = 60)) %>% 
		  formatStyle(colnames(.),color = table_text_colour)
	})
	#### Gender Table --------------------------
	output$gender_table = DT::renderDataTable({
		
		covid_gender.df <- covid.df() %>%
			group_by(Gender) %>%
			tally()
		
		DT::datatable(covid_gender.df,options = list(
			pageLength = 60)) %>% 
		  formatStyle(colnames(.),color = table_text_colour)
	})
	#### Age Table --------------------------
	output$age_table = DT::renderDataTable({
		
		covid_age.df <- covid.df() %>%
			group_by(Age) %>%
			tally()
		
		DT::datatable(covid_age.df,options = list(
			pageLength = 60)) %>% 
		  formatStyle(colnames(.),color = table_text_colour)
	})
	
	# Downloadable csv of cases dataset ----
	output$download <- downloadHandler(
		filename = function() {
			paste("covid_19_cases_nz_",as.numeric(Sys.time()),".csv", sep = "")
		},
		content = function(file) {
			write.csv(covid.df(), file, row.names = FALSE)
		}
	)
	# Downloadable csv of time series dataset ----
	output$download_ts <- downloadHandler(
		filename = function() {
			paste("covid_19_timeseries_nz_",as.numeric(Sys.time()),".csv", sep = "")
		},
		content = function(file) {
			write.csv(covid_ts.df(), file, row.names = FALSE)
		}
	)
	# Downloadable csv of DHB dataset ----
	output$download_loc <- downloadHandler(
		filename = function() {
			paste("covid_19_DHB_nz_",as.numeric(Sys.time()),".csv", sep = "")
		},
		content = function(file) {
			write.csv(covid_loc.df(), file, row.names = FALSE)
		}
	)
	
	## About Section -------------------
	output$about <- renderUI({
		HTML('<a href = "https://covid19.govt.nz/">covid19.govt.nz</a><br>
				 Source Code: <a href = "https://github.com/MattSkiff/covid19_nz_data">Shiny App GitHub Repo</a><br> 
				 Source Ministry of Health data: <a href = "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases">Ministry of Health Data, Maps and Charts</a><br>
				 This tool was developed as a personal project and is not official. Please check the Ministry of Health for all official statistics.<br><br>
				 On Saturday, the 21st of March, 2020, Alert Level 2 in New Zealand was announced. Data visualisation and tool kits in New Zealand was limited at this stage. 
				 Consequently, I decided to develop a simple tool to view cases day by day, and by gender and region. Since then, I have been and continue to add features as time permits and as new data is available. <br><br>
				 The purpose of this tool is to visualise the descriptive statistics released by the Ministry of Health. <br> 
				 This tool does not perform any predictive or inferential modelling and has been written by a non-expert.<br> 
				 Best viewed on a desktop PC - this tool is not optimised for mobile. <br>
				 Data is sourced from Ministry of Health Excel Spreadsheets and scraped from some web tables, which are updated daily.<br> 
				 <br> 
				 Made by Matthew Skiffington <br> 
				 Contact: skiffcoffee@gmail.com. Feedback and suggestions are welcome.')
	})
	
	}


shinyApp(ui = ui, server = server)
# 
# library(gridExtra)
# png("today.png",
# width = 1400,
# height = 800,
# type = "cairo")
# grid.arrange(ts.g,nc.g,ncol = 2)
# dev.off()