library(rvest) # scraping MoH
library(rgdal) # geo stuff
library(leaflet) # interactive map
library(shiny) # app
library(reshape2) # melting
library(dplyr) # wrangling
library(magrittr) # piping
library(ggplot2) # core plot package
library(viridis) # nice colour scale
library(forcats) # factors
library(plotly) # interactive viz
library(DT) # for interactive data tables
library(readr) # to nicely read in data
library(shinydashboard)
library(dashboardthemes)
library(readr)
# library(htmlwidgets)
# library(htmltools)

## Options --------------------
# set timezone
# set text size on x axis
text_size = 8
zoom_level_init = 5.5
lat_init = -39.9005585
lng_init = 172.8859711
min_zoom = 2
max_zoom = 8
moh_open_link <- "window.open('https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases', '_blank')"

## Case Summary Information ---------------
# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases
total_cases <- 205
total_cases_new_24 <- 50 
    
confirmed_cases <- 189
confirmed_cases_new_24 <- 47
    
probable_cases <- 16
probable_cases_new_24 <- 3
    
recovered_cases <- 22
recovered_cases_new_24 <- 10
    
community_cases <- 4

alert_level <- 4
###

# Projection: WGS 84 (EPSG:4326 Geographic)

# Useful Links
# https://datascott.com/blog/subtitles-with-ggplotly/
# https://datafinder.stats.govt.nz/search/?q=territorial%2Bclipped
# https://rstudio.github.io/leaflet/markers.html

date_stamp <- "Current to 25/03/2020"

# Define UI for application 
## UI -----------
## Body content
ui <- dashboardPage(
    dashboardHeader(title = "NZ COVID19 Data Explorer"),
    
    ## Side bar content ----------------
    dashboardSidebar(
        sidebarMenu(
            menuItem("Figures & Maps", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Time Series", tabName = "time_series", icon = icon("clock")),
            menuItem("Age", tabName = "age", icon = icon("birthday-cake")),
            menuItem("Region", tabName = "region", icon = icon("arrows-alt")),
            menuItem("Gender", tabName = "gender", icon = icon("venus-mars")),
            menuItem("Age & Gender", tabName = "age_gender", icon = icon("bookmark")),
            menuItem("Region & Gender", tabName = "region_gender", icon = icon("bookmark")),
            menuItem("Region & Age", tabName = "region_age", icon = icon("bookmark")),
            menuItem("Raw Data Table", tabName = "raw_table", icon = icon("table")),
            menuItem("Additional Tables", tabName = "additional_tables", icon = icon("plus-square")),
            menuItem("Downloads",tabName = "downloads",icon = icon("download")),
            menuItem("About", tabName = "about", icon = icon("address-card"))
        )
    ),
    ## Dasboard body ---------------------
    dashboardBody(
        # Change Theme
        shinyDashboardThemes(
            theme = "purple_gradient" #"grey_dark"
        ),
        # tab-dashboard
        tabItems(
            tabItem(tabName = "dashboard",
                    ## info boxes ---------------------------
                    fluidRow(
                            fluidRow(
                                infoBox("Total Cases", total_cases , icon = icon("arrow-up"),
                                        width = 2,color = "red"),
                                infoBox("Total (24 hours)", total_cases_new_24,icon = icon("exclamation-triangle"),
                                        width = 2,color = "orange"),
                                infoBox("Confirmed Cases", confirmed_cases , icon = icon("clock"),
                                        width = 2,color = "red"),
                                infoBox("Confirmed (24 hours)", confirmed_cases_new_24  , icon = icon("user-clock"),
                                        width = 2,color = "orange"),
                                infoBox("Probable Cases", probable_cases, icon = icon("question"),
                                        width = 2,color = "orange"),
                                infoBox("Probable (24 hours)", probable_cases_new_24, icon = icon("user-clock"),
                                        width = 2,color = "orange")
                            ),
                            fluidRow(
                                infoBox("Recovered Cases", recovered_cases, icon = icon("walking"),
                                        width = 3,color = "green"),
                                infoBox("Recovered \n(24 hours)", recovered_cases_new_24 , icon = icon("accessible-icon"),
                                        width = 3,color = "olive"),
                                infoBox("Community Cases", community_cases, icon = icon("home"),
                                        width = 3,color = "maroon"),
                                infoBox("Alert Level", alert_level, icon = icon("bell"),
                                        width = 3,color = "black")
                            ),
                            ## maps --------------------------------------
                            fluidRow(
                                box(h3("Cases by Regional Council"),width = 6),
                                box(h3("Cases by Location"),width = 6)
                            ),
                            fluidRow(
                                box(leafletOutput("mapRC",height = 600),width = 6),
                                box(leafletOutput("markerRC",height = 600),width = 6)
                            )
                        )
                    ),
            ## plotly plots ----------------------------------------------------
            
            # tab-time-series
            tabItem(tabName = "time_series",
                    fluidRow(
                        tags$br(),
                        box(plotlyOutput("time_series_cumulative_plot", height = 800),width = 6),
                        box(plotlyOutput("time_series_new_plot", height = 800),width = 6))),
            # tab-age
            tabItem(tabName = "age",
                    fluidRow(
                        tags$br(),
                        box(plotlyOutput("age_plot", height = 600),width = 12)
                        )),
            # tab-region
            tabItem(tabName = "region",
                    fluidRow(
                        tags$br(),
                        box(plotlyOutput("region_plot", height = 600),width = 12)
                        )),
            # tab-gender
            tabItem(tabName = "gender",
                    fluidRow(
                        tags$br(),
                        box(plotlyOutput("gender_plot", height = 800),width = 12)
                        )),
            # tab-age-gender
            tabItem(tabName = "age_gender",
                    fluidRow(
                        tags$br(),
                        box(plotlyOutput("age_gender_plot", height = 800),width = 12)
                        )),
            # tab-region-gender
            tabItem(tabName = "region_gender",
                    fluidRow(
                        tags$br(),
                        box(plotlyOutput("region_gender_plot", height = 800),width = 12)
                        )),
            # tab-region-age
            tabItem(tabName = "region_age",
                    fluidRow(
                        tags$br(),
                        box(plotlyOutput("region_age_plot", height = 800),width = 12)
                        )),
    
            # tab-tables
            tabItem(tabName = "raw_table",
                    fluidRow(
                        tags$br(),
                        box(DT::dataTableOutput("raw_table"),width = 12)
                        )),
            # tab-tables
            tabItem(tabName = "additional_tables",
                    fluidRow(
                        tags$br(),
                        box(DT::dataTableOutput("region_table"),width = 12)
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
                                           label = "Download Location Data"),width = 6)
                    )
        ))
    )
)

# Define server logic 
server <- function(input, output,session) {
    
    rv <- reactiveValues()
    rv$run <- 0
    ## Main Scraping and Dataframe ------------
    covid_ts.df <- eventReactive(eventExpr = c(input$updateButton,rv),
                                 valueExpr = {
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
                                     covid_ts.df <- read.csv(file = "covid_ts.csv",header = T)
                                     covid_ts.df$variable <- as.character(covid_ts.df$variable)
                                     covid_ts.df <- rbind(covid_ts.df,c("New Zealand","3/25/20",205))
                                     
                                     covid_ts.df$variable <- as.factor(covid_ts.df$variable)
                                     covid_ts.df$value <- as.numeric(covid_ts.df$value)
                                     covid_ts.df
                                     
                                     covid.lag <- lag(covid_ts.df$value,1)
                                     covid.lag[is.na(covid.lag)] <- 0
                                     
                                     covid_ts.df$new_cases <- covid_ts.df$value - covid.lag
                                     
                                     covid_ts.df
                                     
                                     # write.csv(x = covid_ts.df,file = "covid_ts.csv",quote = F,row.names = F)
                                 })
    
    covid.df <- eventReactive(eventExpr = c(input$updateButton,rv),
                              valueExpr = {
                                  # data gen
                                  #download.file("https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases",destfile="t.html")
                                  
                                  url <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases"
                                  #<- url %>%
                                  
                                  
                                  covid.ls <- read_html(url) %>% # "23_03_2020.html" # for static 
                                      html_table()
                                  
                                  covid.df <- covid.ls[[1]]
                                  covid_p.df <- covid.ls[[2]]
                                  
                                  covid_p.df$Case <- paste(covid_p.df$Case,"probable")
                                  
                                  covid.df <- rbind(covid.df,covid_p.df)
                                  
                                  covid.df$Gender <- as.character(covid.df$Gender)
                                  covid.df$Gender[covid.df$Gender == ""] <- "Not Reported"
                                  covid.df$Gender <- as.factor(covid.df$Gender) 
                                  
                                  levels(covid.df$Gender)[levels(covid.df$Gender) == ""] <- "Not Reported"
                                  levels(covid.df$Location)[levels(covid.df$Location) == ""] <- "Not Reported"
                                  covid.df$Location[covid.df$Location == "TBC"] <- "Not Reported"
                                  
                                  covid.df$Age <- as.character(covid.df$Age)
                                  covid.df$Age[covid.df$Age == ""] <- "Not Reported"
                                  covid.df$Age <- as.factor(covid.df$Age) 
                                  
                                  levels(covid.df$Age)[levels(covid.df$Age) == "Not provided"] <- "Not Reported"
                                  
                                  
                                  covid.df <- covid.df %>%  mutate(Gender = recode(Gender, 
                                                                                   `Male` = "M",
                                                                                   `Female` = "F",
                                                                                   `Not provided` = "Not Reported"))
                                  
                                  
                                  # sort levels by frequency of location
                                  covid.df$Location <- fct_recode(covid.df$Location, c("Hawkes Bay" = "Hawke’s Bay")) 
                                  covid.df$Location <- fct_infreq(covid.df$Location, ordered = NA)
                                  covid.df$Age <- fct_recode(covid.df$Age, c("60s" = "64")) #fct_infreq(covid.df$Age, ordered = NA)     
                                  
                                  #write.csv(covid.df,"covid19.csv")
                                  #write.csv(covid.df,"covid19.csv")
                                  covid.df$Age <- fct_relevel(covid.df$Age, c("Not Reported","Child","Teens","20s","30s","40s","50s","60s","70s","80s")) 
                                  #fct_infreq(covid.df$Age, ordered = NA)                                  
                                  #write.csv(covid.df,"covid19.csv")
                                  covid.df
                              })
    ## Map Discalimer -------------------
    output$mapDisclaimer <- renderText({
        df <- covid.df()
        paste0("Hoverover: enabled | Number of cases with location unknown: ",sum(df$Location == "Not Reported")," | Data not available to TA level | Coordinates approximate")
    })
    
    # map_title <- tags$div(
    #     HTML('<img border="0" alt="ImageTitle" src="/www/ImageR.jpeg" width="300" height="100">')
    # )  
    ## Map RC -------------------
    output$mapRC <- renderLeaflet({
        covid_rc.df <- covid.df()
        covid_rc.df$Location <- fct_recode(covid_rc.df$Location, 
                                           "Auckland Region" = "Auckland",
                                           "Auckland Region" = "Waitemata",
                                           "Wellington Region" = "Wellington",
                                           "Marlborough Region" = "Marlborough",
                                           "Bay of Plenty Region" = "Bay of Plenty",
                                           "Otago Region" = "Dunedin",
                                           "Canterbury Region" = "Canterbury",
                                           "Otago Region" = "Wanaka",
                                           "Otago Region" = "Queenstown",
                                           "Waikato Region" = "Hamilton",
                                           "Canterbury Region" = "Christchurch",
                                           "Manawatu-Whanganui Region" = "New Plymouth",
                                           "Wellington Region" = "Wairarapa",
                                           "Wellington Region" = "Kapiti Coast", 
                                           "Hawke's Bay Region" = "Hawkes Bay",
                                           "Nelson Region" = "Nelson",
                                           "Waikato Region" = "Waikato",
                                           "Otago Region" = "Otago",
                                           "Canterbury Region" = "Waitaki",
                                           "Wellington Region" = "Upper Hutt",
                                           "Tasman Region" = "Tasman",
                                           "Manawatu-Whanganui Region" = "Manawatu",
                                           "Northland Region" = "Northland",
                                           "Taranaki Region" = "Taranaki",
                                           "Taupo Region" = "Taupo",
                                           "Auckland Region" = "Southern DHB",
                                           "Bay of Plenty Region" = "Rotorua",
                                           "Southland Region" = "Invercargill",
                                           "Marlborough Region" = "Blenheim") 
        
        covid_rc_tally.df <- as.data.frame(table(covid_rc.df$Location))
        colnames(covid_rc_tally.df)[1] <- "REGC2020_2"
        rc.sdf <- readOGR(dsn = "rc", layer = "regional-council-2020-clipped-generalised")
        virpal <- colorNumeric( palette = "viridis", domain = rc.sdf@data$Freq, na.color = "transparent")
        
        rc.sdf@data <- left_join(x = rc.sdf@data,
                                 y = covid_rc_tally.df,
                                 by = "REGC2020_2")
        
        rc.sdf@data$Freq[is.na(rc.sdf@data$Freq)] <- 0
        
        leaflet(data = rc.sdf,
                options = leafletOptions(minZoom = min_zoom, maxZoom = max_zoom,preferCanvas = T)) %>%
            addPolygons(fillColor = ~virpal(rc.sdf@data$Freq),
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
                        label = paste0(rc.sdf$REGC2020_2,":\n",rc.sdf$Freq),
                        labelOptions = labelOptions(
                            style = list("font-weight" = "normal", padding = "3px 8px"),
                            textsize = "15px",
                            direction = "auto")) %>% 
            addLegend(pal = virpal, values = ~Freq, opacity = 0.7, title = "COVID19 Cases",
                      position = "bottomright") %>%
            setView(lng = lng_init, 
                    lat = lat_init, 
                    zoom = zoom_level_init) 
        #addControl(map_title, position = "topleft")
    })
    
    covid_loc.df <- reactive({
        covid_marker.df <- covid.df()
        coords.df <- read_csv(file = "loc.csv")
        coords.df$lat <- as.numeric(coords.df$lat)
        covid_marker_tally.df <- as.data.frame(table(covid_marker.df$Location))
        colnames(covid_marker_tally.df)[1] <- "Location"
        coords.df <- left_join(coords.df,covid_marker_tally.df,by = "Location")
        coords.df
    })
    
    ## Map Marker -------------------
    output$markerRC <- renderLeaflet({
        
        coords.df <- covid_loc.df()
        
        mk.sdf <- readOGR(dsn = "rc", layer = "regional-council-2020-clipped-generalised")
        
        leaflet(data = mk.sdf,
                options = leafletOptions(minZoom = min_zoom, maxZoom = max_zoom,preferCanvas = T)) %>%
            addPolygons(fillColor = "white",
                        weight = 1,
                        opacity = 1,
                        color = "black",
                        dashArray = "3",
                        fillOpacity = 0.7) %>% 
            addCircleMarkers(data = coords.df,
                             lat = coords.df$lat,
                             lng = coords.df$long,
                             radius = 4 + (0.8 * coords.df$Freq),
                             color = "red",
                             stroke = FALSE, 
                             fillOpacity = 0.5,
                             label = paste0(coords.df$Location,":\n",coords.df$Freq)
            ) %>%
            setView(lng = lng_init, 
                    lat = lat_init, 
                    zoom = zoom_level_init) 
        #addControl(map_title, position = "topleft")
    })
    
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
    
    ## Time Series Plots -------------------
    output$time_series_cumulative_plot <- renderPlotly({
        
        ## Cumulative Time Series ---------------
        ts.df <- covid_ts.df()
        
        # recode dates
        ts.df$variable <- as.Date(ts.df[,2],
                                  format = "%m/%d/%y")
        
        ts.g <- ggplot(data = ts.df) +
            geom_line(mapping = aes(x = variable,y = value,group = 1)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            geom_point(mapping = aes(x = variable,y = value,group = 1)) +
            labs(title = "New Zealand COVID19 cases: Time Series (Cumulative)",subtitle = paste(Sys.time(),Sys.timezone()),x = "Date",y = "Cumulative Number of cases") +
            theme_bw() +
            #annotate(geom = "text", x = 1, y = max(ts.df$value)/2, label = paste0("N = ",nrow(ts.df)),color = "black") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = text_size)) +
            scale_x_date(breaks = seq(min(ts.df$variable), max(ts.df$variable), by = "2 day"), minor_breaks = "1 day") #+
        #geom_text(data = tail(ts.df),aes(x = variable - 0.5,y = value + max(new_cases)/20,label = value))
        #scale_x_date(breaks = ts.df$variable[seq(1, length(ts.df$variable), by = 3)])
        
        ts.g %>% 
            ggplotly() %>% #tooltip = c("Number of cases")
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('NZ COVID19 cases - Time Series',
                                              '<br>',
                                              '<sup>',
                                              date_stamp,
                                              '</sup>'))) 
    })
    
    ## New Cases Time Series -------------------
    output$time_series_new_plot <- renderPlotly({
        nc.df <- covid_ts.df()
        
        # recode dates
        nc.df$variable <- as.Date(nc.df[,2],
                                  format = "%m/%d/%y")
        
        nc.g <- ggplot(data = nc.df) +
            #geom_line(mapping = aes(x = variable,y = new_cases,group = 1)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            geom_col(mapping = aes(x = variable,y = new_cases,group = 1)) +
            labs(title = "NZ COVID19: New cases",subtitle = paste(Sys.time(),Sys.timezone()),x = "Date",y = "Number of new cases") +
            theme_bw() +
            #annotate(geom = "text", x = 1, y = max(ts.df$value)/2, label = paste0("N = ",nrow(ts.df)),color = "black") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1,size = text_size)) +
            scale_x_date(breaks = seq(min(nc.df$variable), max(nc.df$variable), by = "2 day"), minor_breaks = "1 day") #+
        #geom_text(data = tail(nc.df),aes(x = variable,y = new_cases + max(new_cases)/20,label = new_cases))
        #scale_x_date(breaks = ts.df$variable[seq(1, length(ts.df$variable), by = 3)])
        
        nc.g %>% 
            ggplotly() %>% #tooltip = c("Number of cases")
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('NZ COVID19 cases: New Cases',
                                              '<br>',
                                              '<sup>',
                                              date_stamp,
                                              '</sup>'))) 
    })
    ## Stacked Bar Charts ----------------
    output$region_age_plot <- renderPlotly({
        covid_main.df <- covid.df() %>%
            group_by(Age,Location) %>%
            summarise(n = length(Case))
        
        main.g <- ggplot(data = covid_main.df) +
            geom_col(mapping = aes(x = Location,y = n,fill = Age)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            labs(title = "NZ COVID19 cases - Region and Age",subtitle = paste(Sys.time(),Sys.timezone()),x = "Region",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() + theme(legend.position = "bottom") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
        
        main.g %>% 
            ggplotly(tooltip = c("Region","Age","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID19 cases - Region and Age',
                                              '<br>',
                                              '<sup>',
                                              date_stamp,
                                              '</sup>'))) 
    })
    output$age_gender_plot <- renderPlotly({
        covid_main.df <- covid.df() %>%
            group_by(Age,Gender) %>%
            summarise(n = length(Case))
        
        main.g <- ggplot(data = covid_main.df) +
            geom_col(mapping = aes(x = Age,y = n,fill = Gender)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            labs(title = "NZ COVID19 cases - Age and Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Age",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() + theme(legend.position = "bottom") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
        
        main.g %>% 
            ggplotly(tooltip = c("Gender","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('NZ COVID19 cases - Age and Gender',
                                              '<br>',
                                              '<sup>',
                                              date_stamp,
                                              '</sup>'))) 
    }) 
    output$region_gender_plot <- renderPlotly({
        covid_main.df <- covid.df() %>%
            group_by(Location,Gender) %>%
            summarise(n = length(Case))
        
        main.g <- ggplot(data = covid_main.df) +
            geom_col(mapping = aes(x = Location,y = n,fill = Gender)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            labs(title = "NZ COVID19 cases by Region and Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Region",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() + theme(legend.position = "bottom") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
        
        main.g %>% 
            ggplotly(tooltip = c("Gender","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('NZ COVID19 cases - Region and Gender',
                                              '<br>',
                                              '<sup>',
                                              date_stamp,
                                              '</sup>'))) 
    })
    ## Plot - Age -------------------
    output$age_plot <- renderPlotly({
        covid_age.df <- covid.df() %>%
            group_by(Age) %>%
            summarise(n = length(Case))
        
        age.g <- ggplot(data = covid_age.df) +
            geom_col(mapping = aes(x = Age,y = n,fill = Age)) + # reorder(covid_age.df$Age, -n)
            labs(title = "NZ COVID19 cases - Age",subtitle = paste(Sys.time(),Sys.timezone()),x = "Age",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() + theme(legend.position = "bottom") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
        
        age.g %>% ggplotly(tooltip = c("Age","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID19 cases by Age',
                                              '<br>',
                                              '<sup>',
                                              date_stamp,
                                              '</sup>'))) 
    })
    ## Plot - Region -------------------
    output$region_plot <- renderPlotly({
        covid_region.df <- covid.df() %>%
            group_by(Location) %>%
            summarise(n = length(Case)) 
        
        region.g <- ggplot(data = covid_region.df) +
            geom_col(mapping = aes(x = reorder(covid_region.df$Location, -n),y = n,fill = Location)) +
            labs(title = "NZ COVID19 cases - Region",subtitle = paste(Sys.time(),Sys.timezone()),x = "Location",y = "Number of cases") +
            theme_light() + theme(legend.position = "bottom") +
            scale_fill_viridis(discrete = T) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
        
        region.g %>% 
            ggplotly(tooltip = c("Location","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('NZ COVID19 cases - Region',
                                              '<br>',
                                              '<sup>',
                                              date_stamp,
                                              '</sup>'))) 
    })
    ## Plot - Gender -------------------
    output$gender_plot <- renderPlotly({
        covid_gender.df <- covid.df() %>%
            group_by(Gender) %>%
            summarise(n = length(Case)) 
        
        gender.g <- ggplot(data = covid_gender.df) +
            geom_col(mapping = aes(x = reorder(covid_gender.df$Gender, -n),y = n,fill = Gender)) +
            labs(title = "NZ COVID19 cases - Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Gender",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() + theme(legend.position = "bottom") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust  = 1,size = text_size))
        
        gender.g %>% ggplotly(tooltip = c("Gender","n")) %>% 
            config(displayModeBar = F) %>%
            layout(title = list(text = paste0('New Zealand COVID19 cases by Gender',
                                              '<br>',
                                              '<sup>',
                                              date_stamp,
                                              '</sup>'))) 
    })
    ## Tables -------------------
    output$raw_table = DT::renderDataTable({
        df <- covid.df()
        
        DT::datatable(df,options = list(
            pageLength = 60))
    })
    output$region_table = DT::renderDataTable({
        
        covid_region.df <- covid.df() %>%
            group_by(Location) %>%
            summarise(n = length(Case)) 
        
        DT::datatable(covid_region.df,options = list(
            pageLength = 60))
    })
    output$gender_table = DT::renderDataTable({
        
        covid_gender.df <- covid.df() %>%
            group_by(Gender) %>%
            summarise(n = length(Case)) 
        
        DT::datatable(covid_gender.df,options = list(
            pageLength = 60))
    })
    output$age_table = DT::renderDataTable({
        
        covid_age.df <- covid.df() %>%
            group_by(Age) %>%
            summarise(n = length(Case))
        
        DT::datatable(covid_age.df,options = list(
            pageLength = 60))
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
    # Downloadable csv of location dataset ----
    output$download_loc <- downloadHandler(
        filename = function() {
            paste("covid_19_location_nz_",as.numeric(Sys.time()),".csv", sep = "")
        },
        content = function(file) {
            write.csv(covid_loc.df(), file, row.names = FALSE)
        }
    )
    
    ## About -------------------
    output$about <- renderUI({
        HTML('<a href = "https://covid19.govt.nz/">covid19.govt.nz</a><br>
              This tool is not official. Check the Ministry of Health for all Official Statistics.<br> 
              Made by Matthew Skiffington <br> 
              Source Code: <a href = "https://github.com/MattSkiff/covid19_nz_data">Shiny App GitHub Repo</a><br> 
              Source MoH data: <a href = "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases">Ministry of Health Confirmed Cases (web tables)</a><br>')
        # Source Time Series data: <a href = "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv">John Hopkins University Centre for Systems Science and Engineering - Time Series Data Source</a><br>')       
    })
    
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
shinyApp(ui = ui, server = server)