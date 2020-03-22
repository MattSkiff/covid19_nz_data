library(rvest) # scraping MoH
#library(rgdal)
#library(sp)
#library(leaflet)
library(shiny) # app
library(reshape2) # melting
library(dplyr) # wrangling
#library(rgeos)
library(magrittr) # piping
library(ggplot2) # core plot package
library(viridis) # nice colour scale
library(forcats) # factors
library(plotly) # interactive viz
library(shinythemes) # for web theme
library(DT) # for interactive data tables
library(readr) # to nicely read in data

# set timezone

# https://datascott.com/blog/subtitles-with-ggplotly/

# https://datafinder.stats.govt.nz/search/?q=territorial%2Bclipped
# WGS 84 (EPSG:4326 Geographic)
# nz.sdf <- readOGR(dsn = ".", layer = "territorial-authority-2020-clipped-generalised")
# taCentroids <- gCentroid(nz.sdf,byid=TRUE)
# 
# nz.sdf@data$TA2020_V_1[1:67]
# 
# leaflet(data = nz.sdf) %>% 
#     addTiles() %>% 
#     addPolygons() %>%
#     setView(lng = 172.8859711, lat = -39.9005585, zoom = 5) %>%
#     addCircleMarkers(data = taCentroids,
#                      radius = ~ sqrt(quantity))

# Define UI for application 
## UI -----------
ui <- fluidPage(theme = shinytheme("simplex"),
    tags$head(
        tags$meta(charset = "UTF-8"),
        tags$meta(name = "description", content = "COVID19 Data from NZ - Plots and Tables"),
        tags$meta(name = "keywords", content = "New Zealand, NZ, COVID19,Coronavirus,Data,Data Visualisation"),
        tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
        tags$meta(name = "og:image", content = "titlecard.jpg"),
        tags$meta(name = "og:url", content = "https://mks29.shinyapps.io/covid_nz/"),
        tags$meta(name = "og:type", content = "website"),
        tags$meta(name = "og:title", content = "COVID19 NZ Shiny App")
    ),
    
    # setup shinyjs
    #useShinyjs(),
    
    ## Application title -------------
    titlePanel(paste("New Zealand COVID19 Cases: ",as.Date(Sys.time() + 13*60*60),"(GMT+13)")), # adjust +13 hours for GMT+13 in NZ
    h3("Data Source: New Zealand Ministry of Health"),
    h4("Time Series Data Source: University of Hopkins Systems Science and Engineering Unit (pulls from World Health Organisation and other sources)"),
    h5("WHO data will have a 1-day lag against Ministry of Health data"),
    
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(2,
               wellPanel(
                   actionButton(inputId = "updateButton",
                                label = "Update")
               )
        ),
        column(5,
               wellPanel(
                   actionButton(inputId = "mohLink",
                                label = "Ministry of Health Cases Page",
                                onclick = "window.open('https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases', '_blank')")
                   #uiOutput("tab")
               )
        ),
        column(5,
               wellPanel(
                   downloadButton(outputId = "download",
                                label = "Download Raw Data")
                   #uiOutput("tab")
               )
        )
    ),
    fluidRow(
        column(12,h3(textOutput("info")))
    ),
    wellPanel(
        fluidRow(
            tabsetPanel(type = "tabs",
                        tabPanel("Time Series",
                                 #stacked barplots
                                 fluidRow(
                                     column(12,plotlyOutput("tsPlot",height = 600))
                                 )
                        ),
                        tabPanel("Bivariate",
                                 #stacked barplots
                                 fluidRow(
                                     column(12,plotlyOutput("mainPlot",height = 600))
                                 ),
                                 fluidRow(
                                     column(12,plotlyOutput("mainPlot2",height = 600))
                                 ),
                                 fluidRow(
                                     column(12,plotlyOutput("mainPlot3",height = 600))
                                 )
                        ),
                        tabPanel("Univariate",
                                 # simple barplots
                                 fluidRow(
                                     column(6,plotlyOutput("agePlot",height = 600)),
                                     column(6,plotlyOutput("genderPlot",height = 600))
                                 ),
                                 fluidRow(
                                     column(12,plotlyOutput("regionPlot",height = 600))
                                 )
                        ),
                        tabPanel("Additional Tables",
                                 fluidRow(
                                     column(12,DT::dataTableOutput("regionTable"))
                                 ),
                                 fluidRow(
                                     column(12,DT::dataTableOutput("ageTable"))
                                 ),
                                 fluidRow(
                                     column(12,DT::dataTableOutput("genderTable"))
                                 )
                        ),
                        tabPanel("Raw Data",
                                 DT::dataTableOutput("rawData")
                        ),
                        tabPanel("About",
                                 uiOutput("about")
                        )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    rv <- reactiveValues()
    rv$run <- 0
    ## Main Scraping and Dataframe ------------
    covid_ts.df <- eventReactive(eventExpr = c(input$updateButton,rv),
                                 valueExpr = {
                                     url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
                                     covid_ts.df <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
                                     covid_ts.df <- covid_ts.df %>% filter(`Country/Region` == "New Zealand") %>% select(-c(Lat,Long,`Province/State`)) 
                                     covid_ts.df <- covid_ts.df %>% rename(Country = `Country/Region`)
                                     coivd_ts.df <- melt(covid_ts.df)
                                     covid_ts.df <- coivd_ts.df %>% filter(value != 0)
                                     })
    
    covid.df <- eventReactive(eventExpr = c(input$updateButton,rv),
                              valueExpr = {
                                  # data gen
                                  url <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases"
                                  covid.ls <- url %>%
                                      html() %>%
                                      # html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
                                      html_table()
                                  
                                  covid.df <- covid.ls[[1]]
                                  
                                  covid.df$Location <- as.factor(covid.df$Location)
                                  covid.df$Gender <- as.factor(covid.df$Gender)
                                  covid.df$Age <- as.factor(covid.df$Age)
                                  
                                  covid.df <- covid.df %>%  mutate(Gender = recode(Gender, 
                                                                                   `Male` = "M",
                                                                                   `Female` = "F"))
                                  
                                  levels(covid.df$Gender)[levels(covid.df$Gender) == ""] <- "Not Reported"
                                  levels(covid.df$Location)[levels(covid.df$Location) == ""] <- "Not Reported"
                                  levels(covid.df$Age)[levels(covid.df$Age) == ""] <- "Not Reported"
                                  
                                  # sort levels by frequency of location
                                  covid.df$Location <- fct_infreq(covid.df$Location, ordered = NA)
                                  covid.df$Age <- fct_infreq(covid.df$Age, ordered = NA)
                                  
                                  #write.csv(covid.df,"covid19.csv")
                                  covid.df
                              })
    ## Stacked Bar Charts -------------------
    output$tsPlot <- renderPlotly({
        ts.df <- covid_ts.df()
        
        # recode dates
        ts.df$variable <- as.Date(ts.df$variable,
                                  format = "%m/%d/%y")
        
        main.g <- ggplot(data = ts.df) +
            geom_line(mapping = aes(x = variable,y = value,group = 1)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            geom_point(mapping = aes(x = variable,y = value,group = 1)) +
            labs(title = "New Zealand COVID cases: Time Series",subtitle = paste(Sys.time(),Sys.timezone()),x = "Date",y = "Cumulative Number of cases") +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) #+
            #scale_x_date(breaks = ts.df$variable[seq(1, length(ts.df$variable), by = 3)])
        
        main.g %>% 
            ggplotly() %>% #tooltip = c("Number of cases")
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID cases - Time Series',
                                              '<br>',
                                              '<sup>',
                                              Sys.time() + 13*60*60,
                                              '</sup>')))
    })
    
    output$mainPlot <- renderPlotly({
        covid_main.df <- covid.df() %>%
            group_by(Age,Location) %>%
            summarise(n = length(Case))
        
        main.g <- ggplot(data = covid_main.df) +
            geom_col(mapping = aes(x = Location,y = n,fill = Age)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            labs(title = "New Zealand COVID cases by Region and Age",subtitle = paste(Sys.time(),Sys.timezone()),x = "Region",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        main.g %>% 
            ggplotly(tooltip = c("Region","Age","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID cases by Region and Age',
                                              '<br>',
                                              '<sup>',
                                              Sys.time() + 13*60*60,
                                              '</sup>')))
    })
    output$mainPlot2 <- renderPlotly({
        covid_main.df <- covid.df() %>%
            group_by(Age,Gender) %>%
            summarise(n = length(Case))
        
        main.g <- ggplot(data = covid_main.df) +
            geom_col(mapping = aes(x = Age,y = n,fill = Gender)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            labs(title = "New Zealand COVID cases by Age and Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Age",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        main.g %>% 
            ggplotly(tooltip = c("Gender","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID cases by Age and Gender',
                                              '<br>',
                                              '<sup>',
                                              Sys.time() + 13*60*60,
                                              '</sup>')))
    })
    output$mainPlot3 <- renderPlotly({
        covid_main.df <- covid.df() %>%
            group_by(Location,Gender) %>%
            summarise(n = length(Case))
        
        main.g <- ggplot(data = covid_main.df) +
            geom_col(mapping = aes(x = Location,y = n,fill = Gender)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            labs(title = "New Zealand COVID cases by Region and Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Region",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        main.g %>% 
            ggplotly(tooltip = c("Gender","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID cases by Region and Gender',
                                              '<br>',
                                              '<sup>',
                                              Sys.time() + 13*60*60,
                                              '</sup>')))
    })
    ## Plot - Age -------------------
    output$agePlot <- renderPlotly({
        covid_age.df <- covid.df() %>%
            group_by(Age) %>%
            summarise(n = length(Case))
        
        age.g <- ggplot(data = covid_age.df) +
            geom_col(mapping = aes(x = reorder(covid_age.df$Age, -n),y = n,fill = Age)) +
            labs(title = "New Zealand COVID19 cases by Age",subtitle = paste(Sys.time(),Sys.timezone()),x = "Age",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        age.g %>% ggplotly(tooltip = c("Age","n")) %>% 
                  config(displayModeBar = F) %>% 
                  layout(title = list(text = paste0('New Zealand COVID19 cases by Age',
                                                    '<br>',
                                                    '<sup>',
                                                    Sys.time() + 13*60*60,
                                                    '</sup>')))
    })
    ## Plot - Region -------------------
    output$regionPlot <- renderPlotly({
        covid_region.df <- covid.df() %>%
            group_by(Location) %>%
            summarise(n = length(Case)) 
        
        region.g <- ggplot(data = covid_region.df) +
            geom_col(mapping = aes(x = reorder(covid_region.df$Location, -n),y = n,fill = Location,)) +
            labs(title = "New Zealand COVID19 cases by Region",subtitle = paste(Sys.time(),Sys.timezone()),x = "Location",y = "Number of cases") +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        region.g %>% 
            ggplotly(tooltip = c("Location","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID19 cases by Region',
                                              '<br>',
                                              '<sup>',
                                              Sys.time() + 13*60*60,
                                              '</sup>')))
    })
    ## Plot - Gender -------------------
    output$genderPlot <- renderPlotly({
        covid_gender.df <- covid.df() %>%
            group_by(Gender) %>%
            summarise(n = length(Case)) 
        
        gender.g <- ggplot(data = covid_gender.df) +
            geom_col(mapping = aes(x = reorder(covid_gender.df$Gender, -n),y = n,fill = Gender)) +
            labs(title = "New Zealand COVID19 cases by Gender",subtitle = paste(Sys.time(),Sys.timezone()),x = "Gender",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        gender.g %>% ggplotly(tooltip = c("Gender","n")) %>% 
                     config(displayModeBar = F) %>%
                     layout(title = list(text = paste0('New Zealand COVID19 cases by Gender',
                                                      '<br>',
                                                      '<sup>',
                                                      Sys.time() + 13*60*60,
                                                      '</sup>')))
    })
    ## Info -------------------
    output$info <- renderText({
        paste("The total number of confirmed cases in New Zealand is:",nrow(covid.df()))
    })
    ## Tables -------------------
    output$rawData = DT::renderDataTable({
        DT::datatable(covid.df(),options = list(
            pageLength = 60))
    })
    output$regionTable = DT::renderDataTable({
        
        covid_region.df <- covid.df() %>%
            group_by(Location) %>%
            summarise(n = length(Case)) 
        
        DT::datatable(covid_region.df,options = list(
            pageLength = 60))
    })
    output$genderTable = DT::renderDataTable({
        
        covid_gender.df <- covid.df() %>%
            group_by(Gender) %>%
            summarise(n = length(Case)) 
        
        DT::datatable(covid_gender.df,options = list(
            pageLength = 60))
    })
    output$ageTable = DT::renderDataTable({
        
        covid_age.df <- covid.df() %>%
            group_by(Age) %>%
            summarise(n = length(Case))
        
        DT::datatable(covid_age.df,options = list(
            pageLength = 60))
    })
    
    # output$tab <- renderUI({
    #     url <- a("Ministry of Health Cases Page", href = "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases")
    #     tags$div(class = "submit",
    #              tags$a(href = url, 
    #                     "Learn More", 
    #                     target = "_blank")
    #     )
    #     tagList("URL link:", url)
    # })
    
    # Downloadable csv of selected dataset ----
    output$download <- downloadHandler(
        filename = function() {
            paste("covid_19_nz_",as.numeric(Sys.time()),".csv", sep = "")
        },
        content = function(file) {
            write.csv(covid.df(), file, row.names = FALSE)
        }
    )
    
    output$about <- renderUI({
        HTML('Developed by Matthew Skiffington.  <br> 
              Source Code: <a href = "https://github.com/MattSkiff/covid19_nz_data">GitHub Repo</a><br> 
              Source MoH data: <a href = "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases">Ministry of Health Confirmed Cases</a><br>
              Source Time Series data: <a href = "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv">Time Series Data Source</a><br>')
    })
    
    rv$run <- 1
    
}

# Run the application 
shinyApp(ui = ui, server = server)
