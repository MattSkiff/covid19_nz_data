library(rvest)
#library(rgdal)
#library(sp)
#library(leaflet)
library(shiny)
library(magrittr)
library(reshape2)
library(dplyr)
#library(rgeos)
library(magrittr)
library(ggplot2)
library(viridis)
library(forcats)
library(plotly)
library(shinythemes)
library(DT)

# set timezone
Sys.setenv(TZ ='Auckland/Pacific')

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
ui <- fluidPage(theme = shinytheme("simplex"),
    
    # setup shinyjs
    #useShinyjs(),
    
    # Application title
    titlePanel(paste("New Zealand COVID19 Cases: ",Sys.Date())),
    h3("Data Source: New Zealand Ministry of Health"),
    
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
                        tabPanel("Summary",
                                 fluidRow(
                                     column(12,plotlyOutput("mainPlot",height = 600))
                                 ),
                                 fluidRow(
                                     column(12,plotlyOutput("mainPlot2",height = 600))
                                 )
                        ),
                        tabPanel("Other",
                                 fluidRow(
                                     column(6,plotlyOutput("agePlot",height = 600)),
                                     column(6,plotlyOutput("genderPlot",height = 600))
                                 ),
                                 fluidRow(
                                     column(12,plotlyOutput("regionPlot",height = 600))
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
    output$mainPlot <- renderPlotly({
        covid_main.df <- covid.df() %>%
            group_by(Age,Location) %>%
            summarise(n = length(Case))
        
        main.g <- ggplot(data = covid_main.df) +
            geom_col(mapping = aes(x = Location,y = n,fill = Age)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            labs(title = "New Zealand COVID cases by Age and Region",subtitle = Sys.time(),x = "Region",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        main.g %>% 
            ggplotly(tooltip = c("Region","Age","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID cases by Age and Region',
                                              '<br>',
                                              '<sup>',
                                              Sys.time(),
                                              '</sup>')))
    })
    output$mainPlot2 <- renderPlotly({
        covid_main.df <- covid.df() %>%
            group_by(Age,Gender) %>%
            summarise(n = length(Case))
        
        main.g <- ggplot(data = covid_main.df) +
            geom_col(mapping = aes(x = Age,y = n,fill = Gender)) + # reorder(covid_main.df$Location,left_join(covid_main.df,order.df)$order)
            labs(title = "New Zealand COVID cases by Age and Gender",subtitle = Sys.time(),x = "Age",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        main.g %>% 
            ggplotly(tooltip = c("Gender","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID cases by Age and Gender',
                                              '<br>',
                                              '<sup>',
                                              Sys.time(),
                                              '</sup>')))
    })
    output$agePlot <- renderPlotly({
        covid_age.df <- covid.df() %>%
            group_by(Age) %>%
            summarise(n = length(Case))
        
        age.g <- ggplot(data = covid_age.df) +
            geom_col(mapping = aes(x = reorder(covid_age.df$Age, -n),y = n,fill = Age)) +
            labs(title = "New Zealand COVID19 cases by Age",subtitle = Sys.time(),x = "Age",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        age.g %>% ggplotly(tooltip = c("Age","n")) %>% 
                  config(displayModeBar = F) %>% 
                  layout(title = list(text = paste0('New Zealand COVID19 cases by Age',
                                                    '<br>',
                                                    '<sup>',
                                                    Sys.time(),
                                                    '</sup>')))
    })
    output$regionPlot <- renderPlotly({
        covid_region.df <- covid.df() %>%
            group_by(Location) %>%
            summarise(n = length(Case)) 
        
        region.g <- ggplot(data = covid_region.df) +
            geom_col(mapping = aes(x = reorder(covid_region.df$Location, -n),y = n,fill = Location,)) +
            labs(title = "New Zealand COVID19 cases by Region",subtitle = Sys.time(),x = "Location",y = "Number of cases") +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        region.g %>% 
            ggplotly(tooltip = c("Location","n")) %>% 
            config(displayModeBar = F) %>% 
            layout(title = list(text = paste0('New Zealand COVID19 cases by Region',
                                              '<br>',
                                              '<sup>',
                                              Sys.time(),
                                              '</sup>')))
    })
    output$genderPlot <- renderPlotly({
        covid_gender.df <- covid.df() %>%
            group_by(Gender) %>%
            summarise(n = length(Case)) 
        
        gender.g <- ggplot(data = covid_gender.df) +
            geom_col(mapping = aes(x = reorder(covid_gender.df$Gender, -n),y = n,fill = Gender)) +
            labs(title = "New Zealand COVID19 cases by Gender",subtitle = Sys.time(),x = "Gender",y = "Number of cases") +
            scale_fill_viridis(discrete = T) +
            theme_light() +
            theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
        
        gender.g %>% ggplotly(tooltip = c("Gender","n")) %>% 
                     config(displayModeBar = F) %>%
                     layout(title = list(text = paste0('New Zealand COVID19 cases by Gender',
                                                      '<br>',
                                                      '<sup>',
                                                      Sys.time(),
                                                      '</sup>')))
    })
    
    output$info <- renderText({
        paste("The total number of confirmed cases in New Zealand is:",nrow(covid.df()))
    })
    output$rawData = DT::renderDataTable({
        DT::datatable(covid.df(),options = list(
            pageLength = 60
        ))
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
            paste("covid_19_",as.numeric(Sys.time()),".csv", sep = "")
        },
        content = function(file) {
            write.csv(covid.df(), file, row.names = FALSE)
        }
    )
    
    output$about <- renderUI({
        HTML('Developed by Matthew Skiffington.  <br> 
              Source Code: <a href = "https://github.com/MattSkiff/covid19_nz_data">GitHub Repo</a>')
    })
    
    rv$run <- 1
    
}

# Run the application 
shinyApp(ui = ui, server = server)
