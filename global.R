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
#library(readr) # to nicely read in data
library(shinydashboard) # dashboard structure
library(dashboardthemes) # snazzy themes
#library(rgeos) # centroids

app_title <- "NZ COVID19 Data Explorer"
app_status <- "App up to date as of 27/03/2020"

## Options --------------------
# set timezone
# set text size on x axis
text_size = 8
zoom_level_init = 7
lat_init = -38.45145547118427
lng_init = 175.717106114185
min_zoom = 5
max_zoom = 8
moh_open_link <- "window.open('https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases', '_blank')"
covid_open_link <- "window.open('https://covid19.govt.nz', '_blank')"

## Case Summary Information ---------------
# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases
total_cases <- 368
total_cases_new_24 <- 85 

confirmed_cases <- 338
confirmed_cases_new_24 <- 76

probable_cases <- 30
probable_cases_new_24 <- 9

recovered_cases <- 37
recovered_cases_new_24 <- 10

in_hospital <- "8"

alert_level <- 4
###

# Projection: WGS 84 (EPSG:4326 Geographic)

# Useful Links
# https://datascott.com/blog/subtitles-with-ggplotly/
# https://datafinder.stats.govt.nz/search/?q=territorial%2Bclipped
# https://rstudio.github.io/leaflet/markers.html

date_stamp <- "Current to 27/03/2020"