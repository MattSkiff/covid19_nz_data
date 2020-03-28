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
library(shinydashboard) # dashboard structure
library(dashboardthemes) # snazzy themes
#library(rgeos) # centroids

app_title <- "NZ COVID19 Data Explorer"
app_status <- "Updated: 28/03/2020 : Status - Up to Date"
date_stamp <- "Current to 28/03/2020"

#moh_map_url = "https://www.health.govt.nz/sites/default/files/images/our-work/diseases-conditions/covid19/hp7357_-_covid_confirmed_and_probable_cases_by_dhb-merged-270320_updated_0.jpg"

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
#moh_open_link <- "window.open('https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases', '_blank')"

## Case Summary Information ---------------
# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases
total_cases <- 451
total_cases_new_24 <- 78 

confirmed_cases <- 416
confirmed_cases_new_24 <- 78

probable_cases <- 35
probable_cases_new_24 <- 5

recovered_cases <- 50
recovered_cases_new_24 <- 13

in_hospital <- "12"

alert_level <- 4
###

# Projection: WGS 84 (EPSG:4326 Geographic)

# Useful Links
# https://datascott.com/blog/subtitles-with-ggplotly/
# https://datafinder.stats.govt.nz/search/?q=territorial%2Bclipped
# https://rstudio.github.io/leaflet/markers.html

# SEO Sections
share <- list(
  title = "COVID19 NZ Data App",
  url = "https://mks29.shinyapps.io/covid_nz/",
  image = "titlecard.jpg",
  description = "COVID19 NZ Data Explorer: Explore pandemic data, updated daily.",
  twitter_user = "mattskiff_"
)