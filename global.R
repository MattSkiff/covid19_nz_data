# Author: Matthew Skiffington
# Purpose: Libraries and Options Code for COVID19 Data Explorer - runs a shiny app

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
library(DT) # interactive data tables
library(shinydashboard) # dashboard structure
library(dashboardthemes) # snazzy themes
library(readxl) # read moh excel fileas_data function
library(lubridate) # date handling - in particular, 
#library(rgeos) # centroids

app_title <- "NZ COVID19 Data Explorer"
app_status <- "Last Updated: Thursday, 30/04/2020: Notice - an official Ministry of Health dashboard is now available"
date_stamp <- "Current to Thursday, 30/04/2020"
data_note_1 <- " | probable and confirmed infections"
omission_string <- " unreported age cases omitted"

`%nin%` = Negate(`%in%`)

#moh_map_url = "https://www.health.govt.nz/sites/default/files/images/our-work/diseases-conditions/covid19/hp7357_-_covid_confirmed_and_probable_cases_by_dhb-merged-270320_updated_0.jpg"

## Options --------------------
# set timezone
# set text size on x axis

m <- list(
	l = 50,
	r = 50,
	b = 50,
	t = 50,
	pad = 2
)

text_size = 10
plotly_text_size = 10
zoom_level_init = 7
lat_init = -38.45145547118427
lng_init = 175.717106114185
min_zoom = 5
max_zoom = 8

## Links

main_moh_url <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases"
cluster_moh_url <- "https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases/covid-19-clusters"

moh_open_link <- "window.open('https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases', '_blank')"
covid_open_link <- "window.open('https://covid19.govt.nz', '_blank')"
moh_dash_link <- "window.open('https://nzcoviddashboard.esr.cri.nz/', '_blank')"

#moh_open_link <- "window.open('https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-situation/covid-19-current-cases', '_blank')"

## Case Summary Information ---------------
# https://www.health.govt.nz/our-work/diseases-and-conditions/covid-19-novel-coronavirus/covid-19-current-cases

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