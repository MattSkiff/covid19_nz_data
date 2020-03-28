# COVID19 NZ Data Explorer

A shiny app to view covid19 data scraped from the MoH website. This app is not official in any way shape or form and is simply a personal project. All data is now sourced from the Ministry of Health.

Update: The Ministry of Health now has some charts and a map available on their website.

DHB boundaries are from 2015 and sourced from the Statistics NZ datafinder tool. The .shp file has had the geometry optimised to improve leaflet load time.

This application will be archived once the Ministry Health provides a similar level of analytics, or once a clearly superior tool is available, or once updating the app daily becomes infeasible (the Ministry of Health data source website has been restructured 5 times, each time requiring recoding the app accordingly). 

The apps depends on quite a few packages, in particular the excellent shiny dashboard themes package available here:
https://github.com/nik01010/dashboardthemes

If there is something you would like me to add, please let me know. 
https://mks29.shinyapps.io/covid_nz/

## To Do

* A map showing the travel patterns of cases prior to getting coronavirus
* Markers added to the map showing clusters would be a good idea
* Optimisation to load faster and use less memory 
* Graph styling could be improved
* Info boxes are updated manually -> automatically update from the Ministry of Health website
* Statistical analysis
* Host on a URL to redirect to Shiny app hosting
* Statistical analysis 
* Time series analysis showing the spread of cases would be interesting

## Contact

https://www.linkedin.com/in/matthew-skiffington/

