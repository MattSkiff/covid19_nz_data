# COVID19 NZ Data Explorer

UPDATE: The Ministry of Health / ESR now provides a dashboard for COVID19 data:
https://nzcoviddashboard.esr.cri.nz/

A shiny app to view covid19 data scraped from the MoH website. This app is not official in any way shape or form and is a personal project. All data is now sourced from the Ministry of Health.

Update: The Ministry of Health now has some charts and a map available on their website.

DHB boundaries are from 2015 and sourced from the Statistics NZ datafinder tool. The .shp file has had the geometry optimised to improve leaflet load time.

This application will be archived once the Ministry Health provides a similar level of analytics, or once a clearly superior tool is available, or once updating the app daily becomes infeasible (the Ministry of Health data source website has been restructured 5 times, each time requiring recoding the app accordingly). 

The apps depends on quite a few packages, in particular the excellent shiny dashboard themes package available here:
https://github.com/nik01010/dashboardthemes

If there is something you would like me to add, please let me know.

## To Do

* Optimisation to load faster and use less memory 
* Optimisation and layout for mobile devices
* Graph styling could be improved
* Host on a (better) URL to redirect to Shiny app hosting
* Statistical analysis 
* Time series analysis showing the spread of cases would be interesting
* Style core stats table into info boxes automatically
* Add tranmission table and graph
* Add lab testing metrics

## Features

* A 3D world map showing the locations of cases prior to getting coronavirus
* Univariate breakdowns - age, gender, dhb, ethnicity
* Bivariate breakdowns
* Searchable tables of information (cluster,age,raw data, dhb, etc)
* NZ Choropleth with markers showing cases by DHB
* Basic time series plots of new and cumulative cases
* Cumulative time series by region

## Contact

skiffcoffee@gmail.com

