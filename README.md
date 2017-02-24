# Project 2: Open Data App - an RShiny app development project

### [Project Description](doc/project2_desc.md)


In this second project of GR5243 Applied Data Science, we develop an *Exploratory Data Analysis and Visualization* shiny app on US international trade using U.S. government open data released on the [data.gov](https://data.gov/) website. See [Project 2 Description](project2_desc.md) for more details.  

Data Source: [data.gov](http://www.census.gov/foreign-trade/statistics/country/sitc/index.html).

## Project Title Trace of Aroma - World Trade with U.S.
Term: Spring 2017

+ Team # Scent of Spirit
+ **Trace of Aroma - World Trade with U.S.**: + Team members
	+ Ruxue Peng
	+ Raphael Xiv
	+ Xiaowo Sun
	+ Bowen Huang
	+ Terry Li

+ **Project summary**: In this cool project, we aim to help our user explore the trend of international trade of coffee, tea, chocolate, cocoa and spices(all belong to the same subgroup in Customs)and look into the reason of up-and-downs of trade value. We do that by presenting our data and results interactively: trade relationships are displayed on 3D globe, 2D map with different countries' news link and a series of statistical plots. 

+ **Contribution statement**: ([default](doc/a_note_on_contributions.md)) All team members contributed equally in all stages of this project. All team members approve our work presented in this GitHub repository including this contributions statement. 

+ **Project demostration**: 
We have mainly four parts show on the web, "Home", "3D Globe", "2D Map", and "Summary Statistics", which are shown as follows. 
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311757/3246d4a4-fa85-11e6-99f6-dbfb9519b6d5.png)

"Home" is our homepage, with the names of out project and team members. "3D Globe" is an interactive 3D map for earch, which shows the geographical distribution of the countries whom the U.S. imports from or exports to. "3D Globe" is shown below.
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311771/46c380bc-fa85-11e6-879a-5236c3ecbc0c.png)

"2D Map" shows the position of counties which U.S. exports to or imports from. Comparing to "3D Globe", you can view the name of countries and have a better comprehension of the rank of the countries.
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311785/51c62f28-fa85-11e6-9931-b595a4ac56a3.png)

As for "Summary Statistics", there are four parts in it, which are designed to analyze trades more in depth from several perspectives. In the tab "Regional Findings", you can see the visualization of trade along the timeline divided by different regions. If you move to the panel for "Market Share", you can see pretty clear maps representing the share and rank for different countries.
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311796/5b65f3ba-fa85-11e6-82ca-e30b3538ae22.png)
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311812/67406436-fa85-11e6-81e4-f4e3ffe3278d.png)

For the tab "Exchange rate", you can find on "Mirror Histogram" one country's import and export with U.S. on the same chart according to time. And you can also find the exchange rate shown on the chart. In order to find the relation ship between, the regression results are shown for different if you choose the tab "Regression" on the right.
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23313482/e72b7b4e-fa8b-11e6-9580-b7d665e56953.png)
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311853/8c02345c-fa85-11e6-86bd-3a06cb1e2ab2.png)

For the "Clustering Analysis" tab, you can find the clustering results for all countries concerning five commodities' import. This part is mainly for find the similaritis for tading commodities among various countries. The result are shown on the map as well as in the table. Different colors means different clusters, and the number of countries in each cluster and the average magnitude for different commodies are shown in each line of the tavble.
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311861/941aeb7a-fa85-11e6-861e-e0022c413754.png)
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311874/9dc5d950-fa85-11e6-9bf8-49d15465086a.png)

The last part is "Motion Chart", in which you can see the dynamic change of two commodities for import concerning all countries. And if you choose the tab for line chart, the variation of one country/ several selected countries are also visualized.
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311904/b9574e38-fa85-11e6-8dad-18ca1ff49cda.png)
![screenshot](https://cloud.githubusercontent.com/assets/25423915/23311918/c13cf706-fa85-11e6-991d-842728323d21.png)

The above demonstation are mainly what we built for this app. 


Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

To reproduce the result, please refer to the app folder and its readme file.

Resources:  
[Visualization ideas-d3](https://d3js.org/)  
[How to build a 3D globe](https://rpubs.com/aagarwal29/r3dglobe)  
[3D globe-r blogger](https://www.r-bloggers.com/animate-maps-with-mapmate-r-package-for-map-and-globe-based-still-image-sequences/)  
[Animation of 3D globe](http://leonawicz.github.io/gc_animation_example/app_traffic_example.html)  
[2D choropleth map](https://cran.r-project.org/web/packages/rworldmap/rworldmap.pdf)  
[Map using Leaflet](https://rstudio.github.io/leaflet/basemaps.html)  
[Map using Leaflet-Example Explained in Chinese](http://blog.csdn.net/allenlu2008/article/details/52865163)  
[Tree Map and Motion Chart-googlevis package in R](https://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html)  
[How to customize your shinyapp style](https://www.w3schools.com/css/css_background.asp)  
[CSS](http://shiny.rstudio.com/articles/css.html)  



