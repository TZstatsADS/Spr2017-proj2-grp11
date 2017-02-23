## Packages

packages.used <- 
  c("geosphere", # For spatial methods  
    "threejs",   # threejs is used for 3-D interactive Earth Visualization
    "rworldmap", # For creating earth map
    "leaflet",   # Leaflet for R provides functions to control and integrate Leaflet, a JavaScript library for interactive maps, within R.
    "rgeos",      # Provides functions for handling operations on topologies.
    "raster",     # For raster image
    "DT",         # For creating interactive tables
    "ggplot2",
    "sp"   ,       # For Spatial processing of data
    "ggmap",       # To reverse geocode Long/Lat
    "knitr",        # TO enable 3-D visualization embedding in the HTML page
    "rglwidget",
    "rgl",
    "plyr",
    "reshape2",
    "maptools",
    "shiny",
    "googleVis",
    "dplyr",
    "plotly",
    "RColorBrewer",
    "treemap",
    "gplots"
  )

# check packages that need to be installed.
packages.needed=setdiff(packages.used, 
                        intersect(installed.packages()[,1], 
                                  packages.used))
# install additional packages
if(length(packages.needed)>0){
  install.packages(packages.needed, dependencies = TRUE)
}

#load the packages
library("gplots")
library("plyr")
library("dplyr")
library("reshape2")
library("geosphere")
library("threejs")
library("rworldmap")
library("leaflet")
library("rgeos")
library("raster")
library("DT")
library("ggplot2")
library("sp")
library("ggmap")
library("knitr")
library("rglwidget")
library("rgl")
library("maptools")
library("shiny")
library("googleVis")
library("plotly")
library("grid")
library("gtable")
library("treemap")
library("RColorBrewer")

## preprocess work, Load dataframe already prepared for plotting
input_data =  read.csv("mydata_wRegions.csv",header = T,as.is = T)
input_data = input_data[!is.na(input_data$longitude),]
input_data = input_data[input_data$value != 0,]
input_data[!is.na(input_data$Commodity_Name) & input_data$Commodity_Name == "COCOA",10] = "Cocoa"
#Load the data for Google motion data
country<-read.csv("country_cleaned.csv")
# force all values in country dataset to be numeric
for (i in 3:7){
  country[,i]<-as.numeric(country[,i])
}
## end preprocess data



## mergring exchange rate data
exchange_rate =  read.csv("exchange_rate.csv")
CPI =  read.csv("CPI.csv")
import <- filter(input_data, input_data$type=="Import") 
Export <- filter(input_data, input_data$type=="Export") 
import$id <- paste0(import$Country,"/",import$Year)
import<-merge(x = import, y = exchange_rate, by = "id", all.x = TRUE)

import.without.aggregate <- filter(import, import$Commodity_Name != "Annual Aggregate")
import.without.aggregate$source <- as.character(import.without.aggregate$Country)
import.without.aggregate$target <- as.character(import.without.aggregate$Commodity_Name)
##

## map creation preprocess
data(wrld_simpl) # Basic country shapes
bgcolor = "#000000"
arc_colors = c("red","blue","green","#ffe9bf","pink","orange")
map_pal = data.frame(AnnualAggregate = c("red"),Chocolate = c("blue"),Coffee = c("green"),COCOA = c("#ffe9bf"),Spices = c("pink"),Tea = c("orange"))
names(map_pal)[1] = "Annual Aggregate"
## end preprocess map

## Load clustering data
cluster_data_import = read.csv("clustering-ready-Import.csv")
code = read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')[,c(1,3)]

## end loading cluster data


## UI Function

ui<- navbarPage(
  
  ##link to css.file
  theme = "bootstrap2.css",
  
  ##Project Title
  "Flowing in and Flowing out",
  
  ## 3D Globe tab
  tabPanel("3D Globe",
           titlePanel("Coffee ,tea, and others traded between US and the world"),
           absolutePanel(id = "controls", class = "panel panel-default",
                         draggable = TRUE, 
                         top = 180, left = "auto", right = 60, bottom = "auto",
                         width = 350, height = "auto",
                         
                         h2("3D Explorer"),
                         
                         radioButtons(inputId = "type",
                                      label  = "Choose import/export",
                                      choices = c('Export','Import'),
                                      selected ='Import'),
                         sliderInput(inputId = "year_3D",
                                     label = "Select a year",
                                     value = 2016, min =1996, max =2016),
                         sliderInput(inputId = "number_countries",
                                     label = "Top Countries in Trade",
                                     value = 10,min = 1,max = 50),
                         selectInput(inputId = "commodity_3D",
                                     label  = "Select the commodity",
                                     choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                                     selected ='Coffee')
                         ),
           
           absolutePanel(id = "controls", class = "panel panel-default",
                         draggable = TRUE, 
                         top = "auto", left = 60, right = "auto", bottom = 20,
                         width = 500 , height = 200,
                         
                         plotOutput("ggplot",width="100%",height="200px")),
           
           
           globeOutput("Globe",width="100%",height="650px")),
  ## end 3D Globe tab
  
  ## 2D Map tab
  tabPanel("2D Map",
           titlePanel("Coffee ,tea, and others traded between US and the world"),
           
           leafletOutput("mymap",width = "100%", height = 600),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, 
                         top = 180, left = 60, right = "auto", bottom = "auto",
                         width = 350, height = "auto",
                         
                         h2("2D Explorer"),
                         
                         radioButtons(inputId = "type_2D",
                                      label  = "Choose import/export",
                                      choices = c('Export','Import'),
                                      selected ='Import'),
                         sliderInput(inputId = "year_2D",
                                     label = "Select a year",
                                     value = 2016, min =1996, max =2016),
                         sliderInput(inputId = "num_countries",
                                     label = "Top Countries in Trade",
                                     value = 20,min = 1,max = 50),
                         selectInput(inputId = "commodity_2D",
                                     label  = "Select the commodity",
                                     choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                                     selected ='Coffee')
                         
           )
  ),
  
  ## end 2D Map tab
  
  ## Summary Statistics tab
  navbarMenu("Summary Statistics",
             
             ### Motion Chart
             tabPanel("Motion Chart",
                      mainPanel(
                        htmlOutput("view")
                      )
             ),
             ### end Motion Chart
             
             ##Regional Findings tabset
             tabPanel("Regional Findings",
                      tabsetPanel(
                        
                        ##Continent & Region
                        tabPanel("Regional statistics",
                                 titlePanel("Continent & Region"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "commodity",
                                                 label  = "choose the commodity",
                                                 choices = na.omit(unique(input_data$Commodity_Name)),
                                                 selected ='Coffee'),
                                     sliderInput(inputId = "year",
                                                 label = "Select a year",
                                                 value = 2016, min =1996, max =2016),
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("continent_import", height = "330px"),
                                     plotlyOutput("regional_import",height = "420px")
                                   )
                                   
                                 )
                        ),
                        
                        ### Tree Map
                        tabPanel("Market Share",
                                 titlePanel("Market Share of Countries"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "com_tree",
                                                 label  = "Select the commodity",
                                                 choices = c('Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                                                 selected ='Coffee'),
                                     sliderInput(
                                       inputId = "year_tree",
                                       label = "Select a year",
                                       value = 2016, min =1996, max =2016),
                                     
                                     width = 3
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("treemap",width = "100%", height = 600)
                                   )
                                 )
                        )
                        ### end Tree Map
                        
                      )
             ),
             #end Regional
             
             ## Clustering tab
             
             tabPanel("Clustering Analysis",
                      titlePanel("Clustering Analysis"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          sliderInput(inputId = "number_clusters",
                                      label = "Number of Clusters",
                                      value = 5,min = 2,max = 10),
                          sliderInput(
                            inputId = "year_cluster",
                            label = "Select a year",
                            value = 1996, min =1996, max =2016),
                          width = 3
                        ),
                        
                        mainPanel(
                          plotlyOutput("cluster", width = "100%", height = "400px"),
                          textOutput("text_1"),
                          textOutput("text_2"),
                          dataTableOutput("mytable")
                        )
                      )
                      
             ),
             ## end Clustering tab
             
             
             ### Exchange Rate
             tabPanel("Exchange Rate", 
                      tabsetPanel(
                        
                        ### end Exchange Rate
                        
                        ### Mirror Histogram
                        tabPanel("Mirror Histogram",
                                 titlePanel("Trade Trend vs Exchange Rate"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "commodity_hist",
                                                 label  = "Select the commodity",
                                                 choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                                                 selected ='Tea'),
                                     selectInput(inputId = "country_hist1",
                                                 label  = "Select the country1",
                                                 choices = sort(unique(input_data$Country)),
                                                 selected ='Canada'),
                                     selectInput(inputId = "country_hist2",
                                                 label  = "Select the country2",
                                                 choices = sort(unique(input_data$Country)),
                                                 selected ='China'),
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("Hist1"),
                                     plotOutput("Hist2")
                                   )
                                 )
                                 
                                 
                        ),
                        
                        tabPanel("Regression",
                                 titlePanel("Regression of Import and Exchange Rate"),
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput(inputId = "exchange_commodity",
                                                 label  = "choose the commodity",
                                                 choices = unique(import$Commodity_Name),
                                                 selected ='Spices'),
                                     selectInput(inputId = "exchange_country",
                                                 label  = "choose the country",
                                                 choices = unique(import$Country),
                                                 selected ='China'),
                                     width = 3
                                     
                                   ),
                                   
                                   mainPanel(
                                     plotOutput("linear_exchange")
                                   )
                                 )
                                 
                        )
                        ### end Mirror Histogram
                        
                        
                      )
             )
             ## end Summary Statistics tab
             
  )
)