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
    "treemap"
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
library("plyr")
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
library("treemap")


## preprocess work, Load dataframe already prepared for plotting
input_data =  read.csv("data/mydata.csv",header = T,as.is = T)
exchange_rate =  read.csv("data/exchange_rate.csv")
input_data = input_data[!is.na(input_data$longitude),]
input_data = input_data[input_data$value != 0,]
#create 6 level for value data whose magnitude ranges from 1e3 tp 1e8
input_data$log = ceiling(log(input_data$value)/3)-2
## end preprocess data


## mergring exchange rate data
import <- filter(input_data, input_data$type=="Import") 
Export <- filter(input_data, input_data$type=="Export") 
import$id <- paste0(import$Country,"/",import$Year)
import<-merge(x = import, y = exchange_rate, by = "id", all.x = TRUE)

import.without.aggregate <- filter(import, import$Commodity_Name != "Annual Aggregate")
import.without.aggregate$source <- as.character(import.without.aggregate$Country)
import.without.aggregate$target <- as.character(import.without.aggregate$Commodity_Name)


##

## UI Function

ui<- navbarPage(
  "Flowing in and Flowing out",
  tabPanel("3D Globe",
           titlePanel("Coffee ,tea, and others traded between US and the world"),
           sidebarLayout(
             sidebarPanel(
               radioButtons(inputId = "type",
                            label  = "Choose import/export",
                            choices = c('Export','Import'),
                            selected ='Export'),
               sliderInput(inputId = "year_3D",
                           label = "Select a year",
                           value = 1996, min =1996, max =2016),
               sliderInput(inputId = "number_countries",
                           label = "Top Countries in Trade",
                           value = 10,min = 1,max = 50),
               selectInput(inputId = "commodity_3D",
                           label  = "Select the commodity",
                           choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                           selected ='Coffee')
             ),
             mainPanel(
               globeOutput("Globe"))
           ),
           sidebarLayout(
             sidebarPanel(
               sliderInput(inputId = "year_linear_graph",
                           label = "choose a year",
                           value = 1996, min =1996, max =2016)),
             mainPanel (
               plotOutput("linear")
             )
           )
  ),
  tabPanel("2D Map",
           titlePanel("Coffee ,tea, and others traded between US and the world"),
           sidebarLayout(
             sidebarPanel(
               radioButtons(inputId = "type_2D",
                            label  = "Choose import/export",
                            choices = c('Export','Import'),
                            selected ='Export'),
               sliderInput(inputId = "year_2D",
                           label = "Select a year",
                           value = 1996, min =1996, max =2016),
               sliderInput(inputId = "num_countries",
                           label = "Top Countries in Trade",
                           value = 10,min = 1,max = 50),
               selectInput(inputId = "commodity_2D",
                           label  = "Select the commodity",
                           choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                           selected ='Coffee')
             ),
             mainPanel(
               leafletOutput("mymap",width = "100%", height = 600))
           )
  ),
  navbarMenu("Summary Stastics",
             tabPanel("Exchange Rate", sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "exchange_commodity",
                             label  = "choose the commodity",
                             choices = c('Annual Aggregate','Chocolate', 'Coffee','COCOA','Spices','Tea'),
                             selected ='SPICES'),
                 selectInput(inputId = "exchange_country",
                             label  = "choose the country",
                             choices = unique(import$Country),
                             selected ='China')
               ),
               
               mainPanel(
                 plotOutput("linear_exchange")
               )
             )
                      
                      ),
             tabPanel("Motion chart",
                     mainPanel(
                          htmlOutput("view")
                          ))
             tabPanel("Tree map",
                      titlePanel("Tree map for certain year and commodity"),
                      sidebarLayout(
                        sidebarPanel = (
                          selectInput(
                            inputId = "com_tree",
                            label  = "Choose the commodity",
                            choices = c('Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                            selected ='Tea')),
                          sliderInput(
                            inputId = "year_tree",
                            label = "Select a year",
                            value = 1996, min =1996, max =2016)),
                      mainPanel(
                        plotOutput("treemap",height = 600, width = 600)))
             )),
  
  tabPanel("More")
)



## map creation preprocess
data(wrld_simpl) # Basic country shapes
bgcolor = "#000000"
arc_colors = c("#ffd3d3","#d3ffd3","#d3d3ff","#ffffd3","#d3ffff","#ffd3ff")
map_pal = data.frame(AnnualAggregate = c(0,0,0,0,0,0),
                     Chocolate = c(0,0,0,0,0,0), 
                     Coffee = c(0,0,0,0,0,0), 
                     Cocoa = c(0,0,0,0,0,0), 
                     Spices = c(0,0,0,0,0,0), 
                     Tea = c(0,0,0,0,0,0))
map_pal$AnnualAggregate = c("#151010","#201515","#252020","#302525","#403535","#665d5d")
map_pal$Chocolate = c("#101510","#152015","#202520","#253025","#354035","#5d665d")
map_pal$Coffee = c("#101015","#151520","#202025","#252530","#353540","#5d5d66")
map_pal$Cocoa = c("#151510","#202015","#252520","#303025","#404035","#66665d")
map_pal$Spices = c("#101515","#152020","#202525","#253030","#354040","#5d6666")
map_pal$Tea = c("#151015","#201520","#252025","#302530","#403540","#665d66")
names(map_pal)[1] = "Annual Aggregate"
## end preprocess map

## Server function

server<- function(input, output){
  output$Globe <- renderGlobe({
    ##### subset dataframe
    temp = input_data
    temp = subset(temp,Commodity_Name == as.character(input$commodity_3D))
    temp = subset(temp,Year == as.integer(input$year_3D))
    temp = subset(temp,type == as.character(input$type))
    temp = arrange(temp,desc(value))[1:input$number_countries,]
    index = match(input$commodity_3D,c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'))
    ##### end subset
    
    ##### map colors creation
    earth <- tempfile(fileext=".jpg")
    jpeg(earth, width=2048, height=1024, quality=100, bg=bgcolor, antialias="default")
    par(mar = c(0,0,0,0),    pin = c(4,2),    pty = "m",    xaxs = "i",
        xaxt = "n",          xpd = FALSE,    yaxs = "i",    yaxt = "n")
    
    map_palette = map_pal[,index]
    clrs = rep('#050505', length(wrld_simpl$NAME))
    names(clrs) = wrld_simpl$NAME
    clrs[temp$Country] <- map_palette[temp$log]
    
    plot(wrld_simpl,  col=clrs,   bg=bgcolor,  border="#757575", cex = 0.1,  ann=FALSE,
         axes=FALSE,  xpd=FALSE,  xlim=c(-180,180), ylim=c(-90,90),  setParUsrBB=TRUE)
    
    graphics.off()
    
    ##### end map creation
    
    ## Globe plotting
    globejs(earth, bg="black", emissive="#aaaacc",
            fov = 38,
            arcs=temp[,c(4,3,9,8)],
            arcsHeight=0.35, 
            arcsLwd=2, 
            arcsColor = arc_colors[index], 
            arcsOpacity=1,
            atmosphere=TRUE, height=600, width = 600
    )
    ## end globe plotting
  })
  
  ## 2D map
    output$mymap <- renderLeaflet({
    ## Control Icon size and looks
      Icon <- makeIcon(
                        iconWidth = 3, iconHeight = 3,
                        iconAnchorX = 19, iconAnchorY = 19
                      )
    ## Subset the data
      US = data.frame(Country = "US",longitude = -95.71289,latitude = 37.09024)
      ##### subset dataframe
      tmp = input_data
      tmp = subset(tmp,Commodity_Name == as.character(input$commodity_2D))
      tmp = subset(tmp,Year == as.integer(input$year_2D))
      tmp = subset(tmp,type == as.character(input$type_2D))
      tmp = arrange(tmp,desc(value))[1:input$num_countries,]
      rank = 1:nrow(tmp)
      tmp$rank = paste(tmp$Country,"ranks No.",rank)
      index = match(input$commodity_2D,c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'))
      ##### end subset      
      leaflet(tmp)%>%addProviderTiles("Esri.WorldStreetMap")%>%
        addMarkers(popup=~rank,icon = Icon)%>%
        
        addMarkers(data = US, popup=~Country,icon = Icon)%>%  
        setView(lng=116.38,lat=39.9,zoom=2)
  })
    output$linear_exchange <-renderPlot({
      title <- paste(input$exchange_country, input$exchange_commodity, "import v.s. exchange rate",sep = " ")
      temp <- filter(import,import$Commodity_Name== input$exchange_commodity,
                     import$Country == input$exchange_country)
      plot(temp$rate,temp$value, main = title,
           xlab="exchange rate", ylab="yearly import")
      text(temp$rate, temp$value, temp$Year, cex=0.6, pos=4, col="red")
    })
  output$view <- renderGvis({
    gvisMotionChart(country, idvar='Country',timevar = 'Year', sizevar='Coffee', options=list(width="800", height="800"))
  })
  output$treemap<-renderPlot({
   country<-read.csv("country_cleaned.csv")
   #selcet a year and a one of the five categories
   sub_country<-country[country$Year==input$year_tree,]
   sub_country<-data.frame(sub_country,y=1:nrow(sub_country))
   treemap(sub_country, index='Country', vSize=toString(input$com_tree), vColor="y", type="index", palette="RdYlBu",aspRatio=30/30)
 })
}


## Calling shiny App
shinyApp(ui = ui, server= server)
