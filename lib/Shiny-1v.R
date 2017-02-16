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
    "shiny"
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
    navbarMenu("Summary Stastics",
               tabPanel("Component A"),
               tabPanel("Component B")),
    tabPanel("More")
)

## preprocess work, Load dataframe already prepared for plotting
input_data =  read.csv("data/mydata.csv",header = T,as.is = T)
input_data = input_data[!is.na(input_data$longitude),]
input_data = input_data[input_data$value != 0,]
#create 6 level for value data whose magnitude ranges from 1e3 tp 1e8
input_data$log = ceiling(log(input_data$value)/3)-2
## end preprocess data

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
    clrs[input_data$Country] <- map_palette[input_data$log]
    
    plot(wrld_simpl,  col=clrs,   bg=bgcolor,  border="#757575", cex = 0.1,  ann=FALSE,
         axes=FALSE,  xpd=FALSE,  xlim=c(-180,180), ylim=c(-90,90),  setParUsrBB=TRUE)
    
    graphics.off()
    
    legendcol=heat.colors(5)[5:1]
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
}


## Calling shiny App
shinyApp(ui = ui, server= server)