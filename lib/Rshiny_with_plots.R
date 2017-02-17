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
    "dplyr",
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
source("lib/Match_flora.R")
source("lib/plot_3D.R")
library("maptools")
library("shiny")

## preprocess work, Load dataframe already prepared for plotting
input_data =  read.csv("data/mydata.csv",header = T,as.is = T)
exchange_rate =  read.csv("data/exchange_rate.csv")
input_data = input_data[!is.na(input_data$longitude),]
input_data = input_data[input_data$value != 0,]
input_data$log = trunc(log(input_data$value)/4)
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

ui<- fluidPage(
  titlePanel("US commodity imports from across the globe"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_3D",
                  label = "Select a year",
                  value = 1996, min =1996, max =2016),
      selectInput(inputId = "commodity_3D",
                  label  = "Select the commodity",
                  choices = c('Annual Aggregate','Chocolate', 'Coffee','COCOA','Spices','Tea'),
                  selected ='Coffee'),
      selectInput(inputId = "type",
                  label  = "Choose import/export",
                  choices = c('Export','Import'),
                  selected ='Export'),
      sliderInput(inputId = "number_countries",
                  label = "Top k countries",
                  value = 10, min =1, max =50)
    ),
  mainPanel(
    globeOutput("Globe")
  )
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "exchange_commodity",
                  label  = "choose the commodity",
                  choices = c('Annual Aggregate','Chocolate', 'Coffee','COCOA','Spices','Tea'),
                  selected ='SPICES'),
      selectInput(inputId = "exchange_country",
                  label  = "choose the country",
                  choices = unique(import_by_country$Country),
                  selected ='China')
    ),
    
    mainPanel(
      plotOutput("linear_exchange")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_sankey",
              label = "choose a year",
              value = 1996, min =1996, max =2016),
      selectInput(inputId = "country_sankey",
                  label  = "choose the country",
                  choices = unique(import_by_country$Country),
                  selected ='China')
    ),
    mainPanel (
      htmlOutput("sankey")
    )
  )
  
)


## map creation preprocess
data(wrld_simpl) # Basic country shapes
bgcolor = "#000000"
arc_colors = c("#998080","#809980","#808099","#999980","#809999","#998099")
map_pal = data.frame(AnnualAggregate = c(0,0,0,0,0),Chocolate = c(0,0,0,0,0), Coffee = c(0,0,0,0,0), COCOA = c(0,0,0,0,0), Spices = c(0,0,0,0,0), Tea = c(0,0,0,0,0))
map_pal$AnnualAggregate = c("#151010","#201515","#252020","#302525","#403535")
map_pal$Chocolate = c("#101510","#152015","#202520","#253025","#354035")
map_pal$Coffee = c("#101015","#151520","#202025","#252530","#353540")
map_pal$COCOA = c("#151510","#202015","#252520","#303025","#404035")
map_pal$Spices = c("#101515","#152020","#202525","#253030","#354040")
map_pal$Tea = c("#151015","#201520","#252025","#302530","#403540")
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
    temp = arrange(temp, desc(value))[1:input$number_countries,]
    index = match(input$commodity_3D,c('Annual Aggregate','Chocolate', 'Coffee','COCOA','Spices','Tea'))
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
    globejs(earth, bg="white", emissive="#aaaacc",
            arcs=temp[,c(4,3,9,8)],
            arcsHeight=0.4, 
            arcsLwd=2, 
            arcsColor = arc_colors[index], 
            arcsOpacity=0.5,
            atmosphere=FALSE, height=600, width = 600
    )
    ## end globe plotting
  })
  output$linear_exchange <-renderPlot({
    title <- paste(input$exchange_country, input$exchange_commodity, "import v.s. exchange rate",sep = " ")
    temp <- filter(import,import$Commodity_Name== input$exchange_commodity,
                   import$Country == input$exchange_country)
    plot(temp$rate,temp$value, main = title,
         xlab="exchange rate", ylab="yearly import")
    text(temp$rate, temp$value, temp$Year, cex=0.6, pos=4, col="red")
  })
  output$sankey<- renderGvis({
    df<- filter(import.without.aggregate,
                import.without.aggregate$Country == input$country_sankey)
    df<- filter(df, df$Year == input$year_sankey)
    myvars <- c("source", "target", "value")
    df <- df[myvars]
    plot(
      gvisSankey(df, from="source",
                 to="target", weight="value")
      )
    
  })
}


## Calling shiny App
shinyApp(ui = ui, server= server)
