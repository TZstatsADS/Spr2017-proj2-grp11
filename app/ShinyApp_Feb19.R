
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
source("../lib/double-axis.R")
## preprocess work, Load dataframe already prepared for plotting
input_data =  read.csv("../data/mydata_wRegions.csv",header = T,as.is = T)
input_data = input_data[!is.na(input_data$longitude),]
input_data = input_data[input_data$value != 0,]
#create 6 level for value data whose magnitude ranges from 1e3 tp 1e8
input_data$log = ceiling(log(input_data$value)/3)-2
#Load the data for Google motion data
country<-read.csv("../data/country_cleaned.csv")
## end preprocess data



## mergring exchange rate data
exchange_rate =  read.csv("../data/exchange_rate.csv")
CPI =  read.csv("../data/CPI.csv")
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
  
  ##link to css.file
  theme = "bootstrap.css",
  
  ##Project Title
  "Flowing in and Flowing out",
  
  ## 3D Globe tab
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
                           selected ='Coffee'),
               width = 3
             ),
             mainPanel(
               globeOutput("Globe",width="100%",height="600px"),
               plotOutput("ggplot",width="100%",height="200px")
             )
           )
  ),
  ## end 3D Globe tab
  
  ## 2D Map tab
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
                           value = 2016, min =1996, max =2016),
               sliderInput(inputId = "num_countries",
                           label = "Top Countries in Trade",
                           value = 30,min = 1,max = 50),
               selectInput(inputId = "commodity_2D",
                           label  = "Select the commodity",
                           choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                           selected ='Coffee'),
               width = 3
             ),
             mainPanel(
               leafletOutput("mymap",width = "100%", height = 600)
             )
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
             ### Tree Map
             tabPanel("Tree Plot",
                      titlePanel("Tree map for certain year and commodity"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput(inputId = "com_tree",
                                      label  = "Select the commodity",
                                      choices = c('Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                                      selected ='Coffee'),
                         sliderInput(
                            inputId = "year_tree",
                            label = "Select a year",
                            value = 1996, min =1996, max =2016)),
                        
                        mainPanel(
                          plotOutput("treemap",width = "100%", height = 600)
                        )
                      )
             ),
             ### end Tree Map
             ### Exchange Rate
             tabPanel("Exchange Rate", sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "exchange_commodity",
                             label  = "choose the commodity",
                             choices = unique(import$Commodity_Name),
                             selected ='Spices'),
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
             ### end Exchange Rate
             
             ### Mirror Histogram
             tabPanel("Mirror Histogram",
              titlePanel("Trade Trend vs Exchange Rate"),
              sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "commodity_hist",
                             label  = "choose the commodity",
                             choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                             selected ='Tea'),
                 selectInput(inputId = "country_hist",
                             label  = "choose the country",
                             choices = unique(input_data$Country),
                             selected ='Canada')
               ),
               
               mainPanel(
                 plotOutput("Hist")
               )
             )
             
             ),
             ### end Mirror Histogram
             
             tabPanel("Continent statistics",sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "continent_commodity",
                             label  = "choose the commodity",
                             choices = unique(input_data$Commodity_Name),
                             selected ='Spices'),
                 sliderInput(inputId = "continent_year",
                             label = "Select a year",
                             value = 2000, min =1996, max =2016)
               ),
               
               mainPanel(
                 plotOutput("continent_import")
               )
             )
             ),
             tabPanel("Regional statistics",sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "regional_commodity",
                             label  = "choose the commodity",
                             choices = unique(input_data$Commodity_Name),
                             selected ='Spices')
               ),
               
               mainPanel(
                 plotlyOutput("regional_import")
               )
             )
             )
  ),
  ## end Summary Statistics tab
  
  ## Clustering tab
  
  tabPanel("Clustering Analysis",
           titlePanel("Clustering Analysis"),
           sidebarLayout(
             sidebarPanel(
               radioButtons(inputId = "type",
                            label  = "Choose import/export",
                            choices = c('Export','Import'),
                            selected ='Export'),
               sliderInput(inputId = "number_clusters",
                           label = "Number of Clusters",
                           value = 5,min = 2,max = 20),
               width = 3
             ),
             mainPanel(
               plotlyOutput("cluster", width = "100%", height = "400px"),
               verbatimTextOutput("click")
             )
           )
  
  ),
  ## end Clustering tab
  
  tabPanel("More")
)

## map creation preprocess
data(wrld_simpl) # Basic country shapes
bgcolor = "#000000"
arc_colors = c("#998080","#809980","#808099","#999980","#809999","#998099")
map_pal = data.frame(AnnualAggregate = c("red"),Chocolate = c("blue"),Coffee = c("green"),COCOA = c("#ffe9bf"),Spices = c("pink"),Tea = c("orange"))
names(map_pal)[1] = "Annual Aggregate"
## end preprocess map

## Load clustering data
cluster_data_import = read.csv("../data/clustering-ready-Import.csv")
code = read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')[,c(1,3)]

## end loading cluster data

## Server function

server<- function(input, output){
  
  ## 3D Globe
  output$Globe <- renderGlobe({
    ##### subset dataframe
    temp = input_data
    temp = subset(temp,Commodity_Name == as.character(input$commodity_3D))
    temp = subset(temp,Year == as.integer(input$year_3D))
    temp = subset(temp,type == as.character(input$type))
    temp = arrange(temp,desc(value))[1:input$number_countries,]
    index = match(input$commodity_3D,c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'))
    maxValue = log(max(temp$value))
    ##### end subset
    
    ##### map colors creation
    earth <- tempfile(fileext=".jpg")
    jpeg(earth, width=2048, height=1024, quality=100, bg=bgcolor, antialias="default")
    par(mar = c(0,0,0,0),    pin = c(4,2),    pty = "m",    xaxs = "i",
        xaxt = "n",          xpd = FALSE,    yaxs = "i",    yaxt = "n")
    
    map_palette = map_pal[,index]
    clrs = rep('#050505', length(wrld_simpl$NAME))
    names(clrs) = wrld_simpl$NAME
    clrs[temp$Country] = alpha(map_palette[1], log(temp$value)/maxValue*0.1)
    
    plot(wrld_simpl,  col=clrs,   bg=bgcolor,  border="#757575", cex = 0.1,  ann=FALSE,
         axes=FALSE,  xpd=FALSE,  xlim=c(-180,180), ylim=c(-90,90),  setParUsrBB=TRUE)
    
    graphics.off()
    ##### end map creation
    
    ##### globe plotting
    globejs(earth, bg="black", emissive="#aaaacc",
            fov = 38,
            arcs=temp[,c(4,3,9,8)],
            arcsHeight=0.35, 
            arcsLwd=2, 
            arcsColor = arc_colors[index], 
            arcsOpacity=1,
            atmosphere=TRUE, height=600, width = 600
    )
  })
  ## end 3D Globe
  
  ## ggplot
  output$ggplot <- renderPlot({
    
    ##### subset dataframe
    temp = input_data
    temp = subset(temp,Commodity_Name == as.character(input$commodity_3D))
    temp = subset(temp,Year == as.integer(input$year_3D))
    temp = subset(temp,type == as.character(input$type))
    temp = arrange(temp,desc(value))[1:input$number_countries,]
    index = match(input$commodity_3D,c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'))
    maxValue = log(max(temp$value))
    map_palette = map_pal[,index]
    clrs = rep('#050505', length(wrld_simpl$NAME))
    names(clrs) = wrld_simpl$NAME
    clrs[temp$Country] = alpha(map_palette[1], log(temp$value)/maxValue*0.1)
    ##### end subset
    
    g = ggplot(data = temp, aes(x = Country, y = value)) + theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "white")) +theme(legend.position="none") + theme(legend.background = element_rect(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank()) + geom_bar(stat = "identity", aes(fill=temp$value)) + scale_fill_gradient(low = "#a7a7a7", high = "#dbdbdb") + scale_x_discrete(limits = temp$Country) + theme(panel.background = element_rect(fill = "#000000")) + theme(plot.background = element_rect(fill = "#000000")) + theme(panel.background = element_rect(colour = "#050505"))
    g
    
  })
  ## end ggplot
  
  
  
  ## 2D map
  output$mymap <- renderLeaflet({
    ## Control Icon size and looks
    levelIcon <- iconList(
      level1 = makeIcon("css-ShinyApp_split/www/trade-icon_1.png", iconAnchorX = 19, iconAnchorY = 19),
      level2 = makeIcon("css-ShinyApp_split/www/trade-icon_2.png", iconAnchorX = 19, iconAnchorY = 19),
      level3 = makeIcon("css-ShinyApp_split/www/trade-icon_3.png", iconAnchorX = 19, iconAnchorY = 19),
      level4 = makeIcon("css-ShinyApp_split/www/trade-icon_4.png", iconAnchorX = 19, iconAnchorY = 19),
      level5 = makeIcon("css-ShinyApp_split/www/trade-icon_5.png", iconAnchorX = 19, iconAnchorY = 19),
      level6 = makeIcon("css-ShinyApp_split/www/trade-icon_6.png", iconAnchorX = 19, iconAnchorY = 19),
      level7 = makeIcon("css-ShinyApp_split/www/trade-icon_7.png", iconAnchorX = 19, iconAnchorY = 19),
      level8 = makeIcon("css-ShinyApp_split/www/trade-icon_8.png", iconAnchorX = 19, iconAnchorY = 19)
    )
    Icon = makeIcon(iconAnchorX = 19, iconAnchorY = 19,
                    iconWidth = 38, iconHeight = 38)
    ## subset the data
    US = data.frame(Country = "US",longitude = -95.71289,latitude = 37.09024)
    ##### subset dataframe
    tmp = input_data
    tmp = subset(tmp,Commodity_Name == as.character(input$commodity_2D))
    tmp = subset(tmp,Year == as.integer(input$year_2D))
    tmp = subset(tmp,type == as.character(input$type_2D))
    tmp = arrange(tmp,desc(value))[1:input$num_countries,]
    rank = 1:nrow(tmp)
    Log = paste("level",ceiling(log(tmp$value)/2)-3,sep = "")
    tmp$rank = paste(tmp$Country,"<br/>",
                     "ranks No.",rank,"<br/>",
                     "Annual Trade Value: $",tmp$value,"<br/>",sep = "",
                     "<a href='https://en.wikipedia.org/wiki/",tmp$Country,"'>Wikipedia Page</a>","<br/>",
                     "<a href='https://www.wsj.com/search/term.html?KEYWORDS=",tmp$Country,"'>Wall Street Journal Page</a>"
    )
    index = match(input$commodity_2D,c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'))
    Colors = c("#231d65","#276d98","#2586a4","#3c6049","#216957","#4abf8c","#9eae1e","#eff09e")
    Labels = paste("Level:",1:8)
    ##### end subset      
    leaflet(tmp)%>%addProviderTiles("Esri.WorldStreetMap")%>%
      addMarkers(popup=~rank,icon = ~levelIcon[Log])%>%
      addMarkers(data = US, 
                 popup=~Country,icon = ~Icon)%>%  
      setView(lng=-30,lat=28,zoom=3)%>%#put US in the centre
      addLegend("bottomright", colors = Colors, labels = Labels,
                title = "Value From Large to Small",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1)
  })
  ## end 2D map
  
  
  
  ## MotionChart
  output$view <- renderGvis({
    
    gvisMotionChart(country, idvar='Country',timevar = 'Year', sizevar='Coffee',colorvar = 'Coffee', options=list(width="800", height="800"))
  })
  ## end MotionChart
  
  ## Tree Map
  output$treemap<-renderPlot({
    country<-read.csv("../data/country_cleaned.csv")
    #selcet a year and a one of the five categories
    sub_country<-country[country$Year==input$year_tree,]
    sub_country<-data.frame(sub_country,y=1:nrow(sub_country))
    sub_country[,3]<-as.numeric( sub_country[,3])
    treemap(sub_country, index='Country', vSize=input$com_tree, vColor="y", type="index", palette="RdYlBu",aspRatio=30/30)
  })
  ## end Tree Map
  
  ## exchange rate
  output$linear_exchange <-renderPlot({
    title <- paste(input$exchange_country, input$exchange_commodity, "import v.s. exchange rate",sep = " ")
    temp <- filter(import,import$Commodity_Name== input$exchange_commodity,
                   import$Country == input$exchange_country)
    plot(temp$rate,temp$value, main = title,
         xlab="exchange rate", ylab="yearly import")
    text(temp$rate, temp$value, temp$Year, cex=0.6, pos=4, col="red")
  })
  ##end exchange rate
  
  ## Mirror Histogram
  output$Hist <- renderPlot({
    ##Subset
    tp = input_data
    tp = subset(tp,Country == as.character(input$country_hist))
    tp = subset(tp,Commodity_Name == as.character(input$commodity_hist))
    tp = tp[order(tp$type,decreasing = T),]#put import first
    Rate = exchange_rate
    Rate = subset(Rate,Country.Name == as.character(input$country_hist))
    ##Data frame for ggplot2
    dat <- data.frame(
      group = tp$type,
      Year = tp$Year,
      Value = ifelse(tp$type == "Import",tp$value,-tp$value)#import on upside, export on downside
    )
    
    ##plotting
    library(grid)
    plot1  = ggplot(dat, aes(x=Year, y=Value, fill=group))+
      geom_bar(stat="identity", position="identity")+
      scale_fill_manual(values=c("#87CEFA","#DC143C"))
    plot2 = ggplot(Rate,aes(x = year,y = rate))+
      geom_line(color = "Black")+
      coord_cartesian(ylim=c(0, max(Rate$rate)))+
      ylab("Exchange rate")
    plot1 <- plot1 + theme_bw() + theme(legend.position="top")
    plot2 <- plot2 + theme_bw() + theme(panel.grid=element_blank()) +
      theme(panel.background = element_rect(fill = NA))
    #plot the exchange rate line on the histogram
    #with 2 different y-axis
    #use self-written function"double_axis_graph
    plot(double_axis_graph(plot1,plot2))
  })
  ## end Mirror Histogram
  
  ##continent analysis
  output$continent_import <- renderPlot({
    input_data$value<-as.numeric(input_data$value)
    title <- paste(input$continent_year, input$continent_commodity, "import",sep = " ")
    temp <- filter(input_data, input_data$Year == input$continent_year ,
                   input_data$type == "Import",
                   input_data$Commodity_Name == input$continent_commodity)
    temp_1<-aggregate(value ~ Continent, temp, sum)
    pie(temp_1$value, labels = temp_1$Continent,  main = title)
  })
  ##
  
  ##Regional analysis
  output$regional_import <- renderPlotly({
    temp <- filter(input_data ,
                   input_data$type == "Import",
                   input_data$Commodity_Name == input$regional_commodity)
    
    p <- plot_ly()
    
    for (i in  unique (temp$Region)){
      temp_1 <- filter(temp , temp$Region ==i)
      temp_1 <- group_by(temp_1,Year)%>% summarise(value = sum(value))
      p <- add_trace(p, x = temp_1$Year, y = temp_1$value, mode = "lines+markers", name = i)
      
    }
    p
  })
  ##
  
  ## Cluster visuals
  output$cluster <- renderPlotly({
    
    k = input$number_clusters
    clusters = kmeans(t(cluster_data_import[,3:dim(cluster_data_import)[2]]),k)$cluster
    df = as.data.frame(clusters)
    df$COUNTRY = rownames(df)
    df = merge(x = df, y = code, all.y = TRUE)
    df[is.na(df$clusters),2] = 0
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df) %>%
      add_trace(
        z = ~clusters, color = ~clusters, colors = brewer.pal(k, "RdYlGn"), type = "scatter", 
        text = ~COUNTRY, locations = ~CODE, marker = list(line = 'l')
      ) %>%
      colorbar(title = 'Cluster number', tickprefix = '') %>%
      layout(
        title = 'Clustering Visual',
        geo = g
      )
  })
  
  output$click <- renderPrint({
    d <- event_data("plotly_click")
    if (is.null(d)) "Click on a state to view event data" else d
  })
  ## end cluster visual
  
}


shinyApp(ui = ui, server= server)
