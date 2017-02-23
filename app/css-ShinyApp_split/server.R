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
source("../../lib/double-axis.R")
## preprocess work, Load dataframe already prepared for plotting
input_data =  read.csv("../../data/mydata_wRegions.csv",header = T,as.is = T)
input_data = input_data[!is.na(input_data$longitude),]
input_data = input_data[input_data$value != 0,]
input_data[!is.na(input_data$Commodity_Name) & input_data$Commodity_Name == "COCOA",10] = "Cocoa"
#Load the data for Google motion data
country<-read.csv("../../data/country_cleaned.csv")
# force all values in country dataset to be numeric
for (i in 3:7){
  country[,i]<-as.numeric(country[,i])
}
## end preprocess data



## mergring exchange rate data
exchange_rate =  read.csv("../../data/exchange_rate.csv")
CPI =  read.csv("../../data/CPI.csv")
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
cluster_data_import = read.csv("../../data/clustering-ready-Import.csv")
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
            arcsOpacity=0.5,
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
    
    g = ggplot(data = temp, aes(x = Country, y = value))+
      geom_bar(stat='identity',position = "dodge")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "white")) +
      theme(legend.position="none") + 
      theme(legend.background = element_rect(),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank()) + geom_bar(stat = "identity", aes(fill=temp$value)) + scale_fill_gradient(low = "#a7a7a7", high = "#dbdbdb") + scale_x_discrete(limits = temp$Country) + theme(panel.background = element_rect(fill = "#000000")) + theme(plot.background = element_rect(fill = "#000000")) + theme(panel.background = element_rect(colour = "#050505"))
    g+
      coord_flip()
    
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
    tmp = arrange(tmp,desc(value))[1:50,]
    min = min(tmp$value, na.rm = TRUE)
    tmp = tmp[1:input$num_countries,]
    rank = 1:nrow(tmp)
    max = max(tmp$value, na.rm = TRUE)
    
    Log = paste("level",floor(log((tmp$value) - min + 1, base =1.0001)/log(max - min + 1, base =1.0001) * 7 + 1),sep = "")
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
      setView(lng=-30,lat=28,zoom=2)%>%#put US in the centre
      addLegend("topright", colors = Colors, labels = Labels,
                title = "Trade Level<br/>From Small to Large",
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
    #selcet a year and a one of the five categories
    sub_country<-country[country$Year==input$year_tree,]
    sub_country[nrow(sub_country)+1,3:7]<-colSums(sub_country[,3:7])
    for(i in 3:7){
      sub_country[,i]<-sub_country[,i]/sub_country[nrow(sub_country),i]
    }
    sub_country<-sub_country[1:nrow(sub_country)-1,]
    sub_country$label<-paste(sub_country$Country,", ",round(100*sub_country[,as.character(input$com_tree)]),"%",sep="")
    treemap(sub_country, index='label', vSize=input$com_tree, vColor="Country", type="categorical", palette="RdYlBu",aspRatio=30/30,drop.unused.levels = FALSE, position.legend="none")
  })
  ## end Tree Map
  
  ##continent analysis
  output$continent_import <- renderPlot({
    input_data$value<-as.numeric(input_data$value)
    title <- paste(input$year, input$commodity, "import",sep = " ")
    temp <- filter(input_data, input_data$Year == input$year ,
                   input_data$type == "Import",
                   input_data$Commodity_Name == input$commodity)
    temp_1<-aggregate(value ~ Continent, temp, sum)
    pie(temp_1$value, labels = temp_1$Continent,  main = title)
  })
  ##
  
  ##Regional analysis
  output$regional_import <- renderPlotly({
    temp <- filter(input_data ,
                   input_data$type == "Import",
                   input_data$Commodity_Name == input$commodity)
    
    p <- plot_ly()
    
    for (i in  unique (temp$Region)){
      temp_1 <- filter(temp , temp$Region ==i)
      temp_1$value <- as.numeric(temp_1$value)
      temp_1 <- group_by(temp_1,Year)%>% summarise(value = sum(value))
      p <- add_trace(p, x = temp_1$Year, y = temp_1$value, mode = "lines+markers", name = i)
      
    }
    p
  })
  ##
  
  
  ## exchange rate
  output$linear_exchange <-renderPlot({
    title <- paste(input$exchange_country, input$exchange_commodity, "import v.s. exchange rate",sep = " ")
    temp <- filter(import,import$Commodity_Name== input$exchange_commodity,
                   import$Country == input$exchange_country)
    dat = data.frame(rate = temp$rate,value = temp$value,year = temp$Year)
    ggplot(dat, aes(x=rate, y=value)) +
      geom_point(aes(colour = value)) + 
      scale_colour_gradient(low = "blue")+
      aes(size = value)+
      ggtitle(title)+
      theme(plot.title = element_text(lineheight=3, face="bold", color = "#666666", size=24,hjust = 1))+
      xlab("Exchange rate")+
      ylab("yearly import")+
      theme(axis.title.y = element_text(color = "#666666",size = rel(1.8), angle = 0))+
      theme(axis.title.x = element_text(color = "#666666",size = rel(1.8), angle = 0))+
      geom_smooth(method=lm)
    #      +
    #    geom_text(aes(x=rate, y=value, label=year, fill=1))
  })
  ##end exchange rate
  
  ## Mirror Histogram
  #####First Histogram
  output$Hist1 <- renderPlot({
    ##Subset
    tp = input_data
    tp = subset(tp,Country == as.character(input$country_hist1))
    tp = subset(tp,Commodity_Name == as.character(input$commodity_hist))
    tp = tp[order(tp$type,decreasing = T),]#put import first
    Rate = exchange_rate
    Rate = subset(Rate,Country.Name == as.character(input$country_hist1))
    ##Data frame for ggplot2
    dat1 <- data.frame(
      group = tp$type,
      Year = tp$Year,
      Value = ifelse(tp$type == "Import",tp$value,-tp$value)#import on upside, export on downside
    )
    
    #### Plotting
    plot1  = ggplot(dat1, aes(x=Year, y=Value, fill=group))+
      geom_bar(stat="identity", position="identity")+
      scale_fill_manual(values=c("#87CEFA","#DC143C"))+
      coord_cartesian(xlim=c(1996, 2016))
    if(length(Rate$rate)){
      plot2 = ggplot(Rate,aes(x = year,y = rate))+
        geom_line(color = "Black")+
        coord_cartesian(ylim=c(0, max(Rate$rate)))+
        ylab("Exchange rate")
    }
    else if(length(Rate$rate) == 0){
      empty <- data.frame(
        year = 1996,
        rate = 0)
      plot2 = ggplot(empty,aes(x = year,y = rate))
    }
    
    plot1 <- plot1 + theme_bw() + theme(legend.position="top")
    plot2 <- plot2 + theme_bw() + theme(panel.grid=element_blank()) +
      theme(panel.background = element_rect(fill = NA))
    #plot the exchange rate line on the histogram
    #with 2 different y-axis
    #use self-written function"double_axis_graph
    plot(double_axis_graph(plot1,plot2))
  })
  
  ######Second Histogram
  output$Hist2 <- renderPlot({
    ##Subset
    tp = input_data
    tp = subset(tp,Country == as.character(input$country_hist2))
    tp = subset(tp,Commodity_Name == as.character(input$commodity_hist))
    tp = tp[order(tp$type,decreasing = T),]#put import first
    Rate = exchange_rate
    Rate = subset(Rate,Country.Name == as.character(input$country_hist2))
    ##Data frame for ggplot2
    dat2 <- data.frame(
      group = tp$type,
      Year = tp$Year,
      Value = ifelse(tp$type == "Import",tp$value,-tp$value)#import on upside, export on downside
    )
    ######## plotting
    plot1  = ggplot(dat2, aes(x=Year, y=Value, fill=group))+
      geom_bar(stat="identity", position="identity")+
      scale_fill_manual(values=c("#87CEFA","#DC143C"))+
      coord_cartesian(xlim=c(1996, 2016))
    if(length(Rate$rate)){
      plot2 = ggplot(Rate,aes(x = year,y = rate))+
        geom_line(color = "Black")+
        coord_cartesian(ylim=c(0, max(Rate$rate)))+
        ylab("Exchange rate")
    }
    else if(length(Rate$rate) == 0){
      empty <- data.frame(
        year = 1996,
        rate = 0)
      plot2 = ggplot(empty,aes(x = year,y = rate))
    }
    
    plot1 <- plot1 + theme_bw() + theme(legend.position="top")
    plot2 <- plot2 + theme_bw() + theme(panel.grid=element_blank()) +
      theme(panel.background = element_rect(fill = NA))
    #plot the exchange rate line on the histogram
    #with 2 different y-axis
    #use self-written function"double_axis_graph
    plot(double_axis_graph(plot1,plot2))
  })
  ## end Mirror Histogram
  
  
  ## Cluster visuals
  output$cluster <- renderPlotly({
    k = input$number_clusters
    newcountry<-country[country$Year==input$year_cluster,]
    #choose the five columns with different commodity values
    newcountry1<-newcountry[,3:7]
    #store the result of kmeans cluster in "cls_result"
    cls_result<-kmeans(newcountry1,k)
    clusters<-cls_result$cluster
    df = as.data.frame(clusters)
    df$COUNTRY = newcountry[,2]
    df = merge(x = df, y = code, all.y = TRUE)
    df[is.na(df$clusters),2] = 0
    ## end cluster visual
    
    g <- list(
      showframe = FALSE,
      showcoastlines = FALSE,
      projection = list(type = 'Mercator')
    )
    
    plot_geo(df) %>%
      add_trace(
        z = ~clusters, color = ~clusters, colors = brewer.pal(k, "RdYlGn"), type = "scatter", 
        text = ~COUNTRY, locations = ~CODE, marker = list(line = 'l'), showlegend = FALSE
      ) %>%
      layout(
        title = paste(k,"clusters for all countries concerning","Import",sep=" "),
        geo = g
      ) %>%
      hide_colorbar()
    
  })
  
  output$text_1<- renderText({
    "Click on a Country to view cluster result" 
  })
  output$text_2<- renderText({
    "Trade Magnitude and number of countries in each cluster as follows:"
  })
  
  output$mytable<-renderDataTable({
    k=input$number_clusters
    newcountry <- country[country$Year==input$year_cluster,]
    #choose the five columns with different commodity values
    newcountry1<-newcountry[,3:7]
    cls_result<-kmeans(newcountry1,k)
    
    newcountry$cluster = cls_result$cluster
    by_clust = group_by(newcountry,cluster)
    by_clust = as.data.frame(summarise(by_clust, 
                                       mean(Coffee),
                                       mean(Tea), 
                                       mean(Spices), 
                                       mean(Chocolate), 
                                       mean(Cocoa)))
    by_clust[,2:6] = t(apply(by_clust[,2:6],1,log))
    names(by_clust) = c("by_clust","Magitude(Of Coffee)",
                        "Magitude(Of Tea)",
                        "Magitude(Of Spices)",
                        "Magitude(Of Chocolate)",
                        "Magitude(Of Cocoa)") 
    table2<-cbind(data.frame(Cluster = 1:k),data.frame(Size = cls_result$size),by_clust[,2:6])
    table2<-round(table2,1)
    #create row names for "table2"
    name_table2<-c()
    for(i in 1:nrow(table2)){
      name_table2<-c(name_table2,paste("cluster means",i,sep = " "))
    }
    rownames(table2)=name_table2
    table2
  })
}
