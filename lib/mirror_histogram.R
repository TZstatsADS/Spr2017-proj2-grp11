##ui part
#to be inserted right after Terry's exchange rate
tabPanel("Mirror Histogram", sidebarLayout(
  sidebarPanel(
    selectInput(inputId = "commodity_hist",
                label  = "choose the commodity",
                choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                selected ='Spices'),
    selectInput(inputId = "country_hist",
                label  = "choose the country",
                choices = unique(input_data$Country),
                selected ='China')
  ),
  
  mainPanel(
    plotOutput("Hist")
  )
)

),

##Server part
output$Hist <- renderPlot({
##Subset
tp = input_data
tp = subset(tp,Country == as.character(input$country_hist))
tp = subset(tp,Commodity_Name == as.character(input$commodity_hist))
tp = na.omit(tp)
tp = tp[order(tp$type,decreasing = T),]#put import first

##Data frame for ggplot2
dat <- data.frame(
  group = tp$type,
  Year = tp$Year,
  Value = ifelse(tp$type == "Import",tp$value,-tp$value)#import on upside, export on downside
  )

##plotting
ggplot(dat, aes(x=Year, y=Value, fill=group))+
  geom_bar(stat="identity", position="identity")+
  scale_fill_manual(values=c("#87CEFA","#DC143C"))
})