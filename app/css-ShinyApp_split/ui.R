
library(shiny)

ui<- navbarPage(
  
  ##link to css.file
  theme = "bootstrap.css",
  "World Trade with US",
  ##Project Title
  tabPanel("Home",
           htmlOutput("blankspace"),
           titlePanel("FINDING TRACE"),
           h4(htmlOutput("text")),
           htmlOutput("teammates")
           ),
  ## 3D Globe tab
  tabPanel("3D Globe",
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, 
                         top = 150, left = 20, right = "auto", bottom = "auto",
                         width = 300, height = "auto",
                         
                         h3("3D Explorer"),
                         
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
                                     selected ='Coffee')),
           
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, 
                         top = 200, left = "auto", right = 20, bottom = "auto",
                         width = 330, height = "auto",
                         
                         plotOutput("ggplot",width="100%",height="200px")
           ),
           globeOutput("Globe",width="100%",height="600px")),
  ## end 3D Globe tab
  
  ## 2D Map tab
  tabPanel("2D Map",
           leafletOutput("mymap",width = "100%", height = 600),
           
           absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                         draggable = TRUE, 
                         top = 150, left = 20, right = "auto", bottom = "auto",
                         width = 300, height = "auto",
                         
                         h3("2D Explorer"),
                         
                         radioButtons(inputId = "type_2D",
                                      label  = "Choose import/export",
                                      choices = c('Export','Import'),
                                      selected ='Import'),
                         sliderInput(inputId = "year_2D",
                                     label = "Select a year",
                                     value = 2016, min =1996, max =2016),
                         sliderInput(inputId = "num_countries",
                                     label = "Top Countries in Trade",
                                     value = 10,min = 1,max = 50),
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
                                     width = 3,
                                     plotOutput("continent_import", height = "330px")
                                   ),
                                   
                                   mainPanel(
                                     plotlyOutput("regional_import",height = "420px")
                                   )
                                   
                                 )
                        ),
                        
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
                                 
                        ),
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
                                 
                                 
                        )
                        ### end Mirror Histogram
                        
                        
                      )
             )
             ## end Summary Statistics tab
             
  )
)
