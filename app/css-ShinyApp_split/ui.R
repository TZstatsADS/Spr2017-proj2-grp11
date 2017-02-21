
library(shiny)

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
                           value = 1996, min =1996, max =2016),
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
             tabPanel("Mirror Histogram", sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "commodity_hist",
                             label  = "choose the commodity",
                             choices = c('Annual Aggregate','Chocolate', 'Coffee','Cocoa','Spices','Tea'),
                             selected ='Chocolate'),
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
             ### end Mirror Histogram
             
             ### Regional statistics
             tabPanel("Regional statistics",sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "Regional_commodity",
                             label  = "choose the commodity",
                             choices = unique(input_data$Commodity_Name),
                             selected ='Spices'),
                 sliderInput(inputId = "Regional_year",
                             label = "Select a year",
                             value = 2016, min =1996, max =2016)
               ),
               
               mainPanel(
                 plotOutput("regional_import")
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
  

