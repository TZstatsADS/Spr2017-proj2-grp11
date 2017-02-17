
library(shiny)

ui<- navbarPage(
  
  theme = "bootstrap.css",
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
  

