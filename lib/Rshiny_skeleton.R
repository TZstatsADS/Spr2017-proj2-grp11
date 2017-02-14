" in the fluidpage function: the sidepanel could be used to take in user input, the input value
can be accessed by input$inputId the mainPanel is the variable name of the visualization. 
in the server funtion generate the output to output$variable_name_for_visualization.
if the visualization is interactive and varies by user input, then make sure to use a render function
I'm not sure of the specific needs, so this is the very basic skeleton. and for the stat summary peeps,
i built a little more complicated skeleton, have some bugs, trying to finish by tomorrow"
EnsurePackage<-function(x)
{
  x<-as.character(x)
  if (!require(x,character.only=TRUE))   # character.only tells require that x should have only character string
  {
    install.packages(pkgs=x,dependencies = TRUE)
    require(x,character.only=TRUE)
  }
}
EnsurePackage("shiny")


ui<- fluidPage(
  titlePanel("US commodity imports from across the globe"),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_3D",
                  label = "choose a year",
                  value = 1996, min =1996, max =2016),
      selectInput(inputId = "commodity_3D",
                  label  = "choose the commodity",
                  choices = c('Coffee','Cocoa','Spices'),
                  selected ='Coffee')
    ),
  mainPanel(
    plotOutput("Globe")
  )
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(inputId = "year_linear_graph",
              label = "choose a year",
              value = 1996, min =1996, max =2016)
    ),
    mainPanel (
      plotOutput("linear")
    )
  )
  
)
  

  
server<- function(input, output){
  output$Globe <-renderPlot({
    title <- "Global commodity imports"
    hist(rnorm(input$year_3D), main = title) # put 3d globe here with input$year input$commodity as inputs
  })
}

shinyApp(ui = ui, server= server)
