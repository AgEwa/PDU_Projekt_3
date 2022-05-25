# Load packages ----
library(shiny)


# Load data ----
#counties <- readRDS("data/counties.rds")

# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Zapytanie"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Realizuje zapytanie"),
      
      selectInput("var", 
                  label = "Choose a city to display",
                  choices = c("New York", "New Jersy"),
                  selected = "New York"),
      
      sliderInput("range", 
                  label = "Range of interest of years of birth:",
                  min = 1899, max = 1998, value = c(1950, 1980))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

server <- function(input, output) {
  output$map <- renderPlot({
    NYC <- switch(input$var,
                   "New York" = T,
                   "New Jersy" = F)
    
    #args$min <- input$range[1]
    #args$max <- input$range[2]
    
    do.call(getThePlot, list(NYC,input$range[1], input$range[2]))
  })
}

# Run app ----
shinyApp(ui, server)