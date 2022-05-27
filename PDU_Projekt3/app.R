# Load packages ----
library(shiny)


# Load data ----
#counties <- readRDS("data/counties.rds")

# Source helper functions -----
source("helpers.R")

# User interface ----
ui <- fluidPage(
  titlePanel("Zapytanie"),
  
  # rozpisany wygląd aplikacji
  
  sidebarLayout(
    sidebarPanel(
      helpText("Realizuje zapytanie, dni: 0- 1 maja, 91 - 31 lipca"),
      
      selectInput("var", 
                  label = "Choose a city to display",
                  choices = c("New York", "New Jersy"),
                  selected = "New Jesrsy"),
      
      dateRangeInput("range",
                     label = "Date range:",
                     start  = "2016-05-13",
                     end    = "2016-06-05",
                     min    = "2016-05-01",
                     max    = "2016-07-31",
                     format = "mm/dd/yy",
                     separator = " - "),
      
      
      #sliderInput("range", 
       #           label = "Range of interest of days:",
        #          min = 0, max = 91, value = c(20,40))
    ),
    
    mainPanel(plotOutput("map"))
  )
)

server <- function(input, output) {
  # rozpisane działanie aplikacji, wywołuje skrypt z zapytaniem i wykresem
  output$map <- renderPlot({
    NYC <- switch(input$var,
                   "New York" = T,
                   "New Jersy" = F)
    
    #args$min <- input$datarange3[1]
    #args$max <- input$datarange3[2]
    
    do.call(getThePlot, list(NYC,input$range[1], input$range[2]))
  })
}

# Run app ----
shinyApp(ui, server)