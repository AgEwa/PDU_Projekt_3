library(shiny)

# Wczytuję plik realizujący zapytanie
source("helpers.R")

# User interface
ui <- fluidPage(
  titlePanel("Trip times statistics"),
  
  # rozpisany wygląd aplikacji
  
  sidebarLayout(
    sidebarPanel(
      helpText("Performs query about the average and median travel time for a day from a given date range and a selected city"),
      
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
    
    do.call(getThePlot, list(NYC,input$range[1], input$range[2]))
  })
}

# Run app ----
shinyApp(ui, server)