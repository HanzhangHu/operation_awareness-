# 
library(shiny)
library(leaflet)
library(dplyr)


hate_crime_total_by_states <- read.csv("hate-crimes.csv", stringsAsFactors = FALSE)

server <- function(input, output){
  output$hate_crime_map <- renderLeaflet({
    palette_fn <- colorNumeric(
      palette = "YlGnBu",
      domain = hate_crime_total_by_states[[input$analysis_var]]
    )
    leaflet(data = hate_crime_total_by_states) %>%
      addProviderTiles("Stamen.TonerLite") %>%
      addPolygons(
        stroke = FALSE,
        color = ~palette_fn(hate_crime_total_by_states[[input$analysis_var]]),
        fillOpacity = 0.7,
        
      )
    addLegend(
      "bottomright",
      title = "ave hate crime in 100K",
      pal = palette_fn,
      values = hate_crime_total_by_states[[input$analysis_var]],
      opacity = 1
    )
  })
  
}