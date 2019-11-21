library(shiny)

hate_crime_total_by_states <- read.csv("hate_crimes.csv", stringsAsFactors = FALSE)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel(title = h4("Hate Crime in the US", align = "center"),
             sidebarLayout(
               sidebarPanel(
                 selectInput("var", "1. Select the variable from the hate_crimes dataset" ,
                             choices = c("share_voters_voted_trump" = 1, 
                                         "median_household_income" = 2, 
                                         "share_population_with_high_school_degree" = 3)),
                 br(),
                 radioButtons("color", "2. Select the color of the plots", choices = c("Black",
                                                                                       "Red",
                                                                                       "Green"),
                              selected = "Red")
               ),
               mainPanel(
                 plotOutput("hatecrimePlot")
               )
             ))
))

#------------------------------------------------------------------------------------
library(dplyr)

# Data load
hate_crime_total_by_states <- read.csv("hate_crimes.csv", stringsAsFactors = FALSE)

shinyServer(
  function(input, output) {
    output$hatecrimePlot <- renderPlot( {
      
      ggplot(hate_crime_total_by_states, aes(x = share_voters_voted_trump, 
                                             y = avg_hatecrimes_per_100k_fbi)) +
        geom_point ()
      
    })
  }
)


# I am keeping the following code just in case if we add and change our viz.
#server <- function(input, output){
# output$hate_crime_map <- renderLeaflet({
#  palette_fn <- colorNumeric(
#   palette = "YlGnBu",
#  domain = hate_crime_total_by_states[[input$analysis_var]]
#)
#leaflet(data = hate_crime_total_by_states) %>%
# addProviderTiles("Stamen.TonerLite") %>%
#addPolygons(
# stroke = FALSE,
#color = ~palette_fn(hate_crime_total_by_states[[input$analysis_var]]),
#fillOpacity = 0.7,

#)
#  addLegend(
#   "bottomright",
#  title = "ave hate crime in 100K",
# pal = palette_fn,
#values = hate_crime_total_by_states[[input$analysis_var]],
#opacity = 1
#)
#})

#}


# Run the application 
shinyApp(ui = shinyUI, server = shinyServer)

