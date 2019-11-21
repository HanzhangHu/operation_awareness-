library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
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
)

#------------------------------------------------------------------------------------
library(dplyr)

# Data load
hate_crime_total_by_states <- read.csv("hate_crimes.csv", stringsAsFactors = FALSE)

server <- 
  function(input, output) {
    output$hatecrimePlot <- renderPlot( {
      
      ggplot(hate_crime_total_by_states, aes(x = share_voters_voted_trump, 
                                             y = avg_hatecrimes_per_100k_fbi)) +
        geom_point ()
      
    })
  }





# Run the application 
shinyApp(ui = ui, server = server)