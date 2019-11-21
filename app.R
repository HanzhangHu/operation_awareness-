#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# load libraries
library(dplyr)
library(shiny)
library(ggplot2)

# prepare data
hate_crime_Seattle <- read.csv("hate_crimes_Seattle.csv", stringsAsFactors = FALSE)
format_date <- as.Date(hate_crime_Seattle$Incident.Date , "%m/%d/%y")
month <- sapply(format_date, substring, 6, 7)
hate_crime_Seattle$month <- month

# ui part
ui <- fluidPage(
  titlePanel("Hate Crimes in Seattle"),
  # content in sidebar   
  sidebarLayout(
    sidebarPanel(
      # choose month in 2018
      selectInput("month", "Choose a month in 2018:",
                 list("Choose a month in 2018", "January", "February",
                      "March", "April", "May", "June", 
                      "July", "August", "September",
                     "October", "November", "December"
                     )
      )
    ),
    # content in main panel
    mainPanel(
      plotOutput("type_bar"),
      textOutput("incidents")
    )
  )
)

# server part
server <- function(input, output) {
  # text content about the numbers of hate crimes
  output$incidents <- renderText({
      # text content for 2018
      if(input$month == "Choose a month in 2018"){
        total_num <- nrow(hate_crime_Seattle)
        paste("In 2018,", total_num,
              "hate crimes happened in Seattle.")
      } else{ 
        # text content for each month
        crimes <- group_by(hate_crime_Seattle, month) %>%
          summarize(crime_num = sum(Number.of.Records))
        abb_month <- sapply(input$month, substring, 1, 3)
        numerical <- match(abb_month,month.abb)
        paste("In", input$month, "2018,", crimes[numerical, 2],
              "hate crimes happened in Seattle.")
      }
  })

    # bar chart about the types of hate crimes
  output$type_bar <- renderPlot({
    # bar about the hate crime types for whole year
    if (input$month == "Choose a month in 2018"){
      by_type <- group_by(hate_crime_Seattle, BIAS.TYPE) %>%
        summarize(type_num = sum(Number.of.Records)) %>%  
        arrange(-type_num)

      ggplot(hate_crime_Seattle, aes(BIAS.TYPE, 1)) +
        geom_col() +
        theme_minimal() +
        labs(x = "Types of Hate Crimes", y = "Number") +
        theme(axis.text.x = element_text(size = 8, angle = 0,
                                             hjust = 0.5, vjust = 0.5)) +
        coord_flip()

        } else {
          # bar about the hate crime types for each month
          abb_month <- sapply(input$month, substring, 1, 3)
          numerical <- match(abb_month,month.abb)
          if(numerical == "10" | numerical == "11"|numerical == "12"){
            num_paste = numerical
          } else {
            num_paste <- paste0("0", numerical)
          }
          monthly_type <- group_by(hate_crime_Seattle, month) %>%
            filter(month == num_paste)
          ggplot(monthly_type, aes(BIAS.TYPE, 1)) +
            geom_col() +
            theme_minimal() +
            labs(x = "Types of Hate Crimes", y = "Number of Hate Crimes") +
            theme(axis.text.x = element_text(size = 8, angle = 0,
                                             hjust = 0.5, vjust = 0.5)) +
            coord_flip() 
      }
    })
    
}
shinyApp(ui = ui, server = server)
