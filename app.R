
# load libraries
library(dplyr)
library(shiny)
library(ggplot2)

# load data set and transfer the date type into month and add a column named
# "month", then group the data set by month
hate_crime_Seattle <- read.csv("hate_crimes_Seattle.csv",
                               stringsAsFactors = FALSE)
format_date <- as.Date(hate_crime_Seattle$Incident.Date , "%m/%d/%y")
month <- sapply(format_date, substring, 6, 7)
hate_crime_Seattle$month <- month

# home_page: introduction about our project
home_page <- tabPanel(
  "Home",
  titlePanel("Hate Crimes in United States"),
  fluidPage(
    p("Hate crimes are an ongoing concern in the United States,
      as thousands are being targeted and attacked based on their race,
      gender, sexuality or religion. Individuals who are directly impacted
      by this fall under minority groups such as people of color, LGBTQ+,
      etc. With the climate in the United States, reasons for hate crimes
      occurring stem from one’s personal prejudice and hatred for another
      individual that are a member of these groups. In the United States,
      the number and types of hate crime are collected by the FBI to help
      inform law enforcement and the general population."),
    p("Hate crime statistics are being severely underreported therefore the
      FBI’s hate crime data significantly underestimates the extent of the
      problem. Crime statistics are important to see where more or fewer
      resources may be needed and to understand what is happening in our
      communities. Keeping track of specific types of crime, like
      bias-motivated crimes, is especially important to this understanding
      of not just our communities, but also our country as a whole."),
    p("Hate crime is characterized by violent actions that are
      bias-motivated including but not limited to one’s race, gender, sexual
      orientation, political belief, disability, or religion (FBI). There is
      strong evidence suggesting that many police departments do not report
      bias-motivated crimes to the FBI and therefore statistically, it
      creates the illusion that a small number of hate crimes had been
      committed. The impact of hate-crime on victims and the victim’s
      family is devastating. It disrupts their psychological well-being
      and creates distress, also sends a message that these people are
      unwelcome and unsafe in the community"),
    p("With our project, we hope to use our data visualizations and
      statistics we collected to highlight hate crimes in major cities,
      and also detailed information about hate crimes in Seattle."),
    p("This is a link to our technical report if you want to learn
      more about our project:"),
    tags$a(href ="https://github.com/HanzhangHu/operation_awareness-/wiki/Technical-Report",
             "Technical Report")
  )
)


# this page is the detailed information about hame crimes in Seattle, 2018
seattle_incidents_page <- tabPanel(
  "Hate Crimes in Seattle 2018",
  titlePanel("Hate Crimes in Seattle 2018"),
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
    # content in main panel with a bar_chart, which could visualize the
    # data about the type of hate crimes happened in each month(or in 2018)
    # and the text is a total number of incidents that happened in each
    # month(or in 2018)
    mainPanel(
      plotOutput("type_bar"),
      textOutput("incidents")
    )
  )
)

# about tech page
tech_page <- tabPanel(
  "About Tech",
  titlePanel("About Tech"),
  fluidPage(
    p("1. The data sets were loaded using read.csv function."),
    p("2. The following libraries were used for P2 ggplot2, shiny,
     leaflet and dplyr."),
    p("3. We have our app.R under the repo, and all the datasets
     in a file called 'data'."),
    p("4. hate_crimes_Seattle.csv file was used to look at the rate
      of different categories of bias-motivated crimes in different
      month in Seattle, 2018.")
  )
)

# page about us
us_page <- tabPanel(
  "About Us",
  titlePanel("About Us"),
  fluidPage(
    p("My name is Nyamsuren Delger and I am majoring in Psychology,
      and my email is delgernyamsuren@gmail.com."),
    p("My name is Hanzhang Hu and I'm also majoring in Psychology,
      and my email is hhz511@uw.edu")
  )
)

# creates the navigation bar, which includes all pages
ui <- navbarPage(
  "Hate Crimes in United States",
  home_page,
  # content in sidebar
  seattle_incidents_page,
  tech_page,
  us_page
)

# define server that renders texts and bar charts
server <- function(input, output) {
  # text content about the numbers of hate crimes
  output$incidents <- renderText({
      # count incidents for 2018 and report with text
      if(input$month == "Choose a month in 2018"){
        total_num <- nrow(hate_crime_Seattle)
        paste("In 2018,", total_num,
              "hate crimes happened in Seattle.")
      } else{
        # count each type of incidents for each month and report with text
        crimes <- group_by(hate_crime_Seattle, month) %>%
          summarize(crime_num = sum(Number.of.Records))
        abb_month <- sapply(input$month, substring, 1, 3)
        numerical <- match(abb_month,month.abb)
        paste("In", input$month, "2018,", crimes[numerical, 2],
              "hate crimes happened in Seattle.")
      }
  })
  
  # bar chart about each types of hate crimes
  output$type_bar <- renderPlot({
    # visualization: bar chart about the hate crime types for whole year
    if (input$month == "Choose a month in 2018"){
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
    # visualization: bar chart about the hate crime types for October,
    # November and December
    if(numerical == "10" | numerical == "11"|numerical == "12"){
      num_paste = numerical
    } else {
      # visualization: bar chart about the hate crime types from month 1
      # to month 9
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

# start running the application
shinyApp(ui = ui, server = server)
