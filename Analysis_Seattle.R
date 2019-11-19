# Set up
setwd("/Users/Nyamka/Documents/UW/Info201/Project")
library("dplyr")
library("tidyr")

#-----------------Bias-motivated Crime Report Seattle, 2018----------------
hate_crime_Seattle <- read.csv("Crime_Cat_Table_data.csv", stringsAsFactors = FALSE)
View(hate_crime_Seattle)

#--------------------------------Summary-----------------------------------
dim(hate_crime_Seattle)

generate_crime_by_type <- function(df, city, year) {
  df %>% 
    filter(...) %>% 
    ggplot() +
    geom_bar(...)
}

generate_race_crime_map <- function(df, city) {
  ggplot(df) +
    geom_point()
}

#Which bias type had the highest 