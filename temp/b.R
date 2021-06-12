
######### A call to libraries 
library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)
library(maps) 

# checking the current working directory
getwd()

# Setting the directory location
setwd("G:\3rd Sem\FIT5147DE\R")

# Saving the file to a Vector
location_data <- read.csv("Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv")
head(location_data)

# Checking for unique values 
unique(location_data$Sensor_Name)

# Saving the file to a Vector
hourly_data <- read.csv("Pedestrian_Counting_System_2019 (Exercise 2).csv")
head(hourly_data)

# Checking for unique values
unique(hourly_data$Sensor_Name)

# Changing the Column name as both the files have same column but different spellings
names(location_data)[1] <- "Sensor_Name"

# Some rows have same location coordinates with a little difference in names, so changing them to one common name.
hourly_data$Sensor_Name[hourly_data$Sensor_Name == "Flinders la - Swanston St (West) Temp"] <- "Flinders La - Swanston St (West) Temp"

location_data$Sensor_Name[location_data$Sensor_Name == "Melbourne Central-Elizabeth St (East)Melbourne Central-Elizabeth St (East)"] <- "Melbourne Central-Elizabeth St (East)"

location_data$Sensor_Name[location_data$Sensor_Name == "Flinders La-Swanston St (West)"] <- "Flinders La - Swanston St (West) Temporary"

hourly_data$Sensor_Name[hourly_data$Sensor_Name == "Pelham St (S)"] <- "Pelham St (South)"

hourly_data$Sensor_Name[hourly_data$Sensor_Name == "Lincoln-Swanston(West)"] <- "Lincoln-Swanston (West)"

location_data$Sensor_Name[location_data$Sensor_Name == "Building 80 RMIT"] <- "Swanston St - RMIT Building 80"

location_data$Sensor_Name[location_data$Sensor_Name == "State Library - New"] <- "State Library"
#
location_data$Sensor_Name[location_data$Sensor_Name == "Flinders La - Swanston St (West) Temporary"] <- "Flinders La - Swanston St (West) Temp"

location_data$Sensor_Name[location_data$Sensor_Name == "RMIT Building 14"] <- "Swanston St - RMIT Building 14"

# Merging both the vectors to one on sensor name.
temp <- merge(location_data, hourly_data, by = c('Sensor_Name'))

# Making a new variable named avg_hourly which has average of hourly counts 
final <- temp %>%
  group_by(Sensor_Name, latitude, longitude) %>%
  summarise(avg_hourly = mean(Hourly_Counts))

hourly <- temp  %>%
  group_by(Sensor_Name,Day,Time,latitude,longitude) %>%
  summarise(avg_hourly = mean(Hourly_Counts))

now <- (unique(temp$Sensor_Name))


##########################################Code for Making the Shiny App#####################################

# UI 
ui <- fluidPage(
  titlePanel("Pedestrian Sensor hourly"),
  #leafletOutput("map"),
  #plotOutput("line"),
  sidebarLayout(selectInput("Temp", 
                            label = "Sensor Names",
                            choices = now,
                            selected = now[0]),
                mainPanel(
                  leafletOutput("map"),
                  plotOutput("line"))
)
)

#Server
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet(data = final) %>% 
      addTiles() %>%
      addCircles(~longitude, ~latitude, radius = ~sqrt(avg_hourly)*4, label = ~Sensor_Name, color = "Green")
  })
  
  #ggplot_data <- reactive({
  #  n <- input$map_marker_click$id
  #  new_dataset[new_dataset$Sensor_Name %in% n,]
  #})
  
  output$line = renderPlot({
    ggplot(data = hourly, aes(x = Time, y = avg_hourly, colour = Sensor_Name)) + 
      geom_line() + facet_wrap(~Day)
  })
}

# Shiny App
shinyApp(ui, server)