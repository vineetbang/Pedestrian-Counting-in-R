library(shiny)
library(dplyr)
library(ggplot2)

library(shiny)
library(leaflet)
library(tidyverse)
library(dplyr)
library(maps) 


server <- function(input, output,session) 
{
  #hourly_data <- read.csv("Pedestrian_Counting_System_2019 (Exercise 2).csv")
  
  location_data <- read.csv("Pedestrian_Counting_System_-_Sensor_Locations (Exercise 2).csv")
  head(location_data) 
  names(location_data)[1] <- "Sensor_Name"
  
  unique(location_data$Sensor_Name)
  
  hourly_data <- read.csv("Pedestrian_Counting_System_2019 (Exercise 2).csv")
  head(hourly_data)
  
  unique(hourly_data$Sensor_Name)
  
  hourly_data$Sensor_Name[hourly_data$Sensor_Name == "Flinders la - Swanston St (West) Temp"] <- "Flinders La - Swanston St (West) Temp"
  
  location_data$Sensor_Name[location_data$Sensor_Name == "Melbourne Central-Elizabeth St (East)Melbourne Central-Elizabeth St (East)"] <- "Melbourne Central-Elizabeth St (East)"
  
  location_data$Sensor_Name[location_data$Sensor_Name == "Flinders La-Swanston St (West)"] <- "Flinders La - Swanston St (West) Temporary"
  
  hourly_data$Sensor_Name[hourly_data$Sensor_Name == "Pelham St (S)"] <- "Pelham St (South)"
  
  hourly_data$Sensor_Name[hourly_data$Sensor_Name == "Lincoln-Swanston(West)"] <- "Lincoln-Swanston (West)"
  
  location_data$Sensor_Name[location_data$Sensor_Name == "Building 80 RMIT"] <- "Swanston St - RMIT Building 80"
  
  location_data$Sensor_Name[location_data$Sensor_Name == "State Library - New"] <- "State Library"
  
  location_data$Sensor_Name[location_data$Sensor_Name == "Flinders La - Swanston St (West) Temporary"] <- "Flinders La - Swanston St (West) Temp"
  
  location_data$Sensor_Name[location_data$Sensor_Name == "RMIT Building 14"] <- "Swanston St - RMIT Building 14"
  
  temp <- merge(location_data, hourly_data, by = c('Sensor_Name'))
  
  final <- temp %>%
    group_by(Sensor_Name, latitude, longitude) %>%
    summarise(avg_hourly = mean(Hourly_Counts))
  
  output$distPlot <- renderPlot({
    disType<-input$Distribution
    discoral<-input$CoralType
    
    if(disType=="Map")
    {
      points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
      }, ignoreNULL = FALSE)
      
      output$mymap <- renderLeaflet({
        leaflet(data = final) %>%
          addTiles() %>%
          addCircles(~longitude, ~latitude, radius = ~sqrt(avg_hourly)*3,popup = ~as.character(Sensor_Name)) 
      %>%
        addMarkers(data = points())
    })
    }
    
    else
    {
        ggplot(data=hourly_data, aes(x=Time, y = mean(Hourly_Counts), colour = Sensor_Name, group = Sensor_Name)) + 
        geom_line() + facet_wrap(~Day)
    }
    
  })
}