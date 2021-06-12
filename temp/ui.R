library(shiny)
library(dplyr)
library(ggplot2)
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  leafletOutput("mymap"),
  p(),
  actionButton("recalc", "New points"),

  
  # App title ----
  titlePanel("Pedestrian Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      selectInput("Distribution","Please Select Distribution Type",choices=c("Map","Line Chart")),
      ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)