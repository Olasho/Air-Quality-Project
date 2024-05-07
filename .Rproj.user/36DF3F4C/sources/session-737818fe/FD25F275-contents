library(shiny)
library(shinyWidgets)
library(DT)

street_all_schools_2
colnames(street_all_schools_2)

Pollutants <- c("NO", "NO2", "O3", "PM1", "PM10", "PM25")

shinyUI(fluidPage(
  
  titlePanel("Low cost sensors Pollutant measurement (zephyr)"),

  sidebarLayout(
    sidebarPanel(
      selectInput("location", "location of sensor", 
                  choices = unique(street_all_schools_2$`School street`),
                  multiple = TRUE),
      selectInput("pollutant", "Pollutant",
                  choices = Pollutants,
                  multiple = TRUE)
    ),
    
    mainPanel(tableOutput("Pollutant_Concentration")
              )
    )
  )
  )


