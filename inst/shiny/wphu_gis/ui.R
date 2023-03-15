library(shiny)
library(dplyr)
library(h3jsr)
library(viridis)
library(sf)
library(rdeck)



ui <- bootstrapPage(
  tags$style(
    type = "text/css", "
      html, body {width:100%;height:100%}

      #selectors{
        background-color: #fff;
        opacity: 0.5;
        border-radius: 10px;
        padding: 10px 15px 10px 15px;
      }
      #selectors:hover{
        opacity: 1;
      }
    "
  ),
  rdeckOutput("map", height = "100%"),
  absolutePanel(
    id = "selectors",
    bottom = 10, left = 10,
    fileInput(
      "input_latlong", "Choose CSV File",
      multiple = FALSE,
      accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")
    ),
    tableOutput("files")
  )
  #   checkboxGroupInput(
  #     inputId = "geographies",
  #     label = "Geographies",
  #     choices = c("LGA", "Meshblock"),
  #     selected = c(TRUE, FALSE)
  #   )
  # )
)
