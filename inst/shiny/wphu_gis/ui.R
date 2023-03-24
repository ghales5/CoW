library(shiny)
library(dplyr)
library(h3jsr)
library(viridis)
library(sf)
library(rdeck)
library(colourpicker)



ui <- fluidPage(
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
  fluidRow(
    style = "height:100vh",
    column(
      3,
      h2("Data"),
      uiOutput(
        "fileInputs"
      ),
      actionButton("dataDecrease", label = NULL, icon = icon("minus", lib = "glyphicon")),
      actionButton("dataIncrease", label = NULL, icon = icon("plus", lib = "glyphicon")),
      tableOutput("files")
    ),
    column(
      9,
      rdeckOutput("map", height = "100vh")
    )
  ),

  #   checkboxGroupInput(
  #     inputId = "geographies",
  #     label = "Geographies",
  #     choices = c("LGA", "Meshblock"),
  #     selected = c(TRUE, FALSE)
  #   )
  # )
)
