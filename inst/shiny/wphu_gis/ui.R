library(shiny)
library(dplyr)
library(h3jsr)
library(viridis)
library(sf)
library(rdeck)
library(colourpicker)



ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  fluidRow(
    column(
      12,
      h1("Centre of WPHU"),
      class = "header"
    )
  ),
  fluidRow(
    style = "height:90",
    column(
      3,
      h3("Data"),
      uiOutput(
        "fileInputs"
      ),
      actionButton("dataDecrease", label = NULL, icon = icon("minus", lib = "glyphicon")),
      actionButton("dataIncrease", label = NULL, icon = icon("plus", lib = "glyphicon")),
      downloadButton(
        outputId = "mapdownload"
      ),
      class = "data-col"
    ),
    column(
      9,
      rdeckOutput("map", height = "100vh")
    )
  )
)
