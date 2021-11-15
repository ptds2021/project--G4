library(shiny)
source("helpers.R")

shinyUI(fluidPage(
    tabsetPanel(
        tabPanel("Import data", 
                 DT::dataTableOutput("responses", width = 300), tags$hr(),
                 textInput("date", "Date", ""),
                 textInput("heure", "Heure", ""),
                 textInput("prelevement", "Prelevement", ""),
                 textInput("request", "Request", ""),
                 textInput("cible", "Cible", ""),
                 sliderInput("sample", "Sample Size",
                             0, 25, 6, ticks = FALSE),
                 actionButton("submit", "Submit")
        ),
        tabPanel("Graph"),
    )
))
