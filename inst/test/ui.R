library(shiny)
library(bslib)
library(shinyjs)
library(pkgproject)
source("helpers.R")

navbarPage(
  title = "SPC app",
  theme = bs_theme(
    bg = "white",
    fg = "#6e5104",
    primary = "#5691cc",
    base_font = font_google("Montserrat"),
    bootswatch = "spacelab"
  ),

  # fill content for tab 2
  tabPanel(
    title = "Input",

    fluidPage(
      shinyjs::useShinyjs(),
      tags$head(tags$style(
        HTML(".shiny-text-output {background-color:#6e5104;}")
      )),
      br(),

      fluidRow(
        column(width = 2,
               wellPanel(
                 dateInput("Date", "Date", "", value = Sys.Date()),
                 hr(),
               )),
        column(width = 1,
               wellPanel(
                 numericInput("Measure1", "Measure 1", ""),
                 hr(),
               )),
        column(width = 1,
               wellPanel(
                 numericInput("Measure2", "Measure 2", ""),
                 hr(),
               )),
        column(width = 1,
               wellPanel(
                 numericInput("Measure3", "Measure 3", ""),
                 hr(),
               )),
        column(width = 1,
               wellPanel(
                 numericInput("Measure4", "Measure 4", ""),
                 hr(),
               )),
        column(width = 1,
               wellPanel(
                 numericInput("Measure5", "Measure 5", ""),
                 hr(),
               )),
        column(width = 1,
               wellPanel(
                 numericInput("Measure6", "Measure 6", ""),
                 hr(),
               )),

        column(width = 1, wellPanel(
          numericInput("Request", "Request", ""),
          hr(),
        )),
        br(),
        column(width = 2, wellPanel(
          selectInput(
            "Operator",
            "Operator",
            c("Samuel",
              "Iegor",
              "Sophie",
              "Ozgür",
              "Valérie",
              "Marc-Olivier")
          ),
          hr(),

        )),

        column(width = 2, wellPanel(
          numericInput("Process.Sample", "Process.Sample", ""),
          hr(),
        )),

        column(width = 2, wellPanel(
          selectInput("Product.Size", "Size", c("S", "M", "L", "XL")),
          hr(),
        )),

        column(width = 2, wellPanel(
          numericInput("Target.Value", "Target.Value", ""),
          hr(),
        )),

        column(width = 2, wellPanel(numericInput("Tare", "Tare", ""),
                                    hr(), )),

        column(
          width = 3,
          offset = 0,
          wellPanel(actionButton("submit", "Submit", align = "center"),
                    hr())
        ),

      ),
      br(),

      shinyjs::hidden(span(id = "submit_msg", "Submitting..."),
                      div(id = "error",
                          div(
                            br(), tags$b("Error: "), span(id = "error_msg")
                          ))),
      shinyjs::hidden(div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )),

      fluidRow(column(
        12,
        wellPanel(
          h3("Weight Data Table"),
          DT::dataTableOutput("responses"),
          hr(),
        )
      )),
      br(),

      fluidRow(column(12,
                      wellPanel(
                        plotOutput("plot_sample"),
                        tableOutput("table"),
                        hr(),
                      ))),
      br(),
    )
  ),


  ##################

  tabPanel(title = "Density Plot",
           fluidPage(
             fluidRow(column(
               12,
               selectInput("req", "Request", unique(nasty$Request)),
               wellPanel(
                 plotOutput("plot_hist", height = "400px"),
                 wellPanel(DT::dataTableOutput("summ_req"))
               )
             )),
             br(),

             fluidRow(column(12,
                             wellPanel(
                               plotOutput("R_bar", height = "400px")
                             ))),
             br()

           )),
  ###############################################################################

  tabPanel(title = "Time Series for operators",
           fluidPage(fluidRow(
             column(
               12,
               uiOutput("choices"),
               uiOutput("select_prelev"),
               plotOutput("TS_g", height = "400px")
             ),
             br()
           ))),

  ####### Home tab ###
  tabPanel(
    title = "App Info",
    br(),
    hr(),
    h4(strong("Project Description")),
    p(
      style = "text-align: justify; font-size = 25px",
      "Our project would be an application on industrial production area.
More precisely the final and ambitious objective of the work is to
help companies with process control by creating a shiny application to
support the controlling mechanism of a specific carateristic.
The application would be used by different users: The operator,
that would use the app to control his work.
The project manager, to check the big picture of the project
in the R&D branch of the company."
    ),
    hr()
  )

)
