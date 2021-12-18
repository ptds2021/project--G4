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

    fluidPage(shinyjs::useShinyjs(),
      tags$head(tags$style(
        HTML(".shiny-text-output {background-color:#6e5104;}")
      )),
      br(),

      fluidRow(column(2,
                      wellPanel(
                        dateInput("Date", "Date", "", value = Sys.Date()),
                        hr(),
                      )),


               column(width = 1, offset = 1,
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


        column(2, wellPanel(
          selectInput("Operator", "Operator", c("Samuel", "Iegor", "Sophie", "Ozgür", "Valérie", "Marc-Olivier")),
          hr(),

        )),




        column(2, wellPanel(
          numericInput("Process.Sample", "Process.Sample", ""),
          hr(),
        )),




          column(2, wellPanel(
            selectInput("Product.Size", "Size", c("S", "M", "L", "XL")),
            hr(),



        )),

        column(2, wellPanel(
          numericInput("Target.Value", "Target.Value", ""),
          hr(),
        )),



        column(2, wellPanel(
          numericInput("Tare", "Tare", ""),
          hr(),
        )),






        column(width = 2, offset = 2, wellPanel(
          actionButton("submit", "Submit", align = "center"),
          hr()
        )),


      ),
    br(),

    shinyjs::hidden(
      span(id = "submit_msg", "Submitting..."),
      div(id = "error",
          div(br(), tags$b("Error: "), span(id = "error_msg"))
      )),
    shinyjs::hidden(
      div(
        id = "thankyou_msg",
        h3("Thanks, your response was submitted successfully!"),
        actionLink("submit_another", "Submit another response")
      )
    ),

    fluidRow(
      column(
        12,
        wellPanel(
          h3("Weight Data Table"),
          DT::dataTableOutput("responses"),
          hr(),
        )
      )),
    br(),

    fluidRow(
      column(
        12,
        wellPanel(
          plotOutput("plot_sample"),
          hr(),
        )
      )),
    br(),
    fluidRow(
      column(5, wellPanel(tableOutput("table"),
        hr()
      ), offset = 6)
    )

 )),


##################

 tabPanel(title = "Density Plot",
          fluidPage(
            fluidRow(
            column(
            12,
            selectInput("req", "Request", unique(nasty$Request)),
            wellPanel(
              plotOutput("plot_hist", height = "400px"),
              wellPanel(DT::dataTableOutput("summ_req"))
            )
          )),
          br(),

          fluidRow(
            column(
              12,
              wellPanel(
                plotOutput("R_bar", height = "400px")
                )
            )),
          br()

          )

),
###############################################################################

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
help the Nestle coffee production creating a shiny application to
support the controlling mechanism of the coffee weight during the
new capsules recipes formulations.
Since the coffee is an expensive row material, even a little variation
in the quantities, taking account of the massive scale of the production,
has an impact on the cost of goods sold of the company; indeed,a control of
the weight in that sense could help to optimize such industrial process.
The application would be used by different users: The operator,
that would use the app to control his work.
The project manager, to check the big picture of the project
in the R&D branch of the company."),
  hr()
)

)
