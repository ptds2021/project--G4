library("shiny")
library("bslib")
library("shinyTime")
#source("helpers.R")
#source("R functions.R")
library("shinyjs")
library(pkgproject)

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
                 numericInput("Poids1", "Poids 1", ""),
                        hr(),
                      )),
               column(width = 1,
                      wellPanel(
                        numericInput("Poids2", "Poids 2", ""),
                        hr(),
                      )),
               column(width = 1,
                      wellPanel(
                        numericInput("Poids3", "Poids 3", ""),
                        hr(),
                      )),
               column(width = 1,
                      wellPanel(
                        numericInput("Poids4", "Poids 4", ""),
                        hr(),
                      )),
               column(width = 1,
                      wellPanel(
                        numericInput("Poids5", "Poids 5", ""),
                        hr(),
                      )),
               column(width = 1,
                      wellPanel(
                        numericInput("Poids6", "Poids 6", ""),
                        hr(),
                      )),

               column(width = 1, wellPanel(
                 numericInput("Request", "Request", ""),
                 hr(),
               )),
    br(),


        column(2, wellPanel(
          selectInput("Operator", "Operator", c("Loïc", "Karen", "Chantal", "Christine", "Sandra", "Tiffany","Sarah", "Anthony", "Chahineze", "Danylo", "Thibaud","Pinar", "Sylvie",  "Dylan", "Sandrine", "Abdallah", "Eymeric", "Erika", "Daniela" )),
          hr(),

        )),




        column(1,offset = 1, wellPanel(
          numericInput("Prelevement", "Prelevement", ""),
          hr(),
        )),




          column(1, wellPanel(
            selectInput("Size", "Size", c("XS","S", "M", "L", "XL", "NA")),
            hr(),



        )),

        #column(2, wellPanel(
          #selectInput("Specification", "Specification", unique(poids$Spec)),
          #hr(), )),

        column(1, wellPanel(
          numericInput("Cible", "Cible", ""),
          hr(),
        )),



        column(1, wellPanel(
          numericInput("Tare", "Tare", ""),
          hr(),
        )),






        column(1, wellPanel(
          actionButton("submit", "Submit", align = "center"),
          hr()
        ))


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
    br()



 )),




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
              selectInput("psize", "Pod Size", choices = unique(nasty$Request),
                          selected = unique(nasty$Request)[1]),
              uiOutput("select_cible"),

              wellPanel(
                plotOutput("cible_hist", height = "400px")
                )
            )),
          br()

          )

),
###############################################################################
tabPanel(title = "Time Series for operators",
         fluidPage(
           fluidRow(
             column(
               12,
               selectInput("prequest", "Request", choices = unique(nasty$Request),
                           selected = unique(nasty$Request)[1]),
               uiOutput("select_prelev"),

               wellPanel(
                 plotOutput("TS_g", height = "400px"),
                 wellPanel(DT::dataTableOutput("summ_ts"))
               )
             )),
           br()

         )

),


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
