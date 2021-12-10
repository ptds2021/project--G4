library(shiny)
library(bslib)
library("shinyTime")
source("helpers.R")
source("R functions.R")

navbarPage(
  title = "SPC app",
  theme = bs_theme(
    bg = "white",
    fg = "#6e5104",
    primary = "#6e5104",
    base_font = font_google("Montserrat")
  ),
  
  
  tabPanel(
    title = "Home",
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
    
    tags$blockquote("Our SPC app is still under continuous development. "),
    hr()
  ),
  
  # fill content for tab 2
  tabPanel(
    title = "Import weight data",
    
    fluidPage(
      tags$head(tags$style(
        HTML(".shiny-text-output {background-color:#6e5104;}")
      )),
      h1(
        "Weight Data",
        span("Import", style = "font-weight: 300"),
        style =
          "font-family: 'Montserrat';
                         color: #6e5104;
                         text-align: center;
                         padding: 20px"
      ),
      br(),
      
      fluidRow(
        column(
        6,
        offset = 3,
        p(
          "This page allows you to enter your measurements on the Prélèvements",
          style = "font-family: 'Montserrat';"
        )
      ))
      ,
      
      br(),
      
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
      
      fluidRow(column(1,
                      wellPanel(
                        dateInput("Date", "Date", "", value = Sys.Date()),
                        hr(),
                      )),
               column(2 ,
                      wellPanel(
                        selectInput("Heure", "Heure:",
                                    c("8:00", "9:00", "10:00", "11:00", "12:00",
                                      "13:00","14:00","15:00","16:00", "17:00","18:00")),
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
               
               
                 
               
               
               ), 
      
      br(),
      
      fluidRow(
        column(1, wellPanel(
          numericInput("Prelevement", "Prelevement", ""),
          hr(),
        )),
        
        column(2, wellPanel(
          selectInput("Operator", "Operator", unique(df$Operator)),
          hr(),
          
        )),
        
        
    
        column(2, offset = 1, wellPanel(
          selectInput("Batch_pod_scellé", "Batch_pod_scellé", unique(df$Batch_pod_scellé)),
          hr(),
        )),
        
          column(2, wellPanel(
            selectInput("Batch_pod_bottom", "Batch_pod_bottom", unique(df$Batch_pod_bottom)),
            hr(),
            
          
          
        )),
      
        column(1, wellPanel(
          numericInput("Cible", "Cible", ""),
          hr(),
        )),
        
        
        
        column(1, wellPanel(
          numericInput("Tare", "Tare", ""),
          hr(),
        )),
        
        column(1, offset=1, wellPanel(
          textInput("Request", "Request", ""),
          hr(),
        )),
      ),
      
      br(),
      fluidRow(
        column(2, wellPanel(
          actionButton("submit", "Submit", align = "center"),
          hr()
        ))
      ),
      br()
    ),
      
      fluidRow(
        column(5, wellPanel(
          plotOutput("plot_sample"),
          hr()
        )),
        column(5, wellPanel(
          tableOutput("table_summary"),
          hr()
        ))
      ),
      br(),
        
      
 ),
      
      
      

  tabPanel(title = "Graph",
           fluidPage(
             selectInput("Request", "Request", unique(data_all$Request)),
             mainPanel(plotOutput("plot_hist", height = "400px"),
             mainPanel(DT::dataTableOutput("summ_req")
           ))
)

)


)


