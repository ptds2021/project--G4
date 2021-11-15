library(shiny)
library(bslib)
source("helpers.R")


navbarPage(title = "SPC app",
           theme = bs_theme(bg = "white",
                            fg = "brown",
                            primary = "maroon",
                            base_font = font_google("Montserrat")
           ),
           
           
           tabPanel(title = "Home",br(),
                    hr(),
                    h4(strong("Project Description")),
                    p(style = "text-align: justify; font-size = 25px",
                      
        "Our project would be an application on industrial production area. 
More precisely the final and ambitious objective of the work is to help the Nestle coffee production
creating a shiny application to support the controlling mechanism of the coffee weight during the new capsules recipes formulations.
Since the coffee is an expensive row material, even a little variation in the quantities, taking account of the massive scale of the production,
has an impact on the cost of goods sold of the company; indeed, a control of the weight in that sense could help to optimize such industrial process. 
The application would be used by different users: The operator, that would use the app to control his work. 
The project manager, to check the big picture of the project in the R&D branch of the company."),
                    
            tags$blockquote("Our SPC app is still under continuous development. "),
                    hr()),
           
           # fill content for tab 2
           tabPanel(title = "Import data",
                    fluidPage(
                        tabPanel(
                            "Import data",
                            DT::dataTableOutput("responses", width = 300),
                            tags$hr(),
                            textInput("date", "Date", ""),
                            textInput("heure", "Heure", ""),
                            textInput("prelevement", "Prelevement", ""),
                            textInput("request", "Request", ""),
                            textInput("cible", "Cible", ""),
                            sliderInput("sample", "Sample Size",
                                        0, 25, 6, ticks = FALSE),
                            actionButton("submit", "Submit")
                        )
                    )),
           
           tabPanel(title = "Graph",
                    "content 3"),
           inverse = T
)