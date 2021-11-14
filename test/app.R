fields <- c("date", "heure", "prelevement", "request", "cible", "sample")

outputDir <- "responses"

saveData <- function(data) {
    data <- t(data)
    # Create a unique file name
    fileName <- sprintf("%s_%s.csv", as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
        x = data,
        file = file.path(outputDir, fileName), 
        row.names = FALSE, quote = TRUE
    )
}

loadData <- function() {
    # Read all the files into a list
    files <- list.files(outputDir, full.names = TRUE)
    data <- lapply(files, read.csv, stringsAsFactors = FALSE) 
    # Concatenate all data together into one data.frame
    data <- do.call(rbind, data)
    data
}

shinyApp(
    ui = fluidPage(
        tabsetPanel(
            tabPanel("Import data", 
                     DT::dataTableOutput("responses", width = 300), tags$hr(),
                     textInput("date", "Date", ""),
                     textInput("heure", "Heure", ""),
                     textInput("prelevement", "Prelevement", ""),
                     textInput("request", "Request", ""),
                     textInput("cible", "Cible", ""),
                     sliderInput("sample", "Sample Size",
                                 0, 25, 2, ticks = FALSE),
                     actionButton("submit", "Submit")
            ),
            tabPanel("Graph"),
        )
    ),
    server = function(input, output, session) {
        
        # Whenever a field is filled, aggregate all form data
        formData <- reactive({
            data <- sapply(fields, function(x) input[[x]])
            data
        })
        
        # When the Submit button is clicked, save the form data
        observeEvent(input$submit, {
            saveData(formData())
        })
        
        # Show the previous responses
        # (update with current response when Submit is clicked)
        output$responses <- DT::renderDataTable({
            input$submit
            loadData()
        })     
    }
)
