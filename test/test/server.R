library(shiny)

shinyServer(function(input, output, session) {

#####Import Data output
    formData <- reactive({
        data <- sapply(weights, function(x) input[[x]])
        data
    })
    
    observeEvent(input$submit, {
        saveData(formData())
    })

    output$responses <- DT::renderDataTable({
        input$submit
        DT::datatable(loadData(),
                      options = list(pageLength = 5, scrollX = TRUE),
                      class = 'cell-border stripe', rownames = FALSE, width =500)
    })
    
    output$plot_sample <- renderPlot({
        ggplot() +
            geom_point(data = as.matrix(input$poids1), col = 'black') +
            geom_point(data = as.matrix(input$poids1), col = 'black')
    })
    
    
#####Graph output
    
})