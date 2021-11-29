library(shiny)

shinyServer(function(input, output, session) {

#####Import Data output
    formData <- reactive({
        data <- sapply(weights, function(x)input[[x]])
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
        ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(y = input$Poids1, x = 1)) +
            ggplot2::geom_point(ggplot2::aes(y = input$Poids2, x = 2)) +
            ggplot2::geom_point(ggplot2::aes(y = input$Poids3, x = 3)) +
            ggplot2::geom_point(ggplot2::aes(y = input$Poids4, x = 4)) +
            ggplot2::geom_point(ggplot2::aes(y = input$Poids5, x = 5)) +
            ggplot2::geom_point(ggplot2::aes(y = input$Poids6, x = 6)) +
            ggplot2::geom_hline(ggplot2::aes(
                yintercept = (
                    input$Poids1 + input$Poids2 + input$Poids3 + input$Poids4 + input$Poids5 + input$Poids6
                )/6
            ),
            color = "blue",
            linetype = 3) +
            ggplot2::geom_hline(yintercept = (input$Cible),
                                linetype = "dashed",
                                color = "red") +
            theme_qcc() +
            labs(x = "Poids", y = "Weight (Gr)",
                 title = paste("Request",input$Request),
                 subtitle = paste("Prélèvement",input$Prelevement))
    })
    
    
#####Graph output
    
})