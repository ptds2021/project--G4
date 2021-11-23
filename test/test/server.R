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
            ggplot2::geom_point(ggplot2::aes(y = input$poids1, x = 1)) +
            ggplot2::geom_point(ggplot2::aes(y = input$poids2, x = 2)) +
            ggplot2::geom_point(ggplot2::aes(y = input$poids3, x = 3)) +
            ggplot2::geom_point(ggplot2::aes(y = input$poids4, x = 4)) +
            ggplot2::geom_point(ggplot2::aes(y = input$poids5, x = 5)) +
            ggplot2::geom_point(ggplot2::aes(y = input$poids6, x = 6)) +
            ggplot2::geom_hline(ggplot2::aes(
                yintercept = (
                    input$poids1 + input$poids2 + input$poids3 + input$poids4 + input$poids5 + input$poids6
                )/6
            ),
            color = "blue",
            linetype = 3) +
            ggplot2::geom_hline(yintercept = (input$cible),
                                linetype = "dashed",
                                color = "red") +
            theme_qcc()
    })
    
    
#####Graph output
    
})