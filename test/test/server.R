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
            ggplot2::geom_point(ggplot2::aes(y = (input$Poids1-input$Tare), x = 1)) +
            ggplot2::geom_point(ggplot2::aes(y = (input$Poids2-input$Tare), x = 2)) +
            ggplot2::geom_point(ggplot2::aes(y = (input$Poids3-input$Tare), x = 3)) +
            ggplot2::geom_point(ggplot2::aes(y = (input$Poids4-input$Tare), x = 4)) +
            ggplot2::geom_point(ggplot2::aes(y = (input$Poids5-input$Tare), x = 5)) +
            ggplot2::geom_point(ggplot2::aes(y = (input$Poids6-input$Tare), x = 6)) +
            ggplot2::geom_hline(ggplot2::aes(
                yintercept = (
                    (input$Poids1-input$Tare + input$Poids2-input$Tare + input$Poids3-input$Tare + input$Poids4-input$Tare + input$Poids5-input$Tare + input$Poids6-input$Tare)
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
    output$plot <- renderPlot({
        ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(y = (data_all$Poids1-data_all$Tare), x = 1)) +
            ggplot2::geom_point(ggplot2::aes(y = (data_all$Poids2-data_all$Tare), x = 2)) +
            ggplot2::geom_point(ggplot2::aes(y = (data_all$Poids3-data_all$Tare), x = 3)) +
            ggplot2::geom_point(ggplot2::aes(y = (data_all$Poids4-data_all$Tare), x = 4)) +
            ggplot2::geom_point(ggplot2::aes(y = (data_all$Poids5-data_all$Tare), x = 5)) +
            ggplot2::geom_point(ggplot2::aes(y = (data_all$Poids6-data_all$Tare), x = 6)) +
            ggplot2::geom_hline(ggplot2::aes(
                yintercept = (
                    (data_all$Poids1-data_all$Tare + data_all$Poids2-data_all$Tare + data_all$Poids3-data_all$Tare + data_all$Poids4-data_all$Tare + data_all$Poids5-data_all$Tare + data_all$Poids6-data_all$Tare)
                )/6
            ),
            color = "blue",
            linetype = 3) +
            ggplot2::geom_hline(yintercept = (data_all$Cible),
                                linetype = "dashed",
                                color = "red") +
            theme_qcc() +
            labs(x = "Poids", y = "Weight (Gr)",
                 title = paste("Request",data_all$Request),
                 subtitle = paste("Prélèvement",data_all$Prelevement))
    
})