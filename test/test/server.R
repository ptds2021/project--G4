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
    
    output$select_cible <- renderUI({
        
        selectInput("cible", label = "Cible",
                    
                    # the choices below is filtered based on input$country
                    choices = cible_p_SPC_ %>% 
                        filter(`Batch Pod size` == input$psize) %>% 
                        pull(Cible) %>% unique()
        )
        
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
            labs(x = "Poids", y = "Weight (Gr)",
                 title = paste("Request",input$Request),
                 subtitle = paste("Prélèvement",input$Prelevement))
    })
    
    output$table_summary <- renderTable({
        inputs <- c(input$Poids1-input$Tare, input$Poids2-input$Tare, input$Poids3-input$Tare, input$Poids4-input$Tare, input$Poids5-input$Tare, input$Poids6-input$Tare)
        mean <- mean(inputs)
        median <- median(inputs)
        q1 <- quantile(inputs, probs=0.25)
        q3 <- quantile(inputs, probs=0.75)
        sd <- sd(inputs)
        
        summary <- dplyr::as_tibble(c(mean, median, q1, q3, sd))
        summary$name <- c("mean", "median", "q1", "q3", "sd")
        summary
        })
    
    
    output$plot_hist <- renderPlot({
        request_CL(input$Request)
    })
    
    output$cible_hist <- renderPlot({
        cible_CL(input$psize, input$cible)
    })
    
    output$summ_req <- DT::renderDataTable({
        DT::datatable(summary_stat(input$Request),
                      options = list(pageLength = 5, scrollX = TRUE),
                      class = 'cell-border stripe', rownames = FALSE, width =500)
    })  

})