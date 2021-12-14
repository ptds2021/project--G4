library(shiny)
library(shinyjs)
library(pkgproject)
shinyServer(function(input, output, session) {

#####Import Data output
    formData <- reactive({
        data <- sapply(weights, function(x)input[[x]])
        data$Date <- as.character(input$Date)
        data
    })


    observe({

        mandatoryFilled <-
            vapply(fieldsMandatory,
                   function(x) {
                       !is.na(input[[x]]) || input[[x]] != ""
                   },
                   logical(1))
        mandatoryFilled <- all(mandatoryFilled)


        shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })

    observeEvent(input$submit, {

        #UX design
        shinyjs::disable("submit")
        shinyjs::show("submit_msg")
        shinyjs::hide("error")


        tryCatch({
            saveData(formData())
            shinyjs::reset("form")
            shinyjs::hide("form")
            shinyjs::show("thankyou_msg")
        },
        error = function(err) {
            shinyjs::html("error_msg", err$message)
            shinyjs::show(id = "error", anim = TRUE, animType = "fade")
        },
        finally = {
            shinyjs::enable("submit")
            shinyjs::hide("submit_msg")
        })
    })




    observeEvent(input$submit_another, {
        shinyjs::show("form")
        shinyjs::hide("thankyou_msg")
        shinyjs::reset("Poids1")
        shinyjs::reset("Poids2")
        shinyjs::reset("Poids3")
        shinyjs::reset("Poids4")
        shinyjs::reset("Poids5")
        shinyjs::reset("Poids6")
        shinyjs::reset("Prelevement")
        shinyjs::reset("Request")
        shinyjs::reset("Cible")
        shinyjs::reset("Tare")
    })

    output$responses <- DT::renderDataTable({
        input$submit
        DT::datatable(loadData(),
                      options = list(pageLength = 10, scrollX = TRUE),
                      class = 'cell-border stripe', rownames = FALSE, width =500)
    })

    output$select_cible <- renderUI({

        selectInput("cible", label = "Cible",


                    choices = cible_p_SPC_ %>%
                        filter(`Batch Pod size` == input$psize) %>%
                        pull(Cible) %>% unique()
        )

    })

    output$select_prelev <- renderUI({

        selectInput("prelevement", label = "Prelevement",


                    choices = nasty %>%
                        filter(Request == input$prequest) %>%
                        pull(Prelevement) %>% unique()
        )

    })

    output$plot_hist <- renderPlot({
        request_CL(input$req)
    })

    output$cible_hist <- renderPlot({
        cible_CL(input$psize, input$cible)
    })

    output$TS_g <- renderPlot({
        request_TS(input$prequest, input$prelevement)
    })

    output$summ_req <- DT::renderDataTable({
        DT::datatable(summary_stat(input$Request),
                      options = list(pageLength = 10, scrollX = TRUE),
                      class = 'cell-border stripe', rownames = FALSE, width =500)
    })

    output$summ_ts <- DT::renderDataTable({
        DT::datatable(summary_TS(input$prequest, input$prelevement),
                      options = list(pageLength = 10, scrollX = TRUE),
                      class = 'cell-border stripe', rownames = FALSE, width =500)
    })
})
