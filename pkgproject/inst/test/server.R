library(shiny)
library(shinyjs)
library(pkgproject)

shinyServer(function(input, output, session) {

  #####Import Data output
  formData <- reactive({
    data <- sapply(weights, function(x)
      input[[x]])
    data$Date <- as.character(input$Date)
    data
  })




  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.na(input[[x]]) && input[[x]] != ""
             },
             logical(1))

    mandatoryFilled <- all(mandatoryFilled)


    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })

  rv <- reactiveVal(dataLoaded)

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
      shinyjs::show(id = "error",
                    anim = TRUE,
                    animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })

    # To update the global data
    dataLoaded <- loadData()
    rv(dataLoaded)
  })






  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
    shinyjs::reset("Measure1")
    shinyjs::reset("Measure2")
    shinyjs::reset("Measure3")
    shinyjs::reset("Measure4")
    shinyjs::reset("Measure5")
    shinyjs::reset("Measure6")
    shinyjs::reset("Process.Sample")
    shinyjs::reset("Request")
    shinyjs::reset("Target.Value")
    shinyjs::reset("Tare")
  })

  output$responses <- DT::renderDataTable({
    input$submit
    DT::datatable(
      rv(),
      options = list(pageLength = 10, scrollX = TRUE),
      class = 'cell-border stripe',
      rownames = FALSE,
      width = 500
    )
  })

  output$select_cible <- renderUI({
    selectInput(
      "cible",
      label = "Cible",
      choices = Target.Value_p_SPC_ %>%
        filter(Product.Size == input$psize) %>%
        pull(Target.Value) %>% unique()
    )

  })

  output$select_prelev <- renderUI({
    selectInput(
      "prelevement",
      label = "Prelevement",
      choices = rv() %>%
        filter(Request == input$prequest) %>%
        pull(Process.Sample) %>% unique()
    )

  })

  output$plot_hist <- renderPlot({
    request_CL(nasty, input$req)
  })

  output$R_bar <- renderPlot({
    R_bar_chart(nasty, input$req)
  })


  output$summ_req <- DT::renderDataTable({
    DT::datatable(
      summary_stat(nasty, input$req),
      options = list(pageLength = 10, scrollX = TRUE),
      class = 'cell-border stripe',
      rownames = FALSE,
      width = 500
    )
  })

  output$summ_ts <- DT::renderDataTable({
    DT::datatable(
      summary_TS(input$prequest, input$prelevement),
      options = list(pageLength = 10, scrollX = TRUE),
      class = 'cell-border stripe',
      rownames = FALSE,
      width = 500
    )
  })

  output$choices <- renderUI({selectInput(
    "prequest",
    "Request",
    choices = unique(rv()$Request)
  )})

  output$plot_sample <- renderPlot({
    ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(y = (input$Measure1 - input$Tare), x = 1)) +
      ggplot2::geom_point(ggplot2::aes(y = (input$Measure2 - input$Tare), x = 2)) +
      ggplot2::geom_point(ggplot2::aes(y = (input$Measure3 - input$Tare), x = 3)) +
      ggplot2::geom_point(ggplot2::aes(y = (input$Measure4 - input$Tare), x = 4)) +
      ggplot2::geom_point(ggplot2::aes(y = (input$Measure5 - input$Tare), x = 5)) +
      ggplot2::geom_point(ggplot2::aes(y = (input$Measure6 - input$Tare), x = 6)) +
      ggplot2::geom_hline(ggplot2::aes(
        yintercept = (
          input$Measure1 - input$Tare +
            input$Measure2 -
            input$Tare +
            input$Measure3 -
            input$Tare +
            input$Measure4 -
            input$Tare +
            input$Measure5 -
            input$Tare +
            input$Measure6 -
            input$Tare
        ) / 6
      ),
      color = "blue",
      linetype = 3) +
      ggplot2::geom_hline(
        yintercept = (input$Target.Value),
        linetype = "dashed",
        color = "red"
      )
  })

  output$table <- renderTable({
    summary_table(
      input$Measure1,
      input$Measure2,
      input$Measure3,
      input$Measure4,
      input$Measure5,
      input$Measure6,
      input$Tare

    )
  })

  output$TS_g <- renderPlot({
    request_TS(rv(), input$prequest, input$prelevement)
  })
})
