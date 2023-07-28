#-------------------------------------------------------- Proteomics server ----

proteomics_server = function(id, ns, input, output, session) {
  output$test_output = shiny::renderUI({
    shiny::selectInput(
      inputId = ns('truffles'),
      label = 'Proteomics',
      choices = c(1, 2, 3)
    )
  })

  session$userData[[id]]$test = shiny::observeEvent(input$truffles,{
    print(input$truffles)
  })

}
