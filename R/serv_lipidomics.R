#-------------------------------------------------------- Lipidomics server ----

lipidomics_server = function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      output$test_output = shiny::renderUI({
        shiny::selectInput(
          inputId = ns('truffles'),
          label = 'truffles',
          choices = c(1, 2, 3)
        )
      })
    }
  )
}
