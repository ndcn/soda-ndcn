#------------------------------------------------------------ Experiment UI ----

experiment_ui = function(id) {
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Upload metadata",
      shiny::uiOutput(
        outputId = ns('up_metadata_ui')
      )
    ),
    shiny::tabPanel(
      title = "Filter metadata",

    ),
    shiny::tabPanel(
      title = "Upload data",
      shiny::h4('Placeholder_3')
    ),
    shiny::tabPanel(
      title = "Filter data",
      shiny::h4('Placeholder_4')
    ),
    shiny::tabPanel(
      title = "Visualize data",
      shiny::h4('Placeholder_5')
    ),
    shiny::tabPanel(
      title = "Geneset enrichment",
      shiny::h4('Placeholder_6')
    ),
    shiny::tabPanel(
      title = "Over-representation analysis",
      shiny::h4('Placeholder_7')
    )
  )
}

#-------------------------------------------------------- Experiment server ----

experiment_server = function(id, type) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns

      if (type == 'Lipidomics') {
        lipidomics_server(id = id, ns = ns, input = input, output = output, session = session)
      } else if (type == 'Proteomics') {
        proteomics_server(id = id, ns = ns, input = input, output = output, session = session)
      } else if (type == 'Transcriptomics') {
        transcriptomics_server(id = id, ns = ns, input = input, output = output, session = session)
      }
    }
  )
}




