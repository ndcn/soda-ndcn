#------------------------------------------------------------ Experiment UI ----

experiment_ui = function(id) {
  ns = shiny::NS(id)

  shiny::uiOutput(
    outputId = ns('omics_ui')
  )

  # bs4Dash::tabsetPanel(
  #   type = "tabs",
  #   shiny::tabPanel(
  #     title = "Upload metadata",
  #     shiny::uiOutput(
  #       outputId = ns('up_metadata_ui')
  #     )
  #   ),
  #   shiny::tabPanel(
  #     title = "Upload data",
  #     shiny::uiOutput(
  #       outputId = ns('up_data_ui')
  #     )
  #   ),
  #   shiny::tabPanel(
  #     title = "Visualize data",
  #     shiny::uiOutput(
  #       outputId = ns('visualize_data_ui')
  #     )
  #   ),
  #   shiny::tabPanel(
  #     title = "Geneset enrichment",
  #     shiny::uiOutput(
  #       outputId = ns('geneset_enrichment_ui')
  #     )
  #   ),
  #   shiny::tabPanel(
  #     title = "Over-representation analysis",
  #     shiny::uiOutput(
  #       outputId = ns('over_representation_ui')
  #     )
  #   )
  # )
}

#-------------------------------------------------------- Experiment server ----

experiment_server = function(id, type, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      if (type == 'Lipidomics') {
        lipidomics_server(id = id, ns = ns, input = input, output = output, session = session, module_controler = module_controler)
      } else if (type == 'Proteomics') {
        proteomics_server(id = id, ns = ns, input = input, output = output, session = session, module_controler = module_controler)
      } else if (type == 'Transcriptomics') {
        proteomics_server(id = id, ns = ns, input = input, output = output, session = session, module_controler = module_controler)
      }
    }
  )
}




