
##################################################################### Start ####

#------------------------------------------------------------ Help Start UI ----
help_start_ui = function(id){
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Input format",
      shiny::includeMarkdown("./man/input_formats.md")
    ),
    shiny::tabPanel(
      title = "Start module",
      shiny::includeMarkdown("./man/mod_start.md")
    )
  )
}

#-------------------------------------------------------- Help Start server ----

help_start_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

############################################################# Single omics ####

#----------------------------------------------------- Help Single omics UI ----
help_single_omics_ui = function(id){
  ns = shiny::NS(id)

  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Upload",
      shiny::includeMarkdown("./man/single_omics/upload.md")
    ),
    shiny::tabPanel(
      title = "Visualization",
      shiny::includeMarkdown("./man/single_omics/visualization.md")
    ),
    shiny::tabPanel(
      title = "Enrichment analyses",
      shiny::includeMarkdown("./man/single_omics/enrichment_analyses.md")
    )
  )
}

#------------------------------------------------- Help Single omics server ----

help_single_omics_server = function(id, main_output) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
    }
  )
}

