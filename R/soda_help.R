library(shiny)
library(markdown)

soda_help = function(help_file){
	shiny::includeMarkdown(paste0("./help/", help_file, ".md"))
}

soda_help_data_upload = function(){
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Table format",
      soda_help("data_upload_table_format")
    ),
    shiny::tabPanel(
      title = "Metadata upload",
      soda_help("data_upload_metadata_upload")
    ),
    shiny::tabPanel(
      title = "Metadata filtering",
      soda_help("data_upload_metadata_filter")
    ),
    shiny::tabPanel(
      title = "Lipidomics upload",
      soda_help("data_upload_lipidomics_upload")
    ),
    shiny::tabPanel(
      title = "Lipidomics filtering",
      soda_help("data_upload_lipidomics_filter")
    )
  )
}