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


soda_help_data_visualisation = function(){
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Class distribution",
      soda_help("data_visualisation_class_distribution")
    ),
    shiny::tabPanel(
      title = "Class comparison",
      soda_help("data_visualisation_class_comparison")
    ),
    shiny::tabPanel(
      title = "Volcano plot",
      soda_help("data_visualisation_volcano_plot")
    ),
    shiny::tabPanel(
      title = "Heatmap",
      soda_help("data_visualisation_heatmap")
    ),
    shiny::tabPanel(
      title = "PCA",
      soda_help("data_visualisation_pca")
    ),
    shiny::tabPanel(
      title = "Double bond plot",
      soda_help("data_visualisation_double_bond_plot")
    )
  )
}

soda_help_tables = function(){
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Metadata tables",
      soda_help("tables_metadata")
    ),
    shiny::tabPanel(
      title = "Lipidomics tables",
      soda_help("tables_lipidomics")
    ),
    shiny::tabPanel(
      title = "Feature tables",
      soda_help("tables_feature_metadata")
    ),
    shiny::tabPanel(
      title = "Class tables",
      soda_help("tables_classes")
    ),
    shiny::tabPanel(
      title = "Plot tables",
      soda_help("tables_plots")
    )
  )
}