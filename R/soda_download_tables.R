
#------------------------------------------------------- Download tables UI ----
soda_download_tables_ui = function(id) {
  ns = shiny::NS(id)
  
  shiny::tagList(
    shiny::column(
      width = 6,
      
      shiny::span("Filtered metadata:"),
      shiny::span(textOutput(outputId = ns("msg_meta_filtered")), style="color:red"),
      shiny::downloadButton(
        outputId = ns("download_meta_filtered"),
        label = "Filtered metadata"
      ),
      

      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_data_filtered"),
        label = "Filtered lipidomics data"
      ),

      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_feat_filtered"),
        label = "Lipidomics feature table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_data_z_scored"),
        label = "Z-scored table"
      ),
 
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_data_class_norm"),
        label = "Class normalised table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_data_total_norm"),
        label = "Total normalised table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_data_class_norm_z_scored"),
        label = "Z-scored class normalised table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_data_total_norm_z_scored"),
        label = "Z-scored total normalised table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_data_class_table"),
        label = "Class table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_data_class_table_z_scored"),
        label = "Z-scored class table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_class_distribution_table"),
        label = "Class distribution table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_volcano_table"),
        label = "Volcano table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_heatmap_table"),
        label = "Heatmap table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_pca_scores_table"),
        label = "PCA scores table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_pca_loadings_table"),
        label = "PCA loadings table"
      ),
      
      shiny::br(),
      shiny::downloadButton(
        outputId = ns("download_dbplot_table"),
        label = "Double bonds table"
      ),
      
    )
  )
}



#--------------------------------------------------- Download tables server ----
soda_download_tables_server = function(id, r6) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # Download filtered metadata
      
      if (!is.null(r6$tables$meta_filtered)) {
        output$msg_meta_filtered = shiny::renderText({"Mu."})
        output$download_meta_filtered = shiny::downloadHandler(
          filename = function(){"metadata_filtered.csv"},
          content = function(file_name){
            write.csv(r6$tables$meta_filtered, file_name)
          }
        )
      } else {
        output$msg_meta_filtered = shiny::renderText({"Data unavailable."})
      }
      
      if (is.null(r6$tables$data_filtered)) {
        output$msg_data_filtered = shiny::renderText({"Data unavailable."})
      } else {
        output$msg_data_filtered = shiny::renderText({"Mu."})
      }
      
    }
  )
}
