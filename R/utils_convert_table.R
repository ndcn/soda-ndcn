#--------------------------------------------------------- Convert table UI ----
utils_convert_table_ui = function(id, head = T) {
  
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      # Column to display a preview of the table uploaded
      shiny::column(
        width = 10,
        shiny::h4("Upload proteomics long table"),
        
        # Data upload
        shiny::fileInput(inputId = ns("file_prots"), label = NULL, multiple = F, accept = c(".csv", ".tsv", ".txt"), width = "100%"),
        
        # Table preview box
        shiny::fluidRow(
          bs4Dash::box(
            title = "Preview",
            width = 12,
            DT::dataTableOutput(ns("prots_preview_raw")),style = "height:350px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          )
        ),
        shiny::fluidRow(
          bs4Dash::box(
            title = "Metadata",
            width = 6,
            DT::dataTableOutput(ns("prots_preview_meta")),style = "height:350px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          ),
          bs4Dash::box(
            title = "Data",
            width = 6,
            DT::dataTableOutput(ns("prots_preview_data")),style = "height:350px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          )
        )

      ),
      
      # third column for data curation
      shiny::column(
        width = 2,
        shiny::tags$h3("Select columns"),
        
        # Select ID column
        soda_get_col_ui(label = "ID column", desc = NULL),
        shiny::selectizeInput(inputId = ns("select_id"), choices = NULL, label = NULL, multiple = F, width = "100%"),
        
        # Select metadata columns
        soda_get_col_ui(label = "Metadata columns (non-features)", desc = NULL),
        shiny::selectizeInput(inputId = ns("select_metas"), choices = NULL, label = NULL, multiple = T, width = "100%"),
        
        # Select features column
        soda_get_col_ui(label = "Features column", desc = NULL),
        shiny::selectizeInput(inputId = ns("select_features"), choices = NULL, label = NULL, multiple = F, width = "100%"),
        
        # Select values column
        soda_get_col_ui(label = "Values column", desc = NULL),
        shiny::selectizeInput(inputId = ns("select_values"), choices = NULL, label = NULL, multiple = F, width = "100%"),
        
        # Merge tables
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::tags$h3("Convert"),
        shiny::checkboxInput(inputId = ns("remove_conflicts"),
                             label = "Remove conflicts",
                             value = TRUE,
                             width = "100%"),
        shiny::actionButton(inputId = ns("convert_table"),
                            label = "Start",
                            width = "100%"
        ),
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::tags$h3("Download"),
        shiny::fluidRow(
          shiny::downloadButton(outputId = ns("download_meta"),
                                label = "Metadata",
                                style = "width:50%;"
          ),
          shiny::downloadButton(outputId = ns("download_data"),
                              label = "Data",
                              style = "width:50%;"
          )
        )
      )
    )
  )
}

#----------------------------------------------------- Convert table server ----

utils_convert_table_server = function(id, max_rows = 10, max_cols = 15) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # Initialise tables
      out_tables = shiny::reactiveVal()
      
      # File name to upload
      prots_raw = reactive({
        validate(need(input$file_prots, message = FALSE))
        sep = find_delim(path = input$file_prots$datapath)
        read.table(input$file_prots$datapath,
                   quote = "",
                   header = T,
                   sep = sep,
                   check.names = FALSE)
      })
      
      shiny::observeEvent(input$file_prots,{
        shiny::req(prots_raw())
        shiny::updateSelectizeInput(
          session = session,
          inputId = "select_id",
          choices = colnames(prots_raw()),
          selected = colnames(prots_raw())[1]
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "select_metas",
          choices = colnames(prots_raw()),
          selected = character(0)
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "select_features",
          choices = colnames(prots_raw()),
          selected = colnames(prots_raw())[2]
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "select_values",
          choices = colnames(prots_raw()),
          selected = colnames(prots_raw())[3]
        )
        
        output$prots_preview_raw = renderDataTable({
          DT::datatable(prots_raw()[1:max_rows, 1:min(ncol(prots_raw()), max_cols)], options = list(paging = FALSE))
        })
      })
      
      # Convert and display tables
      shiny::observeEvent(input$convert_table,{
        out_tables(convert_long(long_table = prots_raw(),
                                id_col = input$select_id,
                                feature_col = input$select_features,
                                values_col = input$select_values,
                                meta_cols = input$select_metas,
                                remove_conflicts = input$remove_conflicts))
        output$prots_preview_meta = renderDataTable({
          DT::datatable(out_tables()$meta_table[1:max_rows, 1:min(ncol(out_tables()$meta_table), max_cols)], options = list(paging = FALSE))
        })
        output$prots_preview_data = renderDataTable({
          DT::datatable(out_tables()$data_table[1:max_rows, 1:min(ncol(out_tables()$data_table), max_cols)], options = list(paging = FALSE))
        })
        
      })
      
      # Download converted tables
      output$download_meta = shiny::downloadHandler(
        filename = function(){timestamped_name("prot_meta.tsv")},
        content = function(file_name){
          write.table(out_tables()$meta_table,
                      file_name,
                      sep = "\t",
                      row.names = F,
                      na='')
        }
      )
      output$download_data = shiny::downloadHandler(
        filename = function(){timestamped_name("prot_data.tsv")},
        content = function(file_name){
          write.table(out_tables()$data_table,
                      file_name,
                      sep = "\t",
                      row.names = F,
                      na='')
        }
      )
      
    }
  )
}


