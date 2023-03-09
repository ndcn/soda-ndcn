library(shiny)
library(bs4Dash)



#------------------------------------------------------ Meta data upload UI ----
soda_merge_tables_ui = function(id, head = T) {
  
  ns = shiny::NS(id)
  shiny::tagList(
    ############################ UPLOAD TAB ##################################
    shiny::fluidRow(
      
      # First column for metadata upload
      shiny::column(
        width = 5,
        shiny::h4("Upload metadata"),
        
        # Data upload
        shiny::fileInput(inputId = ns("file_meta"), label = NULL, multiple = F, accept = c(".csv", ".tsv", ".txt"), width = "100%"),
        
        # Table preview box
        bs4Dash::box(
          title = "Metadata table",
          width = 12,
          DT::dataTableOutput(ns("meta_table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
          collapsible = FALSE
        ),
        
        # Text feedback for blanks, QCs and pools found (text patterns)
        shiny::span(textOutput(outputId = ns("found_blanks"))),
        shiny::span(textOutput(outputId = ns("found_qcs"))),
        shiny::span(textOutput(outputId = ns("found_pools")))
      ),
      
      # Second column for lipidomics data upload
      shiny::column(
        width = 5,
        shiny::h4("Upload lipidomics data"),
        
        # Data upload
        shiny::fileInput(inputId = ns("file_data"), label = NULL, multiple = F, accept = c(".csv", ".tsv", ".txt"), width = "100%"),
        
        # Table preview box
        bs4Dash::box(
          title = "Lipidomics table",
          width = 12,
          DT::dataTableOutput(ns("data_table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
          collapsible = FALSE
        ),
      ),
      
      # third column for data curation
      shiny::column(
        width = 2,
        shiny::tags$h3("Select columns"),
        
        # Display preview or full table
        # shiny::checkboxInput(inputId = ns("preview"), label = "Display preview only", value = head),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns('preview'),
          label = NULL,
          choices = c("Metadata", "Lipidomics"),
          selected = c("Lipidomics"),
          direction = "horizontal",
          status = "default",
          justified = TRUE,
          width = '100%',
          checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
        ),
        
        # Select ID column
        soda_get_col_ui(label = "Metadata IDs", desc = NULL),
        shiny::selectInput(inputId = ns("select_meta_id"), choices = NULL, label = NULL, multiple = F, width = "100%"),
        
        # Select ID column
        soda_get_col_ui(label = "Lipidomics IDs", desc = NULL),
        shiny::selectInput(inputId = ns("select_data_id"), choices = NULL, label = NULL, multiple = F, width = "100%"),
        
        
        # Section for regex text patterns
        shiny::h3("Text patterns"),
        
        # Select blank battern text for regex
        soda_get_col_ui(label ="Blank pattern", desc = 'Text pattern to autodect blanks samples from the above metioned "Sample type" column'),
        shiny::textInput(inputId = ns("blank_pattern"), label = NULL, value = "blank", width = "100%"),
        
        # Select QC battern text for regex
        soda_get_col_ui(label ="QC pattern", desc = 'Text pattern to autodect QC samples from the above metioned "Sample type" column'),
        shiny::textInput(inputId = ns("qc_pattern"), label = NULL, value = "quality", width = "100%"),
        
        # Select pool battern text for regex
        soda_get_col_ui(label ="Pool pattern", desc = 'Text pattern to autodect Pooled samples from the above metioned "Sample type" column'),
        shiny::textInput(inputId = ns("pool_pattern"), label = NULL, value = "pool", width = "100%")
      )
    )
  )

}

#-------------------------------------------------- Meta data upload server ----

soda_merge_tables_server = function(id, max_rows = 10, max_cols = 8, r6) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ############################ UPLOAD TAB ##################################
      
      # File name to upload
      metafile = reactive({
        validate(need(input$file_meta, message = FALSE))
        input$file_meta
      })
      
      datafile = reactive({
        validate(need(input$file_data, message = FALSE))
        input$file_data
      })
      
      
      shiny::observe({
        shiny::req(input$file_meta)
        
        sep = find_delim(path = metafile()$datapath)
        
        meta_table_input = read.csv(metafile()$datapath,
                                    header = T,
                                    sep = sep,
                                    check.names = FALSE)
        print("done")
        
        output$meta_table = renderDataTable({
          print('seen')
          DT::datatable(meta_table_input[1:max_rows, 1:max_cols], options = list(paging = FALSE))
        })
      })
      
      
      shiny::observe({
        shiny::req(input$file_data)
        
        sep = find_delim(path = datafile()$datapath)
        
        data_table_input = read.csv(datafile()$datapath,
                                    header = T,
                                    sep = sep,
                                    check.names = FALSE)
        print("done")
        
        output$data_table = renderDataTable({
          print('seen')
          DT::datatable(data_table_input[1:max_rows, 1:max_cols], options = list(paging = FALSE))
        })
      })
      
      
      
      # Load data as raw metadata
      # shiny::observe({
      #   if (!is.null(metafile()$datapath)){
      #     sep = find_delim(path = metafile()$datapath)
      #     r6$set_raw_meta(read.csv(metafile()$datapath,
      #                              header = T,
      #                              sep = sep,
      #                              check.names = FALSE))
      #     r6$tables$meta_raw[is.na(r6$tables$meta_raw)] = "missing"
      #     r6$tables$meta_raw[r6$tables$meta_raw == ""] = "missing"
      #     
      #     # Select ID column from the raw meta data
      #     observe({
      #       shiny::updateSelectInput(
      #         session = session,
      #         inputId = "select_id",
      #         choices = colnames(r6$tables$meta_raw)
      #       )
      #     })
      #     
      #   }
      # })
      
      
      
      
      # Output a preview or the whole table depending on the user input
      # shiny::observe({
        # shiny::req(meta_table_input())
        # print(meta_table_input())
        # output$meta_table = renderDataTable({
        #   DT::datatable(r6$tables$meta_raw[1:min(max_rows, nrow(r6$tables$meta_raw)),1:min(max_cols, ncol(r6$tables$meta_raw))], options = list(paging = FALSE))
        # })
      # })
    
    }
  )
}


