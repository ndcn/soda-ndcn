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
        
        # Select ID column
        soda_get_col_ui(label = "Metadata IDs", desc = NULL),
        shiny::selectInput(inputId = ns("select_meta_id"), choices = NULL, label = NULL, multiple = F, width = "100%"),
        
        # Select ID column
        soda_get_col_ui(label = "Lipidomics IDs", desc = NULL),
        shiny::selectInput(inputId = ns("select_data_id"), choices = NULL, label = NULL, multiple = F, width = "100%"),
        
        # Merge tables
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::fluidRow(
          shiny::actionButton(inputId = ns("add_table"),
                              label = "Add table",
                              width = "50%"
          ),
          shiny::actionButton(inputId = ns("reset_table"),
                              label = "Reset table",
                              width = "50%"
          )
        ),
        
        # Download tables
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::fluidRow(
          shiny::downloadButton(
            outputId = ns("download_meta"),
            label = "Metadata",
            style = "width:50%;"
          ),
          shiny::downloadButton(
            outputId = ns("download_data"),
            label = "Data table",
            style = "width:50%;"
          )
        )
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
      
      # Initialise merged table
      main_meta = shiny::reactiveVal()
      main_lips = shiny::reactiveVal()
      
      # File name to upload
      meta_table_input = reactive({
        validate(need(input$file_meta, message = FALSE))
        sep = find_delim(path = input$file_meta$datapath)
        read.csv(input$file_meta$datapath,
                 header = T,
                 sep = sep,
                 check.names = FALSE)
      })
      
      data_table_input = reactive({
        validate(need(input$file_data, message = FALSE))
        sep = find_delim(path = input$file_data$datapath)
        read.csv(input$file_data$datapath,
                 header = T,
                 sep = sep,
                 check.names = FALSE)
      })
      
      
      shiny::observe({
        shiny::req(meta_table_input())
        shiny::updateSelectInput(
          session = session,
          inputId = "select_meta_id",
          choices = colnames(meta_table_input()),
          selected = colnames(meta_table_input())[1]
        )
        output$meta_table = renderDataTable({
          DT::datatable(meta_table_input()[1:max_rows, 1:max_cols], options = list(paging = FALSE))
        })
      })
      
      shiny::observe({
        shiny::req(data_table_input())
        shiny::updateSelectInput(
          session = session,
          inputId = "select_data_id",
          choices = colnames(data_table_input()),
          selected = colnames(data_table_input())[1]
        )
        output$data_table = renderDataTable({
          DT::datatable(data_table_input()[1:max_rows, 1:max_cols], options = list(paging = FALSE))
        })
      })
      
      
      # Add table
      shiny::observeEvent(input$add_table,{
        
        # Set indexes for lipids table
        in_lips = set_index_col(data_table = data_table_input(), idx = input$select_data_id)
        
        # Coerce meta to str to avoid errors when joining and set index
        in_meta = data.frame(lapply(meta_table_input(), as.character), stringsAsFactors=FALSE, check.names = F)
        in_meta = set_index_col(data_table = in_meta, idx = input$select_meta_id)
        
        # Bind all, Bind rows
        if (is.null(main_meta())) {
          main_meta(in_meta)
          main_lips(in_lips)
          
        } else {
          main_meta(dplyr::bind_rows(main_meta(), in_meta))
          main_lips(dplyr::bind_rows(main_lips(), in_lips))
        }
        
        output$meta_table = renderDataTable({
          NULL
        })
        output$data_table = renderDataTable({
          NULL
        })
        
        shiny::updateSelectInput(
          session = session,
          inputId = "select_meta_id",
          choices = NULL,
          selected = character(0)
        )
        
        shiny::updateSelectInput(
          session = session,
          inputId = "select_data_id",
          choices = NULL,
          selected = character(0)
        )
        
        
      })
      
      # Reset table
      shiny::observeEvent(input$reset_table, {
        main_meta(NULL)
        main_lips(NULL)
        
        main_meta(NULL)
        main_lips(NULL)
        
        output$meta_table = renderDataTable({
          NULL
        })
        output$data_table = renderDataTable({
          NULL
        })
      })
      
      
      # Download combined meta
      output$download_meta = shiny::downloadHandler(
        filename = function(){timestamped_name("combined_meta.csv")},
        content = function(file_name){
          main_meta = cbind(ID = rownames(main_meta()), main_meta())
          write.table(main_meta, file_name, sep = ",", row.names = F, na='')
        }
      )
      
      # Download combined data
      output$download_data = shiny::downloadHandler(
        filename = function(){timestamped_name("combined_data.csv")},
        content = function(file_name){
          main_lips = cbind(ID = rownames(main_lips()), main_lips())
          write.table(main_lips, file_name, sep = ",", row.names = F, na='')
        }
      )
    
    }
  )
}


