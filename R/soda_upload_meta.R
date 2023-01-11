library(shiny)
library(shinydashboard)

#------------------------------------------------------ Meta data upload UI ----
soda_upload_meta_ui = function(id, head = F) {

  ns = NS(id)
  shiny::tagList(
    
    # First column with the table input and preview
    shiny::column(
      width = 8,
      shiny::h2("Upload metadata"),
      shiny::fileInput(inputId = ns("file"), label = NULL, multiple = F, accept = c(".csv"), width = "100%"),
      shinydashboard::box(
        width = 12,
        DT::dataTableOutput(ns("table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
      ),
      shiny::span(textOutput(outputId = ns("found_blanks"))),
      shiny::span(textOutput(outputId = ns("found_qcs"))),
      shiny::span(textOutput(outputId = ns("found_pools")))
    ),
    
    # Second column for data curation
    shiny::column(
      width = 4,
      actionButton(ns("do"), "Click Me"),
      shiny::tags$h3("Select columns"),
      
      # Display preview or full table
      shiny::checkboxInput(inputId = ns("preview"), label = "Display preview only", value = head),
      
      # Select ID column
      soda_get_col_ui(label = "Sample IDs", desc = "Column containing the sample IDs."),
      shiny::selectInput(inputId = ns("select_id"), choices = NULL, label = NULL, multiple = F),
      shiny::span(textOutput(outputId = ns("id_error")), style="color:red"),
      
      # Select sample type column
      soda_get_col_ui(label ="Type column", desc = "Column containing the sample types."),
      shiny::selectInput(inputId = ns("select_sample_type"), choices = NULL, label = NULL, multiple = F),
      
      # Section for regex text patterns
      shiny::h3("Text patterns"),
      
      # Select blank battern text for regex
      soda_get_col_ui(label ="Blank pattern", desc = 'Text pattern to autodect blanks samples from the above metioned "Sample type" column'),
      shiny::textInput(inputId = ns("blank_pattern"), label = NULL, value = "blank"),

      # Select QC battern text for regex      
      soda_get_col_ui(label ="QC pattern", desc = 'Text pattern to autodect QC samples from the above metioned "Sample type" column'),
      shiny::textInput(inputId = ns("qc_pattern"), label = NULL, value = "quality"),
      
      # Select pool battern text for regex
      soda_get_col_ui(label ="Pool pattern", desc = 'Text pattern to autodect Pooled samples from the above metioned "Sample type" column'),
      shiny::textInput(inputId = ns("pool_pattern"), label = NULL, value = "pool")
    )
  )
}

#-------------------------------------------------- Meta data upload server ----

soda_upload_meta_server = function(id, max_rows = 10, max_cols = 8, r6 = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # The selected file, if any
      userFile = reactive({
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      shiny::observe({
        if (!is.null(userFile()$datapath)){
          r6$set_raw_meta(read.csv(userFile()$datapath,
                                   header = T,
                                   sep = ",",
                                   check.names = FALSE))
          # Select ID column from the raw meta data
          observe({
            shiny::updateSelectInput(
              session = session,
              inputId = "select_id",
              choices = colnames(r6$meta_raw)
            )
          })
          # Select sample type column from the raw meta data
          shiny::updateSelectInput(
            session = session,
            inputId = "select_sample_type",
            choices = colnames(r6$meta_raw)
          )
        }
      })

      # Output a preview or the whole table depending on the user input
      shiny::observe({
        if (!is.null(userFile()$datapath)) {
          if (input$preview){
            output$table = renderDataTable({
              DT::datatable(r6$meta_raw[1:min(max_rows, nrow(r6$meta_raw)),1:min(max_cols, ncol(r6$meta_raw))], options = list(paging = FALSE))
            })
          }else{
            output$table = renderDataTable({
              DT::datatable(r6$meta_raw, options = list(paging = FALSE))
            })
          }
        }
      })
      
      # Set values to the R6 object
      shiny::observe({
        if (!is.null(input$select_id)){
          
          # Initialise filtered metadata with the ID column
          r6$set_filtered_meta(id_col = input$select_id)
          if (r6$non_unique_ids_meta){
            output$id_error = shiny::renderText({"Non-uniques in ID column. Please correct or choose another column"})
          } else {
            output$id_error = shiny::renderText({NULL})
          }
          
          # Set columns
          r6$set_col(col = input$select_sample_type, type = "type")
          
          # Text patterns
          r6$set_text_pattern(pattern = input$qc_pattern, type = "qc")
          r6$set_text_pattern(pattern = input$blank_pattern, type = "blank")
          r6$set_text_pattern(pattern = input$pool_pattern, type = "pool")
          
          if (r6$pattern_blank != "") {
            count_blanks = length(grep(pattern = r6$pattern_blank,
                                       x = r6$meta_raw[,r6$col_type],
                                       ignore.case = TRUE))
          }else{
            count_blanks = 0
          }
          
          if (r6$pattern_qc != "") {
            count_qcs = length(grep(pattern = r6$pattern_qc,
                                    x = r6$meta_raw[,r6$col_type],
                                    ignore.case = TRUE))
          }else{
            count_qcs = 0
          }
          if (r6$pattern_pool != "") {
            count_pools = length(grep(pattern = r6$pattern_pool,
                                      x = r6$meta_raw[,r6$col_type],
                                      ignore.case = TRUE))
          }else{
            count_pools = 0
          }
          output$found_blanks = shiny::renderText({paste0("Blanks found: ", as.character(count_blanks))})
          output$found_qcs = shiny::renderText({paste0("QCs found: ", as.character(count_qcs))})
          output$found_pools = shiny::renderText({paste0("Pools found: ", as.character(count_pools))})
        }
      })
      
      observeEvent(input$do, {
        print("Nothing to test")
      })
    }
  )
}


