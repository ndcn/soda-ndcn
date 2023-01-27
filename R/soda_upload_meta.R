library(shiny)
library(bs4Dash)

#------------------------------------------------------ Meta data upload UI ----
soda_upload_meta_ui = function(id, head = F) {
  
  ns = NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      ############################ UPLOAD TAB ##################################
      title = "Upload",
      shiny::fluidRow(
        
        # First column with the table input and preview of the raw data
        shiny::column(
          width = 9,
          shiny::h2("Upload metadata"),
          
          # Table preview box
          bs4Dash::box(
            title = "Sample metadata table (raw)",
            width = 12,
            DT::dataTableOutput(ns("raw_table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          ),
          
          # Data upload
          shiny::fileInput(inputId = ns("file"), label = NULL, multiple = F, accept = c(".csv"), width = "100%"),
          
          # Text feedback for blanks, QCs and pools found (text patterns)
          shiny::span(textOutput(outputId = ns("found_blanks"))),
          shiny::span(textOutput(outputId = ns("found_qcs"))),
          shiny::span(textOutput(outputId = ns("found_pools")))
        ),
        
        # Second column for data curation
        shiny::column(
          width = 3,
          shiny::tags$h3("Select columns"),
          
          # Display preview or full table
          shiny::checkboxInput(inputId = ns("preview"), label = "Display preview only", value = head),
          
          # Select ID column
          soda_get_col_ui(label = "Sample IDs", desc = "Column containing the sample IDs."),
          shiny::selectInput(inputId = ns("select_id"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          shiny::span(textOutput(outputId = ns("id_error")), style="color:red"),
          
          # Select sample type column
          soda_get_col_ui(label ="Type column", desc = "Column containing the sample types."),
          shiny::selectInput(inputId = ns("select_sample_type"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          
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
    ),
    shiny::tabPanel(
      ############################ FILTER TAB ##################################
      title = "Filter",
      shiny::fluidRow(
        
        # First column with the table input and preview of the filtered data
        shiny::column(
          width = 9,
          
          # Filtered table preview
          shiny::h2("Filter metadata"),
          bs4Dash::box(
            title = "Sample metadata table (filtered)",
            width = 12,
            DT::dataTableOutput(ns("filtered_table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          ),
          
          # Progress bar for the row count
          shiny::h2("Filtered data"),
          shinyWidgets::progressBar(
            id = ns("row_count_bar"),
            title = "Sample count",
            value = 100,
            total = 100,
            unit_mark = "%"
          )
        ),
        
        # Second column for sample filtering
        shiny::column(
          width = 3,
          shiny::h4("Sample filtering"),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
          
          # Non-sample exclusions
          shiny::h5("Non-sample exclusion"),
          
          # Exclude blanks
          shiny::checkboxInput(
            inputId = ns("exclude_blanks"),
            label = "Exclude blanks",
            value = T
          ),
          # Exclude QCs
          shiny::checkboxInput(
            inputId = ns("exclude_qcs"),
            label = "Exclude QCs",
            value = T
          ),
          # Exclude Pools
          shiny::checkboxInput(
            inputId = ns("exclude_pools"),
            label = "Exclude pools",
            value = T
          ),
          
          # Exclusion based on a metadata column value
          shiny::h5("Metadata exclusion"),
          shiny::h6("Exclude samples based on metadata values"),
          
          # Metadata column selection
          shiny::selectInput(inputId = ns("exclusion_meta_col"), choices = NULL, label = "Column", multiple = F, width = "100%"),
          
          # Value in the metadata column
          shiny::selectInput(inputId = ns("exclusion_meta_val"), choices = NULL, label = "Value", multiple = F, width = "100%"),
          
          # Rows to exclude
          shiny::selectizeInput(inputId = ns("exclusion_meta_row"), choices = NULL, label = "Samples", multiple = T, width = "100%"),
          
          # Manual sample exclusion (selection from rows in the filtered metadata table)
          shiny::h5("Manual sample exclusion"),
          shiny::h6("Manually select samples to exclude"),
          shiny::selectizeInput(inputId = ns("exclusion_manual"), choices = NULL, label = NULL, multiple = T, width = "100%"),
          
          # Action buttons to apply filters, clear filters or reset filtered metadata 
          shiny::fluidRow(
            shiny::actionButton(
              inputId = ns("apply_filters"),
              label = "Filter",
              width = "33%"
            ),
            shiny::actionButton(
              inputId = ns("clear_filters"),
              label = "Clear filters",
              width = "33%"
            ),
            shiny::actionButton(
              inputId = ns("reset_table"),
              label = "Reset table",
              width = "33%"
            )
          )
        )
      )
    )
  )
}

#-------------------------------------------------- Meta data upload server ----

soda_upload_meta_server = function(id, max_rows = 10, max_cols = 8, r6 = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ############################ UPLOAD TAB ##################################
      
      # File name to upload
      table_file = reactive({
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # Load data as raw metadata
      shiny::observe({
        if (!is.null(table_file()$datapath)){
          r6$set_raw_meta(read.csv(table_file()$datapath,
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
            choices = colnames(r6$meta_raw),
            selected = colnames(r6$meta_raw)[2]
          )
        }
      })

      # Output a preview or the whole table depending on the user input
      shiny::observe({
        if (!is.null(table_file()$datapath)) {
          if (input$preview){
            output$raw_table = renderDataTable({
              DT::datatable(r6$meta_raw[1:min(max_rows, nrow(r6$meta_raw)),1:min(max_cols, ncol(r6$meta_raw))], options = list(paging = FALSE))
            })
          }else{
            output$raw_table = renderDataTable({
              DT::datatable(r6$meta_raw, options = list(paging = FALSE))
            })
          }
        }
      })
      
      # Set values to the R6 object
      shiny::observe({
        if (!is.null(input$select_id)){
          
          # Initialise filtered metadata with the ID column
          r6$set_col(col = input$select_id, type = "id_meta")
          r6$set_filtered_meta()
          
          # Check if valid IDs or not
          if (r6$non_unique_ids_meta){
            output$id_error = shiny::renderText({"Non-uniques in ID column. Please correct or choose another column"})
            output$filtered_table = renderDataTable({NULL})

          } else {
            output$id_error = shiny::renderText({NULL})
            output$filtered_table = renderDataTable({
              DT::datatable(r6$meta_filtered, options = list(paging = FALSE))
            })
            
            # Update progress bar with row count (filtering tab)
            shinyWidgets::updateProgressBar(
              session = session,
              id = "row_count_bar",
              value = nrow(r6$meta_filtered),
              total = nrow(r6$meta_raw)
            )
            # Update metadata column input (filtering tab)
            shiny::updateSelectInput(
              session = session,
              inputId = "exclusion_meta_col",
              choices = colnames(r6$meta_filtered)
            )
            
            # Update input for the manual exclusion (filtering tab)
            shiny::updateSelectizeInput(
              session = session,
              inputId = "exclusion_manual",
              choices = rownames(r6$meta_filtered)
            )
          }
          
          # Set columns
          r6$set_col(col = input$select_sample_type, type = "type")
          
          # Text patterns
          r6$set_text_pattern(pattern = input$qc_pattern, type = "qc")
          r6$set_text_pattern(pattern = input$blank_pattern, type = "blank")
          r6$set_text_pattern(pattern = input$pool_pattern, type = "pool")
          
          # Get text pattern counts
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
          
          # Update text pattern feedback
          output$found_blanks = shiny::renderText({paste0("Blanks found: ", as.character(count_blanks))})
          output$found_qcs = shiny::renderText({paste0("QCs found: ", as.character(count_qcs))})
          output$found_pools = shiny::renderText({paste0("Pools found: ", as.character(count_pools))})
        }
      })
      
      ############################ FILTER TAB ##################################
      
      # Update the metadata value once a metadata column is selected
      shiny::observe({
        if (!is.null(input$exclusion_meta_col)) {
          shiny::updateSelectInput(
            session = session,
            inputId = "exclusion_meta_val",
            choices = unique(r6$meta_filtered[,input$exclusion_meta_col]),
            selected = character(0)
          )
        }
      })
      
      # Update the rows to filter once a metadata value is selected
      shiny::observe({
        if (!is.null(input$exclusion_meta_val)) {
          shiny::updateSelectizeInput(
            session = session,
            inputId = "exclusion_meta_row",
            choices = rownames(r6$meta_filtered)[r6$meta_filtered[,input$exclusion_meta_col] == input$exclusion_meta_val],
            selected = rownames(r6$meta_filtered)[r6$meta_filtered[,input$exclusion_meta_col] == input$exclusion_meta_val]
          )
        }
      })
      
      # Filter button
      shiny::observeEvent(input$apply_filters, {
        
        # Initialise rows to delete
        del_rows = c()
        
        # Get blank rows
        if (input$exclude_blanks){
          del_rows = c(del_rows, r6$get_idx_blanks())
        }
        
        # Get QC rows
        if (input$exclude_qcs){
          del_rows = c(del_rows, r6$get_idx_qcs())
        }
        
        # Get Pool rows
        if (input$exclude_pools){
          del_rows = c(del_rows, r6$get_idx_pools())
        }
        
        # Add metadata and manual exclusions
        del_rows = c(del_rows,input$exclusion_meta_row,input$exclusion_manual)
        del_rows = sort(unique(del_rows))
        
        # Filter out the data
        if (!is.null(del_rows)){
          r6$meta_filtered = r6$meta_filtered[!(row.names(r6$meta_filtered) %in% del_rows),]
        }

        # Update feedback on the row count progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "row_count_bar",
          value = nrow(r6$meta_filtered),
          total = nrow(r6$meta_raw)
        )
        
        # Update feedback on the filtered metadata preview
        output$filtered_table = renderDataTable({
          DT::datatable(r6$meta_filtered, options = list(paging = FALSE))
        })
        
        # Reset the metacolumn value to None after filtering
        shiny::updateSelectInput(
          session = session,
          inputId = "exclusion_meta_val",
          selected = character(0)
        )
        
        # Reset the metadata rows to None after filtering
        shiny::updateSelectizeInput(
          session = session,
          inputId = "exclusion_meta_row",
          selected = character(0)
        )
        
        # Same for the manual exclusion and set the values to the remaining rows
        shiny::updateSelectizeInput(
          session = session,
          inputId = "exclusion_manual",
          choices = rownames(r6$meta_filtered),
          selected = character(0)
        )
        
        # Reset all checkboxes to False
        shiny::updateCheckboxInput(
          inputId = "exclude_blanks",
          value = F
        )
        shiny::updateCheckboxInput(
          inputId = "exclude_qcs",
          value = F
        )
        shiny::updateCheckboxInput(
          inputId = "exclude_pools",
          value = F
        )
        

      })
      # Clear button
      shiny::observeEvent(input$clear_filters, {
        
        # Set all checkboxes to False
        shiny::updateCheckboxInput(
          inputId = "exclude_blanks",
          value = F
        )
        shiny::updateCheckboxInput(
          inputId = "exclude_qcs",
          value = F
        )
        shiny::updateCheckboxInput(
          inputId = "exclude_pools",
          value = F
        )
        
        # Set metadata row exclusion to None
        shiny::updateSelectizeInput(
          session = session,
          inputId = "exclusion_meta_row",
          selected = character(0)
        )
        
        # Set manual row exclusion to None
        shiny::updateSelectizeInput(
          session = session,
          inputId = "exclusion_manual",
          selected = character(0)
        )
      })
      
      # Reset button
      shiny::observeEvent(input$reset_table, {
        
        # Reset the filtered metadata to the raw metadata
        r6$set_filtered_meta()
        
        # Update the progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "row_count_bar",
          value = nrow(r6$meta_filtered),
          total = nrow(r6$meta_raw)
        )
        
        # Update the filtered table preview
        output$filtered_table = renderDataTable({
          DT::datatable(r6$meta_filtered, options = list(paging = FALSE))
        })
        
        # Update input for the manual exclusion
        shiny::updateSelectizeInput(
          session = session,
          inputId = "exclusion_manual",
          choices = rownames(r6$meta_filtered)
        )
      })
    }
  )
}


