library(shiny)
library(shinydashboard)

#------------------------------------------------ Lipidomics data upload UI ----
soda_upload_lips_ui = function(id, head = F) {
  
  ns = NS(id)
  shiny::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      title = "Upload",
      shiny::tagList(
        # First column with the table input and preview
        shiny::column(
          width = 8,
          shiny::h2("Upload lipidomics data"),
          shiny::fileInput(inputId = ns("file"), label = NULL, multiple = F, accept = c(".csv"), width = "100%"),
          shinydashboard::box(
            width = 12,
            DT::dataTableOutput(ns("table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
          ),
          shiny::span(textOutput(outputId = ns("found_groups")))
        ),
        
        # Second column for data curation
        shiny::column(
          width = 4,
          shiny::tags$h3("Select columns"),
          
          # Display preview or full table
          shiny::checkboxInput(inputId = ns("preview"), label = "Display preview only", value = head),
          
          # Select ID column
          soda_get_col_ui(label = "Sample IDs", desc = "Column containing the sample IDs."),
          shiny::selectInput(inputId = ns("select_id"), choices = NULL, label = NULL, multiple = F),
          
          # Select group column
          soda_get_col_ui(label = "Group column", desc = "Metadata column with groups for each sample."),
          shiny::selectInput(inputId = ns("select_sample_group"), choices = NULL, label = NULL, multiple = F),
        )
      )
    ),
    shiny::tabPanel(
      title = "Filter",
      shiny::tagList(
        # First column displaying the effects of the parameters on the data
        shiny::column(
          width = 4,
          shiny::h2("Filtered data"),
          actionButton(ns("do"), "Click Me"),
          shiny::span("Row count:"),
          shiny::span(textOutput(outputId = ns("row_count"))),
          shiny::span("Column count:"),
          shiny::span(textOutput(outputId = ns("col_count"))),
          shiny::br(),
          shiny::fluidRow(
            actionButton(ns("reset"), "Reset table"),
            actionButton(ns("save"), "Save filtering")
          )
        ),
        shiny::column(
          width = 4,
          shiny::h4("Feature filtering"),
          
          # Blank multiplier
          soda_get_col_ui(label ="Blank multiplier", desc = 'Multiplier: feature value for a sample should be above blank_multiplier x blank_mean'),
          shiny::textInput(inputId = ns("blank_multiplier"), label = NULL, value = 2),
          
          # Sample threshold
          soda_get_col_ui(label ="Sample threshold", desc = 'Samples that should be above the blank multiplier for a feature to be kept'),
          shiny::sliderInput(inputId = ns("sample_threshold"), label = NULL, value = 0.8, min = 0, max = 1, step = 0.05),
          
          # Group threshold
          soda_get_col_ui(label ="Group threshold", desc = 'Same as above, but only considering the samples in each group'),
          shiny::sliderInput(inputId = ns("group_threshold"), label = NULL, value = 0.8, min = 0, max = 1, step = 0.05)
          
        ),
        shiny::column(
          width = 4,
          shiny::h4("Sample filtering")
        )
      )
    )
  )
}

#-------------------------------------------- Lipidomics data upload server ----
soda_upload_lips_server = function(id, max_rows = 10, max_cols = 8, r6 = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ## Upload tab
      
      # The selected file, if any
      userFile = reactive({
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      shiny::observe({
        if (!is.null(userFile()$datapath)){
          r6$set_raw_data(read.csv(userFile()$datapath,
                               header = T,
                               sep = ",",
                               check.names = FALSE))
          # Select ID column from the raw data
          observe({
            shiny::updateSelectInput(
              session = session,
              inputId = "select_id",
              choices = colnames(r6$data_raw)
            )
          })
          # Select sample group column from the raw meta data
          shiny::updateSelectInput(
            session = session,
            inputId = "select_sample_group",
            choices = colnames(r6$meta_raw)
          )
        }
      })
      
      # Output a preview or the whole table depending on the user input
      shiny::observe({
        if (!is.null(userFile()$datapath)) {
          if (input$preview){
            output$table = renderDataTable({
              DT::datatable(r6$data_raw[1:min(max_rows, nrow(r6$data_raw)),1:min(max_cols, ncol(r6$data_raw))], options = list(paging = FALSE))
            })
          }else{
            output$table = renderDataTable({
              DT::datatable(r6$data_raw, options = list(paging = FALSE))
            })
          }
        }
      })
      
      shiny::observe({
        if (!is.null(input$select_id)){
          
          # Initialise filtered data with the ID column
          r6$set_filtered_data(id_col = input$select_id)
          if (r6$non_unique_ids_data){
            output$id_error = shiny::renderText({"Non-uniques in ID column. Please correct or choose another column"})
          } else {
            output$id_error = shiny::renderText({NULL})
          }
          
          # Set columns
          r6$set_col(col = input$select_sample_group, type = "group")
          
          # Get found groups
          unique_groups = unique(r6$meta_filtered[r6$get_idx_samples(), r6$col_group])
          unique_groups = paste(unique_groups, collapse  = ", ")
          
          # Display found groups
          output$found_groups = shiny::renderText({paste0("Groups found: ", unique_groups)})
          
        }
      })
      
      ## Filter tab
      # Declare reactive values for filtering
      del_cols = shiny::reactiveVal(
        value = NULL
      )
      saved_cols = shiny::reactiveVal(
        value = NULL
      )
      remaining_cols = shiny::reactiveVal(
        value = NULL
      )

      # Make these value reactive to the UI
      del_cols = shiny::reactive({
        if (!is.null(r6$data_filtered)){
          blank_filter(data_table = r6$data_filtered,
                       idx_blanks = r6$get_idx_blanks(),
                       blank_multiplier = as.numeric(input$blank_multiplier),
                       sample_threshold = input$sample_threshold)
        }
      })
      saved_cols = shiny::reactive({
        if (!is.null(r6$data_filtered)){
          group_filter(data_table = r6$data_filtered,
                       meta_table = r6$meta_filtered,
                       del_cols = del_cols(),
                       idx_samples = r6$get_idx_samples(),
                       idx_blanks = r6$get_idx_blanks(),
                       col_group = r6$col_group,
                       blank_multiplier = as.numeric(input$blank_multiplier),
                       group_threshold = input$group_threshold)
        }
      })
      
      remaining_cols = shiny::reactive(({
        if (!is.null(r6$data_filtered)){
          ncol(r6$data_filtered) - length(del_cols()) + length(saved_cols())
          }
      }))
      
      # Render text
      if (!is.null(del_cols)) {
        output$row_count = shiny::renderText({nrow(r6$data_filtered)})
      }
      
      if (!is.null(del_cols)) {
        output$col_count = shiny::renderText({
          paste0(remaining_cols(),
                 " out of ",
                 ncol(r6$data_filtered),
                 " (",
                 round(100*(remaining_cols()/ncol(r6$data_filtered)),2),
                 "%)")
          })
      }


      # Reset button
      shiny::observeEvent(input$reset, {
        r6$set_filtered_data(id_col = input$select_id)
        output$col_count = shiny::renderText({
          paste0(remaining_cols(),
                 " out of ",
                 ncol(r6$data_filtered),
                 " (",
                 round(100*(remaining_cols()/ncol(r6$data_filtered)),2),
                 "%)")
        })
      })
      
      # Save button
      shiny::observeEvent(input$save, {
        r6$feature_filter(blank_multiplier = as.numeric(input$blank_multiplier),
                          sample_threshold = input$sample_threshold,
                          group_threshold = input$group_threshold)
        output$col_count = shiny::renderText({
          paste0(remaining_cols(),
                 " out of ",
                 ncol(r6$data_filtered),
                 " (",
                 round(100*(remaining_cols()/ncol(r6$data_filtered)),2),
                 "%)")
        })
      })

      
      # Debugging button
      observeEvent(input$do, {
        print(paste0("Total features: ", ncol(r6$data_filtered)))
        print(paste0("Remaining features: ", length(r6$data_filtered) - length(del_cols()) + length(saved_cols())))
        print(remaining_cols()/ncol(r6$data_filtered))
        print(ncol(r6$data_filtered))
      })
      
    }
  )
}


