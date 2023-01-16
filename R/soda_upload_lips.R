library(shiny)
library(bs4Dash)
library(shinyWidgets)

#------------------------------------------------ Lipidomics data upload UI ----
soda_upload_lips_ui = function(id, head = F) {
  
  ns = NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      ############################ UPLOAD TAB ##################################
      title = "Upload",
      shiny::fluidRow(
        
        # First column with the table input and preview
        shiny::column(
          width = 9,
          shiny::h2("Upload lipidomics data"),
          
          # Table preview box
          bs4Dash::box(
            width = 12,
            DT::dataTableOutput(ns("table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
          ),
          
          # Data upload
          shiny::fileInput(inputId = ns("file"), label = NULL, multiple = F, accept = c(".csv"), width = "100%"),
          
          # Text feedback for groups to be analysed
          shiny::span(textOutput(outputId = ns("found_groups")))
        ),
        
        # Second column for data curation
        shiny::column(
          width = 3,
          shiny::tags$h3("Select columns"),
          
          # Display preview or full table
          shiny::checkboxInput(inputId = ns("preview"), label = "Display preview only", value = head),
          
          # Select ID column
          soda_get_col_ui(label = "Sample IDs", desc = "Column containing the sample IDs."),
          shiny::selectInput(inputId = ns("select_id"), choices = NULL, label = NULL, multiple = F),
          shiny::span(textOutput(outputId = ns("id_error")), style="color:red"),
          
          # Select group column
          soda_get_col_ui(label = "Group column", desc = "Metadata column with groups for each sample."),
          shiny::selectInput(inputId = ns("select_sample_group"), choices = NULL, label = NULL, multiple = F),
        )
      )
    ),
    shiny::tabPanel(
      ############################ FILTER TAB ##################################
      title = "Filter",
      shiny::fluidRow(
        # First column displaying the effects of the parameters on the data
        shiny::column(
          width = 9,
          shinyWidgets::progressBar(
            id = ns("col_count_bar"),
            title = "Feature count",
            value = 100,
            total = 100,
            unit_mark = "%"
          ),
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::plotOutput(
                outputId = ns("class_bar_1"),
                height = "500px"
              )
            )
          ),
          shiny::fluidRow(
            actionButton(ns("do"), "Debug"),
            actionButton(ns("reset"), "Reset table"),
            actionButton(ns("save"), "Save filtering")
          )
        ),
        shiny::column(
          width = 3,
          shiny::h4("Feature filtering"),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
          
          # Blank multiplier
          soda_get_col_ui(label ="Blank multiplier", desc = 'Multiplier: feature value for a sample should be above blank_multiplier x blank_mean'),
          shiny::textInput(inputId = ns("blank_multiplier"), label = NULL, value = 2),
          
          # Sample threshold
          soda_get_col_ui(label ="Sample threshold", desc = 'Samples that should be above the blank multiplier for a feature to be kept'),
          shiny::sliderInput(inputId = ns("sample_threshold"), label = NULL, value = 0.8, min = 0, max = 1, step = 0.05),
          
          # Group threshold
          soda_get_col_ui(label ="Group threshold", desc = 'Same as above, but only considering the samples in each group'),
          shiny::sliderInput(inputId = ns("group_threshold"), label = NULL, value = 0.8, min = 0, max = 1, step = 0.05)
          
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
      
      total_rows_raw = shiny::reactiveVal(
        NULL
      )
      
      total_rows_raw= shiny::reactive({
        if (!is.null(r6$data_raw)) {
          nrow(r6$data_raw)
        }
      })
      
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
            choices = colnames(r6$meta_filtered),
            selected = colnames(r6$meta_filtered)[2]
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
      
      # Set values to the R6 object
      shiny::observe({
        if (!is.null(input$select_id)){
          
          # Initialise filtered data with the ID column
          r6$set_col(col = input$select_id, type = "id_data")
          r6$set_filtered_data()
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
      col_filters = shiny::reactiveVal(
        value = NULL
      )
      
      col_filters = shiny::reactive({
        list(input$blank_multiplier, input$sample_threshold, input$group_threshold)
      })
      
      
      # Reactive values for column filtering
      shiny::observeEvent(col_filters(),{
        if (!is.null(r6$data_filtered)){
          
          total_cols = ncol(r6$data_filtered)
          del_cols = blank_filter(data_table = r6$data_filtered,
                                  blank_table = r6$data_raw[r6$get_idx_blanks(),-which(colnames(r6$data_raw) == r6$col_id_data)],
                                  blank_multiplier = as.numeric(input$blank_multiplier),
                                  sample_threshold = input$sample_threshold)
          saved_cols = group_filter(data_table = r6$data_filtered,
                                    blank_table = r6$data_raw[r6$get_idx_blanks(),-which(colnames(r6$data_raw) == r6$col_id_data)],
                                    meta_table = r6$meta_filtered,
                                    del_cols = del_cols,
                                    idx_samples = r6$get_idx_samples(),
                                    col_group = r6$col_group,
                                    blank_multiplier = as.numeric(input$blank_multiplier),
                                    group_threshold = input$group_threshold)
          del_cols = setdiff(del_cols,saved_cols)
          remaining_cols = total_cols - length(del_cols)
          
          output$class_bar_1 = shiny::renderPlot(
            expr = preview_class_plot(r6 = r6,
                                      total_cols = total_cols,
                                      saved_cols = saved_cols,
                                      del_cols = del_cols,
                                      blank_multiplier = input$blank_multiplier,
                                      sample_threshold = input$sample_threshold,
                                      group_threshold = input$group_threshold),
            bg = "transparent"
          )
          
          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = remaining_cols,
            total = total_cols
          )
          

        }
      })

      # Reactive values for sample filtering
      
      # Samples from that value
     
      


      

      
      

      


      
      
      # Reset button
      # shiny::observeEvent(input$reset, {
      #   r6$set_filtered_data()
      #   shinyWidgets::updateProgressBar(
      #     session = session,
      #     id = "row_count_bar",
      #     value = nrow(r6$meta_filtered),
      #     total = total_rows_raw()
      #   )
      # })
      
      # Save button
      shiny::observeEvent(input$save, {
        r6$feature_filter(blank_multiplier = as.numeric(input$blank_multiplier),
                          sample_threshold = input$sample_threshold,
                          group_threshold = input$group_threshold)
      })
      
      # Debugging button
      observeEvent(input$do, {
        print(nrow(r6$meta_filtered))
      })
      
    }
  )
}


