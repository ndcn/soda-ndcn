
meta_row_selection_prot = function(input, r6) {
  # Initialise selection
  selected_rows = c()
  
  # Add metadata and manual exclusions
  selected_rows = c(selected_rows,input$exclusion_meta_row,input$selection_manual)
  selected_rows = sort(unique(selected_rows))
  
  return(selected_rows)
}
meta_reset_fields_prot = function(input, session, r6) {

  # Set manual row selection to None and update
  shiny::updateSelectizeInput(
    session = session,
    inputId = "selection_manual",
    choices = rownames(r6$tables$meta_filtered),
    selected = character(0),
    server = TRUE
  )
  
  
  # Set the metacolumn value to None and update
  shiny::updateSelectInput(
    session = session,
    inputId = "exclusion_meta_val",
    choices = unique(r6$tables$meta_filtered[,input$exclusion_meta_col]),
    selected = character(0)
  )
  
  # Set metadata row exclusion to None
  shiny::updateSelectizeInput(
    session = session,
    inputId = "exclusion_meta_row",
    selected = character(0)
  )
}

#------------------------------------------------------ Meta data upload UI ----
soda_upload_prot_ui = function(id, head_meta = F, head_data = T) {
  
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      ##################################################### Upload metadata ####
      title = "Sample metadata",
      shiny::fluidRow(
        
        # First column with the table input and preview of the raw data
        shiny::column(
          width = 9,
          shiny::h2("Upload sample metadata"),
          
          # Data upload
          shiny::fileInput(inputId = ns("file_meta"), label = NULL, multiple = F, accept = c(".csv", ".tsv", ".txt", ".xlsx"), width = "100%"),
          
          # Table preview box
          bs4Dash::box(
            title = "Sample metadata table (raw)",
            width = 12,
            DT::dataTableOutput(ns("raw_table_meta")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          ),
          # Text feedback for groups to be analysed
          shiny::span(textOutput(outputId = ns("found_groups")))
        ),
        
        # Second column for data curation
        shiny::column(
          width = 3,
          shiny::tags$h3("Select columns"),
          
          # Display preview or full table
          shiny::checkboxInput(inputId = ns("preview_meta"), label = "Display preview only", value = head_meta),
          
          # Select ID column
          soda_get_col_ui(label = "Sample IDs", desc = "Column containing the sample IDs."),
          shiny::selectizeInput(inputId = ns("select_id_meta"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          shiny::span(textOutput(outputId = ns("id_error_meta")), style="color:red"),
          
          # Select group column
          soda_get_col_ui(label = "Group column", desc = "Column containing the sample groups"),
          shiny::selectInput(inputId = ns("select_sample_group"), choices = NULL, label = NULL, multiple = F, width = "100%")
        )
      )
    ),
    shiny::tabPanel(
      ##################################################### Filter metadata ####
      title = "Filter samples",
      shiny::fluidRow(
        
        # First column with the table input and preview of the filtered data
        shiny::column(
          width = 9,
          
          # Filtered table preview
          shiny::h2("Filter samples"),
          bs4Dash::box(
            title = "Sample metadata table (filtered)",
            width = 12,
            DT::dataTableOutput(ns("filtered_table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          ),
          
          # Progress bar for the row count
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
          
          # Manual sample exclusion (selection from rows in the filtered metadata table)
          shiny::h5("Manual sample selection"),
          shiny::selectizeInput(
            inputId = ns("selection_manual"), choices = NULL, label = NULL, multiple = T, width = "100%"
          ),
          
          # Exclusion based on a metadata column value
          shiny::h5("Metadata selection"),
          shiny::h6("Select samples based on metadata values"),
          
          shiny::fluidRow(
            shiny::column(
              width = 6,
              # Metadata column selection
              shiny::selectInput(inputId = ns("exclusion_meta_col"), choices = NULL, label = "Column", multiple = F, width = "100%")
            ),
            shiny::column(
              width = 6,
              # Value in the metadata column
              shiny::selectizeInput(inputId = ns("exclusion_meta_val"), choices = NULL, label = "Value", multiple = T, width = "100%")
              
            )
          ),
          
          # Rows to exclude
          shiny::selectizeInput(inputId = ns("exclusion_meta_row"), choices = NULL, label = "Samples", multiple = T, width = "100%"),
          
          # Action buttons to apply filters, clear filters or reset filtered metadata
          shiny::fluidRow(
            shiny::actionButton(
              inputId = ns("selection_drop"),
              label = "Drop",
              width = "25%"
            ),
            shiny::actionButton(
              inputId = ns("selection_keep"),
              label = "Keep",
              width = "25%"
            ),
            shiny::actionButton(
              inputId = ns("clear_filters"),
              label = "Clear filters",
              width = "25%"
            ),
            shiny::actionButton(
              inputId = ns("reset_meta"),
              label = "Reset table",
              width = "25%"
            )
          ),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
          shiny::downloadButton(
            outputId = ns("meta_filtered_download"),
            label = "Download filtered metadata",
            style = "width:100%;"
          )
        )
      )
    ),
    shiny::tabPanel(
      ######################################################### Upload data ####
      title = "Upload data",
      shiny::fluidRow(
        
        # First column with the table input and preview
        shiny::column(
          width = 9,
          shiny::h2("Upload proteomics data"),
          
          # Data upload
          shiny::fileInput(inputId = ns("file_data"), label = NULL, multiple = F, accept = c(".csv", ".tsv", ".txt", ".xlsx"), width = "100%"),
          
          # Table preview box
          bs4Dash::box(
            title = "Data table (raw)",
            width = 12,
            DT::dataTableOutput(ns("table_data")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          ),
          

        ),
        
        # Second column for data curation
        shiny::column(
          width = 3,
          shiny::tags$h3("Select columns"),
          
          # Display preview or full table
          shiny::checkboxInput(inputId = ns("preview_data"), label = "Display preview only", value = head_data),
          
          # Select ID column
          soda_get_col_ui(label = "Sample IDs", desc = "Column containing the sample IDs."),
          shiny::selectizeInput(inputId = ns("select_id_data"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          shiny::span(textOutput(outputId = ns("id_error_data")), style="color:red"),
          shiny::selectInput(inputId = ns("feature_type"),
                             label = "Feature ID type",
                             choices = c("UNIPROT", "SYMBOL", "ENTREZID"),
                             selected = "UNIPROT")
          
        )
      )
    )
  )
}

#-------------------------------------------------- Meta data upload server ----

soda_upload_prot_server = function(id, max_rows = 10, max_cols = 8, r6) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      ##################################################### Upload metadata ####
      
      # File name to upload
      table_file_meta = reactive({
        validate(need(input$file_meta, message = FALSE))
        input$file_meta
      })
      
      # Load data as raw metadata
      shiny::observe({
        if (!is.null(table_file_meta()$datapath)){
          
          if (stringr::str_sub(table_file_meta()$datapath, -5, -1) == ".xlsx") {
            r6$set_raw_meta(as.data.frame(readxl::read_xlsx(table_file_meta()$datapath)))
          } else {
            sep = find_delim(path = table_file_meta()$datapath)
            r6$set_raw_meta(read.csv(table_file_meta()$datapath,
                                     header = T,
                                     sep = sep,
                                     check.names = FALSE))
          }
          
          r6$tables$meta_raw[is.na(r6$tables$meta_raw)] = "missing"
          r6$tables$meta_raw[r6$tables$meta_raw == ""] = "missing"
          
          # Select ID column from the raw meta data
          observe({
            shiny::updateSelectizeInput(
              session = session,
              inputId = "select_id_meta",
              choices = colnames(r6$tables$meta_raw),
              server = TRUE
            )
          })
          
          # Select sample group column from the raw meta data
          shiny::updateSelectInput(
            session = session,
            inputId = "select_sample_group",
            choices = colnames(r6$tables$meta_raw),
            selected = colnames(r6$tables$meta_raw)[3]
          )
        }
      })
      
      # Output a preview or the whole table depending on the user input
      shiny::observe({
        if (!is.null(table_file_meta()$datapath)) {
          if (input$preview_meta){
            output$raw_table_meta = renderDataTable({
              DT::datatable(r6$tables$meta_raw[1:min(max_rows, nrow(r6$tables$meta_raw)),1:min(max_cols, ncol(r6$tables$meta_raw))], options = list(paging = FALSE))
            })
          }else{
            output$raw_table_meta = renderDataTable({
              DT::datatable(r6$tables$meta_raw, options = list(paging = FALSE))
            })
          }
        }
      })
      
      # Set values to the R6 object
      shiny::observe({
        if (!is.null(input$select_id_meta)){
          if (input$select_id_meta != ""){
            
            # Initialise filtered metadata with the ID column
            r6$set_col(col = input$select_id_meta, type = "id_meta")
            r6$set_meta_filtered()
            
            # Check if valid IDs or not
            if (r6$non_unique_ids_meta){
              output$id_error_meta = shiny::renderText({"Non-uniques in ID column. Please correct or choose another column"})
              output$filtered_table = renderDataTable({NULL})
              
            } else {
              output$id_error_meta = shiny::renderText({NULL})
              output$filtered_table = renderDataTable({
                DT::datatable(r6$tables$meta_filtered, options = list(paging = FALSE))
              })
              
              # Update progress bar with row count (filtering tab)
              shinyWidgets::updateProgressBar(
                session = session,
                id = "row_count_bar",
                value = nrow(r6$tables$meta_filtered),
                total = nrow(r6$tables$meta_raw)
              )
              # Update metadata column input (filtering tab)
              shiny::updateSelectInput(
                session = session,
                inputId = "exclusion_meta_col",
                choices = colnames(r6$tables$meta_filtered)
              )
              
              # Update input for the manual exclusion (filtering tab)
              shiny::updateSelectizeInput(
                session = session,
                inputId = "selection_manual",
                choices = rownames(r6$tables$meta_filtered)
              )
            }
            
            # Set columns
            r6$set_col(col = input$select_sample_group, type = "group")
            
            # Get found groups (upload table)
            unique_groups = unique(r6$tables$meta_filtered[, r6$texts$col_group])
            unique_groups = paste(unique_groups, collapse  = ", ")
            
            # Display found groups (upload table)
            output$found_groups = shiny::renderText({paste0("Groups found: ", unique_groups)})
            
            # Set parameters
            r6$params$volcano_plot$group_column = input$select_sample_group
            r6$params$pca$group_column = input$select_sample_group
            r6$params$heatmap$group_column_da = input$select_sample_group
          }
        }
      })
      
      ##################################################### Filter metadata ####
      
      # Update the metadata value once a metadata column is selected
      shiny::observeEvent(input$exclusion_meta_col,{
        # Reset the metacolumn value to None after filtering
        shiny::updateSelectInput(
          session = session,
          inputId = "exclusion_meta_val",
          choices = unique(r6$tables$meta_filtered[,input$exclusion_meta_col]),
          selected = character(0)
        )
      })
      
      # Update the rows to filter once a metadata value is selected
      shiny::observe({
        if (!is.null(input$exclusion_meta_val)) {
          bool_vector = c()
          for (value in input$exclusion_meta_val) {
            bool_vector[[length(bool_vector) + 1]] = r6$tables$meta_filtered[,input$exclusion_meta_col] == value
          }
          bool_vector = Reduce("|", bool_vector)
          
          shiny::updateSelectizeInput(
            session = session,
            inputId = "exclusion_meta_row",
            choices = rownames(r6$tables$meta_filtered)[bool_vector],
            selected = rownames(r6$tables$meta_filtered)[bool_vector]
          )
        }
      })
      
      # Drop button
      shiny::observeEvent(input$selection_drop, {
        
        selected_rows = meta_row_selection_prot(input = input, r6 = r6)
        
        # Drop the data
        if (!is.null(selected_rows)){
          r6$tables$meta_filtered = drop_rows(data_table = r6$tables$meta_filtered,
                                              rows = selected_rows)
        }
        
        # If there is already uploaded data_raw, refresh
        if (!is.null(r6$tables$data_raw)) {
          r6$set_data_filtered()
          r6$normalise_total()
          r6$normalise_z_score()
          r6$normalise_total_z_score()
        }
        
        # Update feedback on the row count progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "row_count_bar",
          value = nrow(r6$tables$meta_filtered),
          total = nrow(r6$tables$meta_raw)
        )
        
        
        # Update feedback on the filtered metadata preview
        output$filtered_table = renderDataTable({
          DT::datatable(r6$tables$meta_filtered, options = list(paging = FALSE))
        })
        
        # Reset fields
        meta_reset_fields_prot(input = input, session = session, r6 = r6)
      })
      
      # Keep button
      shiny::observeEvent(input$selection_keep, {
        
        selected_rows = meta_row_selection_prot(input = input, r6 = r6)
        
        # Keep the data
        if (!is.null(selected_rows)){
          r6$tables$meta_filtered = keep_rows(data_table = r6$tables$meta_filtered,
                                              rows = selected_rows)
        }
        
        # If there is already uploaded data_raw, refresh
        if (!is.null(r6$tables$data_raw)) {
          r6$set_data_filtered()
          r6$normalise_total()
          r6$normalise_z_score()
          r6$normalise_total_z_score()
        }
        
        # Update feedback on the row count progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "row_count_bar",
          value = nrow(r6$tables$meta_filtered),
          total = nrow(r6$tables$meta_raw)
        )
        
        # Update feedback on the filtered metadata preview
        output$filtered_table = renderDataTable({
          DT::datatable(r6$tables$meta_filtered, options = list(paging = FALSE))
        })
        
        # Reset fields
        meta_reset_fields_prot(input = input, session = session, r6 = r6)
        
        
      })
      
      # Clear button
      shiny::observeEvent(input$clear_filters, {
        
        # Reset fields
        meta_reset_fields_prot(input = input, session = session, r6 = r6)
      })
      
      # Reset button
      shiny::observeEvent(input$reset_meta, {
        
        # Reset the filtered metadata to the raw metadata
        r6$set_meta_filtered()
        
        # If there is already uploaded data_raw, refresh
        if (!is.null(r6$tables$data_raw)) {
          r6$set_data_filtered()
          r6$normalise_total()
          r6$normalise_z_score()
          r6$normalise_total_z_score()
        }
        
        # Update the progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "row_count_bar",
          value = nrow(r6$tables$meta_filtered),
          total = nrow(r6$tables$meta_raw)
        )
        
        # Update the filtered table preview
        output$filtered_table = renderDataTable({
          DT::datatable(r6$tables$meta_filtered, options = list(paging = FALSE))
        })
        
        # Reset fields
        meta_reset_fields_prot(input = input, session = session, r6 = r6)
      })
      
      # Download filtered metadata
      output$meta_filtered_download = shiny::downloadHandler(
        filename = function(){timestamped_name("metadata_filtered.csv")},
        content = function(file_name){
          write.csv(r6$tables$meta_filtered, file_name)
        }
      )
      
      ######################################################### Upload data ####
      
      # File upload
      table_file_data = reactive({
        validate(need(input$file_data, message = FALSE))
        input$file_data
      })
      
      # The user's data, parsed into a data frame
      shiny::observe({
        if (!is.null(table_file_data()$datapath)){
          
          if (stringr::str_sub(table_file_data()$datapath, -5, -1) == ".xlsx") {
            r6$set_raw_data(as.data.frame(readxl::read_xlsx(table_file_data()$datapath)))
          } else {
            sep = find_delim(path = table_file_data()$datapath)
            r6$set_raw_data(read.csv(table_file_data()$datapath,
                                     header = T,
                                     sep = sep,
                                     check.names = FALSE))
          }
          # Select ID column from the raw data
          observe({
            shiny::updateSelectizeInput(
              session = session,
              inputId = "select_id_data",
              choices = colnames(r6$tables$data_raw),
              server = TRUE
            )
          })
        }
      })
      
      shiny::observeEvent(input$feature_type, {
        print_time(paste0("Setting feature ID type to ", input$feature_type))
        r6$params$global$feature_id_type = input$feature_type
      })
      
      # Output a preview or the whole table depending on the user input
      shiny::observe({
        if (!is.null(table_file_data()$datapath)) {
          if (input$preview_data){
            output$table_data = renderDataTable({
              DT::datatable(r6$tables$data_raw[1:min(max_rows, nrow(r6$tables$data_raw)),1:min(max_cols, ncol(r6$tables$data_raw))], options = list(paging = FALSE))
            })
          }else{
            output$table_data = renderDataTable({
              DT::datatable(r6$tables$data_raw, options = list(paging = FALSE))
            })
          }
        }
      })
      
      # Set values to the R6 object
      shiny::observeEvent(input$select_id_data, {
        if (input$select_id_data != ""){
          
          # Initialise filtered data with the ID column
          r6$set_col(col = input$select_id_data, type = "id_data")
          r6$set_data_filtered()
          r6$normalise_total()
          r6$normalise_z_score()
          r6$normalise_total_z_score()
          
          # Send error message if non-unique IDs are selected
          if (r6$non_unique_ids_data){
            output$id_error_data = shiny::renderText({"Non-uniques in ID column. Please correct or choose another column"})
          } else {
            
            # if ID correct, filter out deleted rows from the lipids table
            output$id_error_data = shiny::renderText({NULL})
            
            # Initialise preview of the feature filtering
            if (!is.null(r6$tables$data_filtered)) {
              
              # Initialise progress bar
              shinyWidgets::updateProgressBar(
                session = session,
                id = "col_count_bar",
                value = ncol(r6$tables$data_filtered),
                total = ncol(r6$tables$data_raw) - 1
              )
            }
          }
        }
      })
    }
  )
}

