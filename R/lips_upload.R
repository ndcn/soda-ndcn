
lips_update_fields = function(input, session, r6) {
  
  del_cols = lips_get_del_cols(data_table = r6$tables$data_filtered,
                               blank_table = r6$tables$blank_table,
                               meta_table_raw = r6$tables$meta_raw,
                               meta_table_filtered = r6$tables$meta_filtered,
                               idx_blanks = r6$indices$idx_blanks,
                               idx_samples = r6$indices$idx_samples,
                               col_id_meta = r6$texts$col_id_meta,
                               col_group = r6$texts$col_group,
                               col_batch = r6$texts$col_batch,
                               blank_multiplier = as.numeric(input$blank_multiplier),
                               sample_threshold = as.numeric(input$sample_threshold),
                               group_threshold = as.numeric(input$group_threshold)
                               )
  remaining_cols = setdiff(colnames(r6$tables$data_filtered), del_cols)
  
  # Update class selection
  shiny::updateSelectizeInput(
    session = session,
    inputId = "class_selection",
    choices = unique(r6$tables$feat_filtered$lipid_class),
    selected = character(0)
  )
  
  # Update manual selection
  shiny::updateSelectizeInput(
    session = session,
    inputId = "manual_selection",
    choices = remaining_cols,
    selected = character(0)
  )
}

meta_row_selection_lips = function(input, r6) {
  # Initialise selection
  selected_rows = c()
  
  # Get blank rows
  if ("Blanks" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_blanks)
  }
  
  # Get QC rows
  if ("QCs" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_qcs)
  }
  
  # Get Pool rows
  if ("Pools" %in% input$non_samples_selection){
    selected_rows = c(selected_rows, r6$indices$rownames_pools)
  }
  
  # Add metadata and manual exclusions
  selected_rows = c(selected_rows,input$exclusion_meta_row,input$selection_manual)
  selected_rows = sort(unique(selected_rows))
  
  return(selected_rows)
}
meta_reset_fields_lips = function(input, session, r6) {
  # Set all checkboxes to False
  shinyWidgets::updateCheckboxGroupButtons(
    session = session,
    inputId = "non_samples_selection",
    selected = character(0)
  )
  
  # Set manual row selection to None and update
  shiny::updateSelectizeInput(
    session = session,
    inputId = "selection_manual",
    choices = rownames(r6$tables$meta_filtered),
    selected = character(0)
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
soda_upload_lips_ui = function(id, head_meta = F, head_data = T) {
  
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      ##################################################### Upload metadata ####
      title = "Upload metadata",
      shiny::fluidRow(
        
        # First column with the table input and preview of the raw data
        shiny::column(
          width = 9,
          shiny::h2("Upload metadata"),
          
          # Data upload
          shiny::fileInput(inputId = ns("file_meta"), label = NULL, multiple = F, accept = c(".csv", ".tsv", ".txt", ".xlsx"), width = "100%"),
          
          # Table preview box
          bs4Dash::box(
            title = "Sample metadata table (raw)",
            width = 12,
            DT::dataTableOutput(ns("raw_table_meta")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          ),
          
          # Text feedback for blanks, QCs and pools found (text patterns)
          shiny::span(textOutput(outputId = ns("found_groups"))),
          shiny::span(textOutput(outputId = ns("found_blanks"))),
          shiny::span(textOutput(outputId = ns("found_qcs"))),
          shiny::span(textOutput(outputId = ns("found_pools"))),
          shiny::span(textOutput(outputId = ns("found_batches")))
        ),
        
        # Second column for data curation
        shiny::column(
          width = 3,
          shiny::tags$h3("Select columns"),
          
          # Display preview or full table
          shiny::checkboxInput(inputId = ns("preview_meta"), label = "Display preview only", value = head_meta),
          
          # Select ID column
          soda_get_col_ui(label = "Sample IDs", desc = "Column containing the sample IDs."),
          shiny::selectInput(inputId = ns("select_id_meta"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          shiny::span(textOutput(outputId = ns("id_error_meta")), style="color:red"),
          
          # Select sample type column
          soda_get_col_ui(label ="Type column", desc = "Column containing the sample types."),
          shiny::selectInput(inputId = ns("select_sample_type"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          
          # Select group column
          soda_get_col_ui(label = "Group column", desc = "Column containing the sample groups"),
          shiny::selectInput(inputId = ns("select_sample_group"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          
          # Select batch column
          soda_get_col_ui(label = "Batch column", desc = "Column containing the sample batchs"),
          shiny::selectInput(inputId = ns("select_sample_batch"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          
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
      ##################################################### Filter metadata ####
      title = "Filter metadata",
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
          
          shiny::h5("Non-sample selection"),
          
          shinyWidgets::checkboxGroupButtons(
            inputId = ns('non_samples_selection'),
            label = NULL,
            choices = c("Blanks", "QCs", "Pools"),
            selected = c("Blanks", "QCs", "Pools"),
            direction = "horizontal",
            status = "default",
            justified = TRUE,
            width = '100%',
            checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
          ),
          
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
          shiny::h2("Upload lipidomics data"),
          
          # Data upload
          shiny::fileInput(inputId = ns("file_data"), label = NULL, multiple = F, accept = c(".csv", ".tsv", ".txt", ".xlsx"), width = "100%"),
          
          # Table preview box
          bs4Dash::box(
            title = "Lipidomics table (raw)",
            width = 12,
            DT::dataTableOutput(ns("table_data")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
          )
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
          
          shiny::tags$h3("NA imputation"),
          
          # Select ID column
          soda_get_col_ui(label = "Imputation", desc = 'Values which will replace NAs will be this factor multiplied by the minimum value of each batch. "NA" keeps NAs.'),
          shiny::selectizeInput(inputId = ns("na_imputation"),
                                choices = c("NA", "0.00", "0.25", "0.50", "0.75", "1.00"),
                                selected = "0.25",
                                label = NULL,
                                multiple = F,
                                width = "100%"),
        )
      )
    ),
    shiny::tabPanel(
      ######################################################### Filter data ####
      title = "Filter data",
      shiny::fluidRow(
        # First column displaying the effects of the parameters on the data
        shiny::column(
          width = 9,
          shiny::h2("Filter lipidomics data"),
          
          # Declare progress bar
          shinyWidgets::progressBar(
            id = ns("col_count_bar"),
            title = "Feature count",
            value = 100,
            total = 100,
            unit_mark = "%"
          ),
          
          # Declare class barplot
          shiny::fluidRow(
            shiny::column(
              width = 12,
              shiny::plotOutput(
                outputId = ns("class_barplot"),
                height = "500px"
              )
            )
          )
        ),
        
        # Second column displaying the parameters to be used for feature filtering
        shiny::column(
          width = 3,
          shiny::h4("Feature filtering"),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
          
          # Class filter
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectizeInput(inputId = ns("class_selection"), label = "Select classes", choices = NULL, multiple = TRUE, width = "100%")
            ),
            shiny::column(
              width = 6,
              shiny::selectizeInput(inputId = ns("manual_selection"), label = "Select manually", choices = NULL, multiple = TRUE, width = "100%")
            )
          ),
          
          shiny::fluidRow(
            shiny::actionButton(inputId = ns("drop_cols"), label =  "Drop", width = "50%"),
            shiny::actionButton(inputId = ns("keep_cols"), label =  "Keep", width = "50%")
          ),
          
          # Manual filter
          
          shiny::h4("Blank & Group filtering"),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),

          # Blank multiplier
          shiny::textInput(inputId = ns("blank_multiplier"), label = "Blank multiplier", value = 2, width = "100%"),
          
          # Sample threshold
          tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #007bff}")),
          shiny::sliderInput(inputId = ns("sample_threshold"), label = "Sample threshold", value = 0.8, min = 0, max = 1, step = 0.05, width = "100%"),
          
          # Group threshold
          tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #007bff}")),
          shiny::sliderInput(inputId = ns("group_threshold"), label = "Group threshold", value = 0.8, min = 0, max = 1, step = 0.05, width = "100%"),
          
          # Normalisation
          shiny::selectizeInput(inputId = ns("normalise_to_col"),
                                label = "Normalise to column",
                                choices = character(0),
                                width = "100%"),
          
          # Buttons to save or reset the feature filtering
          shiny::fluidRow(
            shiny::actionButton(inputId = ns("save"), label = "Apply filtering", width = "50%"),
            shiny::actionButton(inputId = ns("reset_data"), label =  "Reset", width = "50%")
          ),
          
          # Button to download filtered data
          shiny::h4("Download tables"),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
          shiny::fluidRow(
            shiny::selectizeInput(
              inputId = ns("other_tables_select"),
              label = "Select table:",
              choices = c("Filtered data table",
                          "Class normalised data table", "Total normalised data table",
                          "Raw class table", "Total normalised class table",
                          "Z-scored data table", "Z-scored class normalised data table",
                          "Z-scored total normalised data table",
                          "Z-scored total normalised class table",
                          "Raw feature table",
                          "Filtered feature table",
                          "Group summary species",
                          "Group summary classes"),
              selected = "Filtered data table",
              multiple = FALSE,
              width = "100%"
            ),
            shiny::downloadButton(
              outputId = ns("other_tables_download"),
              label = "Download selected",
              style = "width:100%;"
            )
          )
        )
      )
    )
  )
}

#-------------------------------------------------- Meta data upload server ----

soda_upload_lips_server = function(id, max_rows = 10, max_cols = 8, r6) {
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
            shiny::updateSelectInput(
              session = session,
              inputId = "select_id_meta",
              choices = colnames(r6$tables$meta_raw)
            )
          })
          
          # Select sample type column from the raw meta data
          shiny::updateSelectInput(
            session = session,
            inputId = "select_sample_type",
            choices = colnames(r6$tables$meta_raw),
            selected = colnames(r6$tables$meta_raw)[2]
          )
          
          # Select sample group column from the raw meta data
          shiny::updateSelectInput(
            session = session,
            inputId = "select_sample_group",
            choices = colnames(r6$tables$meta_raw),
            selected = colnames(r6$tables$meta_raw)[3]
          )
          
          # Select sample batch column from the raw meta data
          shiny::updateSelectInput(
            session = session,
            inputId = "select_sample_batch",
            choices = colnames(r6$tables$meta_raw),
            selected = colnames(r6$tables$meta_raw)[4]
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
            
            # Update the column choices in the lipidomics data normalisation widget
            shiny::updateSelectizeInput(
              session = session,
              inputId = "normalise_to_col",
              choices = c("None", colnames(r6$tables$meta_filtered)),
              selected = "None"
            )
            
            # Set columns
            r6$set_col(col = input$select_sample_type, type = "type")
            r6$set_col(col = input$select_sample_group, type = "group")
            r6$set_col(col = input$select_sample_batch, type = "batch")
            r6$get_batch_list()
            
            
            
            # Set parameters
            r6$set_params_class_distribution(input$select_sample_group)
            r6$set_params_class_comparison(input$select_sample_group)
            r6$params$volcano_plot$group_column = input$select_sample_group
            r6$params$pca$group_column = input$select_sample_group
            r6$params$db_plot$group_column = input$select_sample_group
            r6$params$heatmap$group_column_da = input$select_sample_group
            
            # Text patterns
            r6$set_text_pattern(pattern = input$qc_pattern, type = "qc")
            r6$set_text_pattern(pattern = input$blank_pattern, type = "blank")
            r6$set_text_pattern(pattern = input$pool_pattern, type = "pool")
            
            # Get text pattern counts
            if (r6$texts$pattern_blank != "") {
              count_blanks = length(grep(pattern = r6$texts$pattern_blank,
                                         x = r6$tables$meta_raw[,r6$texts$col_type],
                                         ignore.case = TRUE))
            }else{
              count_blanks = 0
            }
            
            if (r6$texts$pattern_qc != "") {
              count_qcs = length(grep(pattern = r6$texts$pattern_qc,
                                      x = r6$tables$meta_raw[,r6$texts$col_type],
                                      ignore.case = TRUE))
            }else{
              count_qcs = 0
            }
            if (r6$texts$pattern_pool != "") {
              count_pools = length(grep(pattern = r6$texts$pattern_pool,
                                        x = r6$tables$meta_raw[,r6$texts$col_type],
                                        ignore.case = TRUE))
            }else{
              count_pools = 0
            }
            if (!is.null(r6$texts$col_batch)) {
              count_batches = length(unique(r6$tables$meta_raw[,r6$texts$col_batch]))
            }else{
              count_batches = 0
            }
            
            
            
            # Get indices
            r6$set_all_indices()

            # Get found groups (upload table)
            unique_groups = unique(r6$tables$meta_filtered[r6$indices$rownames_samples, r6$texts$col_group])
            unique_groups = paste(unique_groups, collapse  = ", ")
                        
            # Update text pattern feedback
            output$found_groups = shiny::renderText({paste0("Groups found: ", unique_groups)})
            output$found_blanks = shiny::renderText({paste0("Blanks found: ", as.character(count_blanks))})
            output$found_qcs = shiny::renderText({paste0("QCs found: ", as.character(count_qcs))})
            output$found_pools = shiny::renderText({paste0("Pools found: ", as.character(count_pools))})
            output$found_batches = shiny::renderText({paste0("Batches found: ", as.character(count_batches))})

          }
        }
      })
      
      ##################################################### Filter metadata ####
      
      # Update the metadata value once a metadata column is selected
      shiny::observeEvent(c(input$exclusion_meta_col),{
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
        
        selected_rows = meta_row_selection_lips(input = input, r6 = r6)
        
        # Drop the data
        if (!is.null(selected_rows)){
          r6$tables$meta_filtered = drop_rows(data_table = r6$tables$meta_filtered,
                                              rows = selected_rows)
        }
        
        r6$get_batch_list()
        
        # If there is already uploaded data_raw, refresh
        if (!is.null(r6$tables$data_raw)) {
          r6$set_data_filtered()
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
        meta_reset_fields_lips(input = input, session = session, r6 = r6)
      })
      
      # Keep button
      shiny::observeEvent(input$selection_keep, {
        
        selected_rows = meta_row_selection_lips(input = input, r6 = r6)
        
        # Keep the data
        if (!is.null(selected_rows)){
          r6$tables$meta_filtered = keep_rows(data_table = r6$tables$meta_filtered,
                                              rows = selected_rows)
        }
        
        r6$get_batch_list()
        
        # If there is already uploaded data_raw, refresh
        if (!is.null(r6$tables$data_raw)) {
          r6$set_data_filtered()
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
        meta_reset_fields_lips(input = input, session = session, r6 = r6)
        
        
      })
      
      # Clear button
      shiny::observeEvent(input$clear_filters, {
        
        # Reset fields
        meta_reset_fields_lips(input = input, session = session, r6 = r6)
      })
      
      # Reset button
      shiny::observeEvent(input$reset_meta, {
        
        # Reset the filtered metadata to the raw metadata
        r6$set_meta_filtered()
        
        r6$get_batch_list()
        
        # If there is already uploaded data_raw, refresh
        if (!is.null(r6$tables$data_raw)) {
          r6$set_data_filtered()
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
        meta_reset_fields_lips(input = input, session = session, r6 = r6)
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
            r6$get_missing_vals()
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
      shiny::observeEvent(c(input$select_id_data, input$na_imputation), {
        if (input$select_id_data != ""){
          # Initialise filtered data with the ID column
          r6$na_imputation_raw(imputation_factor = input$na_imputation)
          r6$set_col(col = input$select_id_data, type = "id_data")
          r6$set_data_filtered()
          
          # Send error message if non-unique IDs are selected
          if (r6$non_unique_ids_data){
            output$id_error_data = shiny::renderText({"Non-uniques in ID column. Please correct or choose another column"})
          } else {
            # if ID correct, filter out deleted rows from the lipids table
            output$id_error_data = shiny::renderText({NULL})
            
            # Initialise preview of the feature filtering
            if (!is.null(r6$tables$data_filtered)) {
              r6$set_blank_table()
              r6$set_feat_raw()
              del_cols = lips_get_del_cols(data_table = r6$tables$data_filtered,
                                           blank_table = r6$tables$blank_table,
                                           meta_table_raw = r6$tables$meta_raw,
                                           meta_table_filtered = r6$tables$meta_filtered,
                                           idx_blanks = r6$indices$idx_blanks,
                                           idx_samples = r6$indices$idx_samples,
                                           col_id_meta = r6$texts$col_id_meta,
                                           col_group = r6$texts$col_group,
                                           col_batch = r6$texts$col_batch,
                                           blank_multiplier = as.numeric(input$blank_multiplier),
                                           sample_threshold = as.numeric(input$sample_threshold),
                                           group_threshold = as.numeric(input$group_threshold)
                                           )
              remaining_cols = setdiff(colnames(r6$tables$data_filtered), del_cols)
              # Initialise bar plot
              output$class_barplot = shiny::renderPlot(
                expr = preview_class_plot(data_table = r6$tables$data_filtered,
                                          del_cols = del_cols,
                                          feat_raw = r6$tables$feat_raw),
                bg = "transparent"
              )
              
              # Initialise progress bar
              shinyWidgets::updateProgressBar(
                session = session,
                id = "col_count_bar",
                value = length(remaining_cols),
                total = ncol(r6$tables$data_raw) - 1
              )
              # Produce ensuing tables
              r6$set_all_tables()
              
              # Update the class filter
              shiny::updateSelectizeInput(
                session = session,
                inputId = "class_selection",
                choices = unique(r6$tables$feat_filtered$lipid_class),
                selected = character(0)
              )
              
              shiny::updateSelectizeInput(
                session = session,
                inputId = "manual_selection",
                choices = remaining_cols,
                selected = character(0)
              )
            }
          }
        }
      })
      
      ######################################################### Filter data ####
      
      # Drop columns
      shiny::observeEvent(input$drop_cols,{
        selected_feats = input$manual_selection
        for (c in input$class_selection) {
          selected_feats = c(selected_feats, rownames(r6$tables$feat_filtered)[r6$tables$feat_filtered[,"lipid_class"] == c])
        }
        
        r6$tables$data_filtered = r6$tables$data_filtered[,!(colnames(r6$tables$data_filtered) %in% selected_feats)]
        
        r6$set_feat_filtered()
        r6$params$db_plot$selected_lipid_class = unique(r6$tables$feat_filtered$lipid_class)[1]
        
        del_cols = lips_get_del_cols(data_table = r6$tables$data_filtered,
                                     blank_table = r6$tables$blank_table,
                                     meta_table_raw = r6$tables$meta_raw,
                                     meta_table_filtered = r6$tables$meta_filtered,
                                     idx_blanks = r6$indices$idx_blanks,
                                     idx_samples = r6$indices$idx_samples,
                                     col_id_meta = r6$texts$col_id_meta,
                                     col_group = r6$texts$col_group,
                                     col_batch = r6$texts$col_batch,
                                     blank_multiplier = as.numeric(input$blank_multiplier),
                                     sample_threshold = as.numeric(input$sample_threshold),
                                     group_threshold = as.numeric(input$group_threshold)
                                     )
        
        # Update class bar plot
        output$class_barplot = shiny::renderPlot(
          expr = preview_class_plot(data_table = r6$tables$data_filtered,
                                    del_cols = del_cols,
                                    feat_raw = r6$tables$feat_raw),
          bg = "transparent"
        )
        
        # Update progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "col_count_bar",
          value = ncol(r6$tables$data_filtered) - length(del_cols),
          total = ncol(r6$tables$data_raw) - 1
        )
        
        # Update selection fields
        lips_update_fields(input = input, session = session, r6 = r6)
        
      })
      
      # Keep columns
      shiny::observeEvent(input$keep_cols,{
        selected_feats = input$manual_selection
        for (c in input$class_selection) {
          selected_feats = c(selected_feats, rownames(r6$tables$feat_filtered)[r6$tables$feat_filtered[,"lipid_class"] == c])
        }
        
        if (is.null(selected_feats)) {
          return()
        }
        
        # Del cols from the selection + apply changes
        del_cols = setdiff(colnames(r6$tables$data_filtered),selected_feats)
        r6$tables$data_filtered = r6$tables$data_filtered[,(colnames(r6$tables$data_filtered) %in% selected_feats)]
        r6$set_feat_filtered()
        r6$params$db_plot$selected_lipid_class = unique(r6$tables$feat_filtered$lipid_class)[1]
        
        # Get del cols from the blank and group filtering
        del_cols = lips_get_del_cols(data_table = r6$tables$data_filtered,
                                     blank_table = r6$tables$blank_table,
                                     meta_table_raw = r6$tables$meta_raw,
                                     meta_table_filtered = r6$tables$meta_filtered,
                                     idx_blanks = r6$indices$idx_blanks,
                                     idx_samples = r6$indices$idx_samples,
                                     col_id_meta = r6$texts$col_id_meta,
                                     col_group = r6$texts$col_group,
                                     col_batch = r6$texts$col_batch,
                                     blank_multiplier = as.numeric(input$blank_multiplier),
                                     sample_threshold = as.numeric(input$sample_threshold),
                                     group_threshold = as.numeric(input$group_threshold)
                                     )
        
        # Update class bar plot
        output$class_barplot = shiny::renderPlot(
          expr = preview_class_plot(data_table = r6$tables$data_filtered,
                                    del_cols = del_cols,
                                    feat_raw = r6$tables$feat_raw),
          bg = "transparent"
        )
        
        # Update progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "col_count_bar",
          value = ncol(r6$tables$data_filtered) - length(del_cols),
          total = ncol(r6$tables$data_raw) - 1
        )
        
        
        
        
        
        # Update selection fields
        lips_update_fields(input = input, session = session, r6 = r6)
        
      })
      
      
      # Display filtering preview
      shiny::observeEvent(c(input$blank_multiplier, input$sample_threshold, input$group_threshold),{
        if (!is.null(r6$tables$data_filtered)){
          
          # Calculate remaining cols
          del_cols = lips_get_del_cols(data_table = r6$tables$data_filtered,
                                       blank_table = r6$tables$blank_table,
                                       meta_table_raw = r6$tables$meta_raw,
                                       meta_table_filtered = r6$tables$meta_filtered,
                                       idx_blanks = r6$indices$idx_blanks,
                                       idx_samples = r6$indices$idx_samples,
                                       col_id_meta = r6$texts$col_id_meta,
                                       col_group = r6$texts$col_group,
                                       col_batch = r6$texts$col_batch,
                                       blank_multiplier = as.numeric(input$blank_multiplier),
                                       sample_threshold = as.numeric(input$sample_threshold),
                                       group_threshold = as.numeric(input$group_threshold)
                                       )
          
          remaining_cols = ncol(r6$tables$data_filtered) - length(del_cols)
          
          # Update class bar plot
          output$class_barplot = shiny::renderPlot(
            expr = preview_class_plot(data_table = r6$tables$data_filtered,
                                      del_cols = del_cols,
                                      feat_raw = r6$tables$feat_raw),
            bg = "transparent"
          )
          
          # Update progress bar
          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = remaining_cols,
            total = ncol(r6$tables$data_raw) - 1
          )
          
          # Update selection fields
          lips_update_fields(input = input, session = session, r6 = r6)
        }
      })
      
      # Save button
      shiny::observeEvent(input$save, {
        
        # Apply filtering to the filtered table
        r6$feature_filter(blank_multiplier = as.numeric(input$blank_multiplier),
                          sample_threshold = input$sample_threshold,
                          group_threshold = input$group_threshold
                          )
        
        # Normalise to metacolumn
        r6$normalise_to_metadata(meta_col = input$normalise_to_col)
        
        # Update progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "col_count_bar",
          value = ncol(r6$tables$data_filtered),
          total = ncol(r6$tables$data_filtered)
        )
        
        # Produce ensuing tables
        r6$set_all_tables()
      })
      
      shiny::observeEvent(input$reset_data, {
        
        # Reset table
        r6$set_data_filtered()
        r6$set_all_tables()
        del_cols = lips_get_del_cols(data_table = r6$tables$data_filtered,
                                     blank_table = r6$tables$blank_table,
                                     meta_table_raw = r6$tables$meta_raw,
                                     meta_table_filtered = r6$tables$meta_filtered,
                                     idx_blanks = r6$indices$idx_blanks,
                                     idx_samples = r6$indices$idx_samples,
                                     col_id_meta = r6$texts$col_id_meta,
                                     col_group = r6$texts$col_group,
                                     col_batch = r6$texts$col_batch,
                                     blank_multiplier = as.numeric(input$blank_multiplier),
                                     sample_threshold = as.numeric(input$sample_threshold),
                                     group_threshold = as.numeric(input$group_threshold)
                                     )
        remaining_cols = ncol(r6$tables$data_filtered) - length(del_cols)
        
        # Update class bar plot
        output$class_barplot = shiny::renderPlot(
          expr = preview_class_plot(data_table = r6$tables$data_filtered,
                                    del_cols = del_cols,
                                    feat_raw = r6$tables$feat_raw),
          bg = "transparent"
        )
        
        # Update progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "col_count_bar",
          value = remaining_cols,
          total = ncol(r6$tables$data_raw) - 1
        )
        
        # Update selection fields
        lips_update_fields(input = input, session = session, r6 = r6)
        
      })
      
      
      
      # Download filtered data
      dl_other_table = shiny::reactiveValues(
        name = NULL,
        table = NULL
      )
      shiny::observeEvent(c(input$other_tables_select, input$save) , {
        dl_other_table$name = timestamped_name(paste0(stringr::str_replace_all(input$other_tables_select, " ", "_"), ".csv"))
        dl_other_table$table = table_switch(selection = input$other_tables_select, r6 = r6)
      })
      
      # Download other tables
      output$other_tables_download = shiny::downloadHandler(
        filename = shiny::reactive(dl_other_table$name),
        content = function(file_name) {
          write.csv(dl_other_table$table, file_name)
        }
      )
      
      
      
    }
  )
}

