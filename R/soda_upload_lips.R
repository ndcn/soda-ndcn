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
            title = "Lipidomics table (raw)",
            width = 12,
            DT::dataTableOutput(ns("table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;",
            collapsible = FALSE
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
          shiny::selectInput(inputId = ns("select_id"), choices = NULL, label = NULL, multiple = F, width = "100%"),
          shiny::span(textOutput(outputId = ns("id_error")), style="color:red"),
          
          # Select group column
          soda_get_col_ui(label = "Group column", desc = "Metadata column with groups for each sample."),
          shiny::selectInput(inputId = ns("select_sample_group"), choices = NULL, label = NULL, multiple = F, width = "100%")
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
            shiny::actionButton(inputId = ns("drop_cols"), label =  "Drop", width = "33.33%"),
            shiny::actionButton(inputId = ns("keep_cols"), label =  "Keep", width = "33.33%"),
            shiny::actionButton(inputId = ns("reset_cols"), label =  "Reset", width = "33.33%")
          ),
          
          # Manual filter
          
          shiny::h4("Blank & Group filtering"),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
          
          # Blank multiplier
          shiny::textInput(inputId = ns("blank_multiplier"), label = "Blank multiplier", value = 2, width = "100%"),
          
          # Sample threshold
          shiny::sliderInput(inputId = ns("sample_threshold"), label = "Sample threshold", value = 0.8, min = 0, max = 1, step = 0.05, width = "100%"),
          
          # Group threshold
          shiny::sliderInput(inputId = ns("group_threshold"), label = "Group threshold", value = 0.8, min = 0, max = 1, step = 0.05, width = "100%"),
          
          # Button to save the feature filtering
          shiny::actionButton(inputId = ns("save"), label = "Save filtering", width = "100%"),
          
          # Button to download filtered data
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
          shiny::downloadButton(
            outputId = ns("data_filtered_download"),
            label = "Download filtered data",
            style = "width:100%;"
          )
        )
      )
    )
  )
}

#-------------------------------------------- Lipidomics data upload server ----
soda_upload_lips_server = function(id, max_rows = 10, max_cols = 8, r6) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      

      ############################ UPLOAD TAB ##################################
      
      # File upload
      table_file = reactive({
        validate(need(input$file, message = FALSE))
        input$file
      })
      
      # The user's data, parsed into a data frame
      shiny::observe({
        if (!is.null(table_file()$datapath)){
          r6$set_raw_data(read.csv(table_file()$datapath,
                               header = T,
                               sep = ",",
                               check.names = FALSE))
          # Select ID column from the raw data
          observe({
            shiny::updateSelectInput(
              session = session,
              inputId = "select_id",
              choices = colnames(r6$tables$data_raw)
            )
          })
          # Select sample group column from the raw meta data
          shiny::updateSelectInput(
            session = session,
            inputId = "select_sample_group",
            choices = colnames(r6$tables$meta_filtered),
            selected = colnames(r6$tables$meta_filtered)[2]
          )
        }
      })
      
      # Output a preview or the whole table depending on the user input
      shiny::observe({
        if (!is.null(table_file()$datapath)) {
          if (input$preview){
            output$table = renderDataTable({
              DT::datatable(r6$tables$data_raw[1:min(max_rows, nrow(r6$tables$data_raw)),1:min(max_cols, ncol(r6$tables$data_raw))], options = list(paging = FALSE))
            })
          }else{
            output$table = renderDataTable({
              DT::datatable(r6$tables$data_raw, options = list(paging = FALSE))
            })
          }
        }
      })
      
      # Set values to the R6 object
      shiny::observe({
        if (!is.null(input$select_id)){

          # Initialise filtered data with the ID column
          r6$set_col(col = input$select_id, type = "id_data")
          r6$set_col(col = input$select_sample_group, type = "group")
          r6$set_data_filtered()

          # Send error message if non-unique IDs are selected
          if (r6$non_unique_ids_data){
            output$id_error = shiny::renderText({"Non-uniques in ID column. Please correct or choose another column"})
          } else {
            # if ID correct, filter out deleted rows from the lipids table
            output$id_error = shiny::renderText({NULL})
            r6$tables$data_filtered = r6$tables$data_filtered[rownames(r6$tables$meta_filtered),]

            # Initialise preview of the feature filtering
            if (!is.null(r6$tables$data_filtered)) {
              total_cols = ncol(r6$tables$data_filtered)
              del_cols = blank_filter(data_table = r6$tables$data_filtered,
                                      blank_table = r6$tables$data_raw[r6$get_idx_blanks(table = r6$tables$meta_raw),-which(colnames(r6$tables$data_raw) == r6$col_id_data)],
                                      blank_multiplier = as.numeric(input$blank_multiplier),
                                      sample_threshold = input$sample_threshold)

              saved_cols = group_filter(data_table = r6$tables$data_filtered,
                                        blank_table = r6$tables$data_raw[r6$get_idx_blanks(table = r6$tables$meta_raw),-which(colnames(r6$tables$data_raw) == r6$col_id_data)],
                                        meta_table = r6$tables$meta_filtered,
                                        del_cols = del_cols,
                                        col_group = r6$col_group,
                                        blank_multiplier = as.numeric(input$blank_multiplier),
                                        group_threshold = input$group_threshold)


              del_cols = setdiff(del_cols,saved_cols)
              remaining_cols = total_cols - length(del_cols)

              # Initialise bar plot
              output$class_barplot = shiny::renderPlot(
                expr = preview_class_plot(r6 = r6,
                                          del_cols = del_cols),
                bg = "transparent"
              )

              # Initialise progress bar
              shinyWidgets::updateProgressBar(
                session = session,
                id = "col_count_bar",
                value = remaining_cols,
                total = total_cols
              )

              ## Produce ensuing tables

              # Normalisation
              r6$normalise_z_score()
              r6$normalise_class()
              r6$normalise_total()
              r6$normalise_class_z_score()
              r6$normalise_total_z_score()
              
              # Create feature table
              r6$set_feat_filtered()

              # Class table
              r6$class_grouping()
              r6$normalise_class_table_z_score()
              
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
                choices = colnames(r6$tables$data_filtered),
                selected = character(0)
              )

            }
          }

          # Get found groups (upload table)
          unique_groups = unique(r6$tables$meta_filtered[r6$get_idx_samples(), r6$col_group])
          unique_groups = paste(unique_groups, collapse  = ", ")

          # Display found groups (upload table)
          output$found_groups = shiny::renderText({paste0("Groups found: ", unique_groups)})
        }
      })
      
      ############################ FILTER TAB ##################################


      # Display filtering preview
      shiny::observeEvent(c(input$blank_multiplier, input$sample_threshold, input$group_threshold),{
        if (!is.null(r6$tables$data_filtered)){
          
          # Calculate remaining cols
          print(colnames((r6$tables$data_filtered)))
          total_cols = ncol(r6$tables$data_filtered)
          del_cols = blank_filter(data_table = r6$tables$data_filtered,
                                  blank_table = r6$tables$data_raw[r6$get_idx_blanks(table = r6$tables$meta_raw),-which(colnames(r6$tables$data_raw) == r6$col_id_data)],
                                  blank_multiplier = as.numeric(input$blank_multiplier),
                                  sample_threshold = input$sample_threshold)
          saved_cols = group_filter(data_table = r6$tables$data_filtered,
                                    blank_table = r6$tables$data_raw[r6$get_idx_blanks(table = r6$tables$meta_raw),-which(colnames(r6$tables$data_raw) == r6$col_id_data)],
                                    meta_table = r6$tables$meta_filtered,
                                    del_cols = del_cols,
                                    col_group = r6$col_group,
                                    blank_multiplier = as.numeric(input$blank_multiplier),
                                    group_threshold = input$group_threshold)
          del_cols = setdiff(del_cols,saved_cols)
          remaining_cols = total_cols - length(del_cols)
          
          # Update class bar plot
          output$class_barplot = shiny::renderPlot(
            expr = preview_class_plot(r6 = r6,
                                      del_cols = del_cols),
            bg = "transparent"
          )
          
          # Update progress bar
          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = remaining_cols,
            total = total_cols
          )
        }
      })
      
      
      # Drop columns
      shiny::observeEvent(input$drop_cols,{
        selected_feats = input$manual_selection
        for (c in input$class_selection) {
          selected_feats = c(selected_feats, rownames(r6$tables$feat_filtered)[r6$tables$feat_filtered[,"lipid_class"] == c])
        }
        
        r6$tables$data_filtered = r6$tables$data_filtered[,!(colnames(r6$tables$data_filtered) %in% selected_feats)]
        print(colnames(r6$tables$data_filtered))
        
        r6$set_feat_filtered()
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "class_selection",
          choices = unique(r6$tables$feat_filtered$lipid_class),
          selected = character(0)
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "manual_selection",
          choices = colnames(r6$tables$data_filtered),
          selected = character(0)
        )
        
        
      })
      
      # Keep columns
      shiny::observeEvent(input$keep_cols,{
        selected_feats = input$manual_selection
        for (c in input$class_selection) {
          selected_feats = c(selected_feats, rownames(r6$tables$feat_filtered)[r6$tables$feat_filtered[,"lipid_class"] == c])
        }
        
        r6$tables$data_filtered = r6$tables$data_filtered[,(colnames(r6$tables$data_filtered) %in% selected_feats)]
        print(colnames(r6$tables$data_filtered))
        
        r6$set_feat_filtered()
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "class_selection",
          choices = unique(r6$tables$feat_filtered$lipid_class),
          selected = character(0)
        )
        
        shiny::updateSelectizeInput(
          session = session,
          inputId = "manual_selection",
          choices = colnames(r6$tables$data_filtered),
          selected = character(0)
        )
        
      })
      
      
      # Save button
      shiny::observeEvent(input$save, {
        
        # Apply filtering to the filtered table
        r6$feature_filter(blank_multiplier = as.numeric(input$blank_multiplier),
                          sample_threshold = input$sample_threshold,
                          group_threshold = input$group_threshold)

        # Update progress bar
        shinyWidgets::updateProgressBar(
          session = session,
          id = "col_count_bar",
          value = ncol(r6$tables$data_filtered),
          total = ncol(r6$tables$data_filtered)
        )
        
        ## Produce ensuing tables
        
        # Normalisation
        r6$normalise_z_score()
        r6$normalise_class()
        r6$normalise_total()
        r6$normalise_class_z_score()
        r6$normalise_total_z_score()
        r6$set_feat_filtered()
        
        # Class table
        r6$class_grouping()
        r6$normalise_class_table_z_score()
      })
      
      
      # Download filtered data
      dl_table = shiny::reactive(r6$tables$data_filtered)
      
      output$data_filtered_download = shiny::downloadHandler(
        filename = function(){"lipidomics_filtered.csv"},
        content = function(file_name){
          write.csv(dl_table(), file_name)
        }
      )
      
    }
  )
}


