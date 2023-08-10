#-------------------------------------------------------- Proteomics server ----

proteomics_server = function(id, ns, input, output, session, module_controler) {

  # Extract some values and update the module controler
  r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
  m = r6$name
  slot = r6$slot

  #---------------------------------------------- Metadata upload rendering ----

  output$up_metadata_ui = shiny::renderUI({
    shiny::fluidRow(

      # First column with the table input and preview of the raw data
      shiny::column(
        width = 8,

        shiny::br(),


        shiny::fluidRow(
          # Data upload
          shiny::column(
            width = 6,
            shiny::fileInput(
              inputId = ns("file_meta"),
              label = NULL,
              multiple = F,
              accept = c(".csv", ".tsv", ".txt", ".xlsx"),
              width = "100%")
          ),
          # Table select
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = ns('select_meta_table'),
              label = NULL,
              choices = c('Imported metadata table'),
              selected = 'Imported metadata table',
              width = '100%'
            )
          ),
          # Download metadata button
          shiny::column(
            width = 3,
            shiny::downloadButton(
              outputId = ns("download_metatable"),
              label = "Download",
              style = "width:100%;"
            )
          )
        ),

        # Table preview box
        bs4Dash::box(
          id = ns('table_box_meta'),
          title = 'Table preview (switch table above)',
          width = 12,
          DT::dataTableOutput(ns("metadata_preview_table")),style = "height:400px; overflow-y: scroll;overflow-x: scroll;",
          collapsible = T,
          collapsed  = T,
          maximizable = T,
          headerBorder = T
        ),

        # Summary box
        bs4Dash::box(
          id = ns('summary_box_meta'),
          title = 'Data summary',
          width = 12,
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinyWidgets::progressBar(
                  id = ns("row_count_bar_meta"),
                  title = "Row count",
                  value = 100,
                  total = 100,
                  unit_mark = "%"
                )
              )
            ),
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shiny::plotOutput(
                  outputId = ns('group_distribution_preview'),
                  height = '300px'
                )
              ),
              shiny::column(
                width = 6,
                shiny::plotOutput(
                  outputId = ns('type_distribution_preview'),
                  height = '300px'
                )
              )
            )
          ),
          collapsible = T,
          collapsed  = T,
          maximizable = F,
          headerBorder = T
        )

      ),

      # Second column for data curation
      shiny::column(
        width = 4,
        shiny::tags$h3("Select columns"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            # Select ID column
            shiny::selectInput(inputId = ns("select_id_meta"), choices = NULL, label = "Sample IDs", multiple = F, width = "100%"),

            # Select group column
            shiny::selectInput(inputId = ns("select_group_col"), choices = NULL, label = "Group column", multiple = F, width = "100%"),
            shiny::span(textOutput(outputId = ns("found_groups")))

          ),
          shiny::column(
            width = 6,
            # Select sample type column
            shiny::selectInput(inputId = ns("select_type_col"), choices = NULL, label = "Type column", multiple = F, width = "100%"),

            # Select batch column
            shiny::selectInput(inputId = ns("select_batch_col"), choices = NULL, label = "Batch column", multiple = F, width = "100%"),

            shiny::span(textOutput(outputId = ns("found_batches")))
          )
        ),

        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),

        # Section for regex text patterns
        shiny::h3("Text patterns"),

        shiny::fluidRow(
          shiny::column(
            width = 4,
            # Select blank battern text for regex
            shiny::textInput(inputId = ns("blank_pattern"), label = "Blanks:", value = "blank", width = "100%")
          ),

          shiny::column(
            width = 4,
            # Select QC battern text for regex
            shiny::textInput(inputId = ns("qc_pattern"), label = "QCs:", value = "quality", width = "100%")
          ),

          shiny::column(
            width = 4,
            # Select pool battern text for regex
            shiny::textInput(inputId = ns("pool_pattern"), label = "Pools:", value = "pool", width = "100%")
          )
        ),

        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),

        # Section for sample filtering
        shiny::h3("Sample filtering"),
        shiny::fluidRow(
          shiny::column(
            width = 12,
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
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("selection_drop"),
                  label = "Drop",
                  width = "100%"
                )
              ),
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("selection_keep"),
                  label = "Keep",
                  width = "100%"
                )
              ),
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("clear_filters"),
                  label = "Clear filters",
                  width = "100%"
                )
              ),
              shiny::column(
                width = 3,
                shiny::actionButton(
                  inputId = ns("reset_meta"),
                  label = "Reset table",
                  width = "100%"
                )
              )
            )
          )
        )
      )
    )
  })


  #-------------------------------------------------- Data upload rendering ----

  output$up_data_ui = shiny::renderUI({
    shiny::fluidRow(
      # First column with the table input and preview of the raw data
      shiny::column(
        width = 8,

        shiny::br(),


        shiny::fluidRow(
          # Data upload
          shiny::column(
            width = 6,
            shiny::fileInput(
              inputId = ns("file_data"),
              label = NULL,
              multiple = F,
              accept = c(".csv", ".tsv", ".txt", ".xlsx"),
              width = "100%")
          ),
          # Table select
          shiny::column(
            width = 3,
            shiny::selectInput(
              inputId = ns('select_data_table'),
              label = NULL,
              choices = c('Imported data table'),
              selected = 'Imported data table',
              width = '100%'
            )
          ),
          # Download button
          shiny::column(
            width = 3,
            shiny::downloadButton(
              outputId = ns("download_datatable"),
              label = "Download",
              style = "width:100%;"
            )
          )
        ),
        # Table preview box
        bs4Dash::box(
          id = ns('table_box_data'),
          title = 'Table preview (switch table above)',
          width = 12,
          DT::dataTableOutput(ns("data_preview_table")),style = "height:400px; overflow-y: scroll;overflow-x: scroll;",
          collapsible = T,
          collapsed  = T,
          maximizable = T,
          headerBorder = T
        ),

        # Summary box
        bs4Dash::box(
          id = ns('summary_box_data'),
          title = 'Data summary',
          width = 12,
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(
                width = 6,
                shinyWidgets::progressBar(
                  id = ns("row_count_bar_data"),
                  title = "Row count",
                  value = 100,
                  total = 100,
                  unit_mark = "%"
                )
              ),
              shiny::column(
                width = 6,
                shinyWidgets::progressBar(
                  id = ns("col_count_bar"),
                  title = "Column count",
                  value = 100,
                  total = 100,
                  unit_mark = "%"
                )
              )
            ),
            shiny::fluidRow(
              shiny::plotOutput(
                outputId = ns('lipid_class_summary'),
                height = '300px'
              )
            )
          ),
          collapsible = T,
          collapsed  = T,
          maximizable = F,
          headerBorder = T
        )
      ),
      shiny::column(
        width = 4,
        shiny::tags$h3("Select columns"),
        # Select ID column
        shiny::selectInput(inputId = ns("select_id_data"), choices = NULL, label = "Sample IDs", multiple = F, width = "100%"),

        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::fluidRow(
          shiny::column(
            width = 4,
            shiny::h3("Imputation")
          ),
          shiny::column(
            width = 3,
            shinyWidgets::switchInput(
              inputId = ns("apply_imputation"),
              label = "Apply",
              onLabel = "Y",
              offLabel = "N",
              value = F,
              labelWidth = "80px"
            )
          ),
          shiny::column(
            width = 5,
            shinyWidgets::switchInput(
              inputId = ns("impute_before"),
              label = "Before filtering",
              onLabel = "Y",
              offLabel = "N",
              value = T,
              labelWidth = "120px"
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectizeInput(inputId = ns("na_imputation"),
                                  choices = c('minimum', 'mean', 'median', 'max'),
                                  selected = "median",
                                  label = 'Imputation method',
                                  multiple = F,
                                  width = "100%")
          ),
          shiny::column(
            width = 6,
            shiny::sliderInput(
              inputId = ns('imputation_min_values'),
              label = 'Minimum values',
              min = 0,
              max = 1,
              value = 0.6,
              step = 0.05,
              width = '100%'
            )
          )
        ),

        # Blank and group filtering
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::fluidRow(
          shiny::column(
            width = 8,
            shiny::h3("Blank & Group filtering")
          ),
          shiny::column(
            width = 4,
            shinyWidgets::switchInput(
              inputId = ns("apply_filtering"),
              label = "Apply",
              onLabel = "Yes",
              offLabel = "No",
              value = T,
              labelWidth = "80px"
            )
          )
        ),

        shiny::fluidRow(
          shiny::textInput(inputId = ns("blank_multiplier"), label = 'Blank multiplier', value = 2, width = "100%")
        ),

        shiny::fluidRow(
          # Sample threshold
          shiny::column(
            width = 6,
            shiny::sliderInput(inputId = ns("sample_threshold"), label = "Sample threshold", value = 0.8, min = 0, max = 1, step = 0.05, width = "100%")
          ),
          # Group threshold
          shiny::column(
            width = 6,
            shiny::sliderInput(inputId = ns("group_threshold"), label = "Group threshold", value = 0.8, min = 0, max = 1, step = 0.05, width = "100%")
          )
        ),

        # Normalisation
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::h3("Normalise to column"),
        shiny::selectizeInput(inputId = ns("normalise_to_col"),
                              label = NULL,
                              choices = character(0),
                              width = "100%"),

        # Manual filtering
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::h3("Manual filtering"),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectizeInput(inputId = ns("class_selection"), label = "Select classes", choices = NULL, multiple = TRUE, width = "100%")
          ),
          shiny::column(
            width = 6,
            shiny::selectizeInput(inputId = ns("manual_selection"), label = "Select feature", choices = NULL, multiple = TRUE, width = "100%")
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 3,
            shiny::actionButton(inputId = ns("drop_cols"), label =  "Drop", width = "100%")
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(inputId = ns("keep_cols"), label =  "Keep", width = "100%")
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(inputId = ns("clear_data_filters"), label =  "Clear filters", width = "100%")
          ),
          shiny::column(
            width = 3,
            shiny::actionButton(inputId = ns("reset_data_table"), label =  "Reset", width = "100%")
          )
        )
      )
    )
  })

  #----------------------------------------------- Visualize data rendering ----

  output$visualize_data_ui = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        # First column with the table input and preview of the raw data
        shiny::column(
          width = 11,
          shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                             label = NULL,
                                             status = "default",
                                             choices = lipidomics_plot_list(),
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                             size = "normal",
                                             justified = TRUE
          )
        ),
        shiny::column(
          width = 1,
          shinyWidgets::actionBttn(inputId = ns("clear_plots"),
                                   label = "Clear",
                                   style = "material-flat",
                                   color = "danger",
                                   block = T,
                                   icon = icon("x"))
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::uiOutput(
            outputId = ns("plotbox_field")
          )
        )
      )
    )
  })

  #------------------------------------------- Geneset enrichment rendering ----

  output$geneset_enrichment_ui = shiny::renderUI({
    shiny::h2('Geneset enrichment coming soon')
  })

  #------------------------------------------ Over-representation rendering ----

  output$over_representation_ui = shiny::renderUI({
    shiny::h2('Over-representation analysis coming soon')
  })



}
