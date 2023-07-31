#-------------------------------------------------------- Lipidomics server ----

lipidomics_server = function(id, ns, input, output, session, head_meta = F, head_data = T) {
  output$up_metadata_ui = shiny::renderUI({
    shiny::fluidRow(

      # First column with the table input and preview of the raw data
      shiny::column(
        width = 8,
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
        # Display preview or full table
        shiny::checkboxInput(inputId = ns("preview_meta"), label = "Display preview only", value = head_meta),
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
            shiny::span(textOutput(outputId = ns("id_error_meta")), style="color:red"),

            # Select group column
            shiny::selectInput(inputId = ns("select_sample_group"), choices = NULL, label = "Group column", multiple = F, width = "100%"),

            shiny::span(textOutput(outputId = ns("found_groups")))

          ),
          shiny::column(
            width = 6,
            # Select sample type column
            shiny::selectInput(inputId = ns("select_sample_type"), choices = NULL, label = "Type column", multiple = F, width = "100%"),

            # Select batch column
            shiny::selectInput(inputId = ns("select_sample_batch"), choices = NULL, label = "Batch column", multiple = F, width = "100%"),

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
            shiny::textInput(inputId = ns("blank_pattern"), label = "Blanks:", value = "blank", width = "100%"),
            shiny::span(textOutput(outputId = ns("found_blanks")))
          ),

          shiny::column(
            width = 4,
            # Select QC battern text for regex
            shiny::textInput(inputId = ns("qc_pattern"), label = "QCs:", value = "quality", width = "100%"),
            shiny::span(textOutput(outputId = ns("found_qcs")))
          ),

          shiny::column(
            width = 4,
            # Select pool battern text for regex
            shiny::textInput(inputId = ns("pool_pattern"), label = "Pools:", value = "pool", width = "100%"),
            shiny::span(textOutput(outputId = ns("found_pools")))
          )
        )
      )
    )
  })

  session$userData[[id]]$test = shiny::observeEvent(input$truffles,{
    print(input$truffles)
  })

}
