#-------------------------------------------------------- Lipidomics server ----

lipidomics_server = function(id, ns, input, output, session, r6, head_meta = F, head_data = T, max_rows = 10, max_cols = 8) {

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
            width = 4,
            shiny::fileInput(
              inputId = ns("file_meta"),
              label = NULL,
              multiple = F,
              accept = c(".csv", ".tsv", ".txt", ".xlsx"),
              width = "100%")
          ),
          # Table select
          shiny::column(
            width = 4,
            shinyWidgets::switchInput(
              inputId = ns("preview_meta"),
              label = "Display full table",
              onLabel = "Yes",
              offLabel = "No",
              value = head_meta,
              labelWidth = "120px"
            )
          ),
          # Display preview or full table
          shiny::column(
            width = 4,
            shiny::selectInput(
              inputId = ns('select_meta_table'),
              label = NULL,
              choices = c('Imported metadata table'),
              selected = 'Imported metadata table',
              width = '100%'
            )
          )
        ),

        # Table preview box
        bs4Dash::box(
          id = ns('table_box'),
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
          id = ns('summary_box'),
          title = 'Data summary',
          width = 12,
          # shiny::uiOutput(outputId = ns('summary_output')),
          shiny::fluidRow(
            shiny::column(
              width = 2,
              shiny::fluidRow(
                shiny::p('Total rows: '),
                shiny::textOutput(outputId = ns('out_total_rows'))
              ),
              shiny::fluidRow(
                shiny::p('Total columns: '),
                shiny::textOutput(outputId = ns('out_total_cols'))
              ),
              shiny::fluidRow(
                shiny::p('Sample count: '),
                shiny::textOutput(outputId = ns('out_sample_count'))
              ),
              shiny::fluidRow(
                shiny::p('Blank count: '),
                shiny::textOutput(outputId = ns('out_blank_count'))
              ),
              shiny::fluidRow(
                shiny::p('QC count: '),
                shiny::textOutput(outputId = ns('out_qc_count'))
              ),
              shiny::fluidRow(
                shiny::p('Pool count: '),
                shiny::textOutput(outputId = ns('out_pool_count'))
              ),
              shiny::fluidRow(
                shiny::p('Batch count: '),
                shiny::textOutput(outputId = ns('out_batch_count'))
              ),
              shiny::fluidRow(
                shiny::p('Group count: '),
                shiny::textOutput(outputId = ns('out_group_count'))
              )

            ),
            shiny::column(
              width = 5,
              shiny::plotOutput(
                outputId = ns('group_distribution_preview'),
                height = '300px'
              )
            ),
            shiny::column(
              width = 5,
              shiny::plotOutput(
                outputId = ns('type_distribution_preview'),
                height = '300px'
              )
            )
          ),
          collapsible = T,
          collapsed  = T,
          maximizable = F,
          headerBorder = T
        ),

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
        )
      )
    )
  })

  #------------------------------------------------- Metadata upload server ----

  # Upload metadata
  session$userData[[id]]$upload_meta = shiny::observeEvent(input$file_meta, {
    file_path = input$file_meta$datapath
    data_table = soda_read_table(file_path = file_path)
    r6$tables$imp_meta = data_table

    # Preview table
    if (input$preview_meta){
      output$metadata_preview_table = renderDataTable({
        DT::datatable(data_table[1:min(max_rows, nrow(data_table)),1:min(max_cols, ncol(data_table))], options = list(paging = FALSE, dom = 't'))
      })
    }else{
      output$metadata_preview_table = renderDataTable({
        DT::datatable(data_table, options = list(paging = FALSE, dom = 't'))
      })
    }

    output$out_total_rows = shiny::renderText(nrow(data_table))
    output$out_total_cols = shiny::renderText(ncol(data_table))

    bs4Dash::updateBox(id = 'table_box', action = 'toggle')
    bs4Dash::updateBox(id = 'summary_box', action = 'toggle')

    # Update select inputs
    shiny::updateSelectInput(
      inputId = 'select_id_meta',
      choices = colnames(r6$tables$imp_meta),
      selected = colnames(r6$tables$imp_meta)[1]
    )
    shiny::updateSelectInput(
      inputId = 'select_group_col',
      choices = colnames(r6$tables$imp_meta),
      selected = colnames(r6$tables$imp_meta)[3]
    )
    shiny::updateSelectInput(
      inputId = 'select_type_col',
      choices = colnames(r6$tables$imp_meta),
      selected = colnames(r6$tables$imp_meta)[2]
    )
    shiny::updateSelectInput(
      inputId = 'select_batch_col',
      choices = colnames(r6$tables$imp_meta),
      selected = colnames(r6$tables$imp_meta)[4]
    )


  })

  # Preview all / subset switch
  session$userData[[id]]$preview_meta = shiny::observeEvent(c(input$preview_meta, input$select_meta_table), {
    shiny::req(r6$tables$imp_meta)
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    if (input$preview_meta){
      output$metadata_preview_table = renderDataTable({
        DT::datatable(data_table[1:min(max_rows, nrow(data_table)),1:min(max_cols, ncol(data_table))], options = list(paging = FALSE, dom = 't'))
      })
    }else{
      output$metadata_preview_table = renderDataTable({
        DT::datatable(data_table, options = list(paging = FALSE, dom = 't'))
      })
    }

    output$out_total_rows = shiny::renderText(nrow(data_table))
    output$out_total_cols = shiny::renderText(ncol(data_table))

  })

  # Get ID
  session$userData[[id]]$id_select_meta = shiny::observeEvent(input$select_id_meta, {
    shiny::req(r6$tables$imp_meta)
    if (length(r6$tables$imp_meta[,input$select_id_meta]) == length(unique(r6$tables$imp_meta[,input$select_id_meta]))) {
      r6$indices$id_col_meta = input$select_id_meta
      r6$set_raw_meta()
      shiny::updateSelectInput(
        inputId = 'select_meta_table',
        choices = c('Imported metadata table', 'Raw metadata table')
      )
    } else {
      print('ERROR: Non-unique IDs in ID column')
      r6$tables$raw_meta = NULL
      shiny::updateSelectInput(
        inputId = 'select_meta_table',
        choices = c('Imported metadata table'),
        selected = 'Imported metadata table'
      )
    }
  })

  # Group col selection
  session$userData[[id]]$select_group_col = shiny::observeEvent(c(input$select_group_col, input$select_meta_table), {
    shiny::req(r6$tables$imp_meta)
    r6$indices$group_col = input$select_group_col
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    groups = unique_na_rm(data_table[, input$select_group_col])
    output$out_group_count = shiny::renderText(length(groups))

    freq = data.frame(table(as.factor(na.omit(data_table[, input$select_group_col]))))
    names(freq) = c("value", "count")

    output$group_distribution_preview = shiny::renderPlot(
      ggplot2::ggplot(data = freq, aes(x = value, y = count)) +
        geom_bar(stat = "identity", fill="blue")+
        geom_text(aes(label=count), vjust=0, hjust = -0.5, size=6)+
        xlab(NULL) +
        ylab(NULL) +
        ylim(0,max(freq$count)+10) +
        theme_minimal() +
        coord_flip() +
        labs(title = 'Groups distribution')+
        theme(
          plot.title = element_text(size=17, hjust = 0.5),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15)
        )
    )
  })

  # Batch col selection
  session$userData[[id]]$select_batch_col = shiny::observeEvent(c(input$select_batch_col, input$select_meta_table), {
    shiny::req(r6$tables$imp_meta)
    r6$indices$batch_col = input$select_batch_col
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    batches = unique_na_rm(data_table[, input$select_batch_col])
    output$out_batch_count = shiny::renderText(length(batches))

  })

  # Type col selection
  session$userData[[id]]$select_type_col = shiny::observeEvent(c(input$select_type_col, input$blank_pattern, input$qc_pattern, input$pool_pattern, input$select_meta_table), {
    shiny::req(r6$tables$imp_meta)
    r6$indices$type_col = input$select_type_col
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)

    type_vector = data_table[, input$select_type_col]

    blank_idx = grep(pattern = input$blank_pattern,
                     x = type_vector,
                     ignore.case = TRUE)
    qc_idx = grep(pattern = input$qc_pattern,
                  x = type_vector,
                  ignore.case = TRUE)
    pool_idx = grep(pattern = input$pool_pattern,
                    x = type_vector,
                    ignore.case = TRUE)

    type_vector[1:length(type_vector)] = 'Samples'
    type_vector[blank_idx] = 'Blanks'
    type_vector[qc_idx] = 'QCs'
    type_vector[pool_idx] = 'Pools'

    blank_count = length(blank_idx)
    qc_count = length(qc_idx)
    pool_count = length(pool_idx)
    sample_count = nrow(data_table) - sum(c(blank_count, qc_count, pool_count))

    output$out_sample_count = shiny::renderText(sample_count)
    output$out_blank_count = shiny::renderText(blank_count)
    output$out_qc_count = shiny::renderText(qc_count)
    output$out_pool_count = shiny::renderText(pool_count)

    type_vector = data.frame(table(as.factor(type_vector)))
    names(type_vector) = c("value", "count")

    output$type_distribution_preview = shiny::renderPlot(
      ggplot2::ggplot(data = type_vector, aes(x = value, y = count)) +
        geom_bar(stat = "identity", fill="blue")+
        geom_text(aes(label=count), vjust=-0.5, hjust = 0.5, size=6)+
        xlab(NULL) +
        ylab(NULL) +
        ylim(0,max(type_vector$count)+10) +
        theme_minimal() +
        labs(title = 'Type distribution')+
        theme(
          plot.title = element_text(size=17, hjust = 0.5),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15)
        )
    )
  })
}
