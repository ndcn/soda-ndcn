#----------------------------------------------------- Lipidomics utilities ----

#
reset_sample_filters = function(input, session, r6) {
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

update_sample_filters = function(input, session, r6) {
  # Update input for the manual exclusion
  shiny::updateSelectizeInput(
    session = session,
    inputId = "selection_manual",
    choices = rownames(r6$tables$raw_meta)
  )

  # Update available groups to filter
  if (input$exclusion_meta_col != "") {
    shiny::updateSelectInput(
      session = session,
      inputId = "exclusion_meta_val",
      choices = unique(r6$tables$raw_meta[,input$exclusion_meta_col]),
      selected = character(0)
    )
  }
}

sample_row_selection = function(input, r6) {
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
            width = 3,
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
          shiny::tagList(
            shiny::fluidRow(
              shiny::column(
                width = 12,
                shinyWidgets::progressBar(
                  id = ns("row_count_bar"),
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
            )
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
    if (ncol(data_table) > 70) {
      print('ERROR: uploaded file has more than 70 columns, unlikely to be a metadata file')
      return()
    }
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

    if (input$table_box$collapsed) {
      bs4Dash::updateBox(id = 'table_box', action = 'toggle')
    }
    if (input$summary_box$collapsed) {
      bs4Dash::updateBox(id = 'summary_box', action = 'toggle')
    }

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

    shinyjs::disable("file_meta")
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


  })

  # Get ID
  session$userData[[id]]$id_select_meta = shiny::observeEvent(input$select_id_meta, {
    shiny::req(r6$tables$imp_meta)
    print('Setting ID column')
    if (length(r6$tables$imp_meta[,input$select_id_meta]) == length(unique(r6$tables$imp_meta[,input$select_id_meta]))) {
      r6$indices$id_col_meta = input$select_id_meta
      r6$set_raw_meta()
      update_sample_filters(input = input, session = session, r6 = r6)

      # Update metadata column input
      shiny::updateSelectInput(
        session = session,
        inputId = "exclusion_meta_col",
        choices = colnames(r6$tables$raw_meta)
      )

      shiny::updateSelectInput(
        inputId = 'select_meta_table',
        choices = c('Imported metadata table', 'Raw metadata table'),
        selected = 'Raw metadata table'
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
  session$userData[[id]]$select_group_col = shiny::observeEvent(c(input$select_group_col, input$selection_drop, input$selection_keep, input$reset_meta), {
    shiny::req(r6$tables$raw_meta)
    print('Setting group column')
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    r6$indices$group_col = input$select_group_col
    groups = unique_na_rm(r6$tables$imp_meta[, input$select_group_col])
    freq = data.frame(table(base::factor(na.omit(data_table[, input$select_group_col]), levels = groups)))
    names(freq) = c("value", "count")

    output$group_distribution_preview = shiny::renderPlot(
      ggplot2::ggplot(data = freq, aes(x = value, y = count)) +
        geom_bar(stat = "identity", fill="blue")+
        geom_text(aes(label=count), vjust=0.5, hjust = -0.5, size=6)+
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
  session$userData[[id]]$select_batch_col = shiny::observeEvent(input$select_batch_col, {
    shiny::req(r6$tables$imp_meta)
    print('Setting batch column')
    r6$indices$batch_col = input$select_batch_col
  })

  # Type col selection
  session$userData[[id]]$select_type_col = shiny::observeEvent(c(input$select_type_col, input$blank_pattern, input$qc_pattern, input$pool_pattern, input$select_id_meta), {
    shiny::req(r6$tables$imp_meta)
    print('Setting indices')
    r6$indices$type_col = input$select_type_col

    type_vector = r6$tables$imp_meta[, input$select_type_col]

    blank_idx = grep(pattern = input$blank_pattern,
                     x = type_vector,
                     ignore.case = TRUE)
    qc_idx = grep(pattern = input$qc_pattern,
                  x = type_vector,
                  ignore.case = TRUE)
    pool_idx = grep(pattern = input$pool_pattern,
                    x = type_vector,
                    ignore.case = TRUE)

    sample_idx = 1:nrow(r6$tables$imp_meta)
    sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

    r6$indices$idx_blanks = blank_idx
    r6$indices$idx_qcs = qc_idx
    r6$indices$idx_pools = pool_idx
    r6$indices$idx_samples = sample_idx

    r6$indices$rownames_blanks = r6$tables$imp_meta[blank_idx, input$select_id_meta]
    r6$indices$rownames_qcs = r6$tables$imp_meta[qc_idx, input$select_id_meta]
    r6$indices$rownames_pools = r6$tables$imp_meta[pool_idx, input$select_id_meta]
    r6$indices$rownames_samples = r6$tables$imp_meta[sample_idx, input$select_id_meta]

  })



  # Type col selection
  session$userData[[id]]$select_type_col = shiny::observeEvent(c(input$select_type_col, input$blank_pattern, input$qc_pattern, input$pool_pattern, input$select_id_meta, input$select_meta_table, input$selection_drop, input$selection_keep, input$reset_meta), {
    shiny::req(r6$tables$raw_meta)
    print('Updating type plot.')

    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    if (input$select_meta_table == 'Imported metadata table') {
      blank_idx = intersect(rownames(data_table), r6$indices$idx_blanks)
      qc_idx = intersect(rownames(data_table), r6$indices$idx_qcs)
      pool_idx = intersect(rownames(data_table), r6$indices$idx_pools)
      sample_idx = intersect(rownames(data_table), r6$indices$idx_samples)
    } else {
      blank_idx = intersect(rownames(data_table), r6$indices$rownames_blanks)
      qc_idx = intersect(rownames(data_table), r6$indices$rownames_qcs)
      pool_idx = intersect(rownames(data_table), r6$indices$rownames_pools)
      sample_idx = intersect(rownames(data_table), r6$indices$rownames_samples)
    }

    data_table[, input$select_type_col] = 'Samples'
    data_table[blank_idx, input$select_type_col] = 'Blanks'
    data_table[qc_idx, input$select_type_col] = 'QCs'
    data_table[pool_idx, input$select_type_col] = 'Pools'

    freq_table = data.frame(table(base::factor(data_table[, input$select_type_col], levels = c('Pools', 'QCs', 'Blanks', 'Samples'))))
    names(freq_table) = c("value", "count")

    output$type_distribution_preview = shiny::renderPlot(
      ggplot2::ggplot(data = freq_table, aes(x = value, y = count)) +
        geom_bar(stat = "identity", fill="blue")+
        geom_text(aes(label=count), vjust=-0.5, hjust = 0.5, size=6)+
        xlab(NULL) +
        ylab(NULL) +
        ylim(0,max(freq_table$count)+10) +
        theme_minimal() +
        labs(title = 'Type distribution')+
        theme(
          plot.title = element_text(size=17, hjust = 0.5),
          axis.text.x = element_text(size = 15),
          axis.text.y = element_text(size = 15)
        )
    )
  })


  # Update the metadata value once a metadata column is selected
  session$userData[[id]]$exclusion_meta_col = shiny::observeEvent(c(input$exclusion_meta_col),{
    shiny::updateSelectInput(
      session = session,
      inputId = "exclusion_meta_val",
      choices = unique(r6$tables$raw_meta[,input$exclusion_meta_col]),
      selected = character(0)
    )
  })


  # Update the rows to filter once a metadata value is selected
  session$userData[[id]]$exclusion_meta_val = shiny::observeEvent(c(input$exclusion_meta_val),{
    if (!is.null(input$exclusion_meta_val)) {
      bool_vector = c()
      for (value in input$exclusion_meta_val) {
        bool_vector[[length(bool_vector) + 1]] = r6$tables$raw_meta[,input$exclusion_meta_col] == value
      }
      bool_vector = Reduce("|", bool_vector)

      shiny::updateSelectizeInput(
        session = session,
        inputId = "exclusion_meta_row",
        choices = rownames(r6$tables$raw_meta)[bool_vector],
        selected = rownames(r6$tables$raw_meta)[bool_vector]
      )
    }
  })

  # Clear button
  session$userData[[id]]$clear_filters = shiny::observeEvent(input$clear_filters, {
    print('Clearing metadata filters')
    reset_sample_filters(input = input, session = session, r6 = r6)
  })

  # Reset button
  session$userData[[id]]$reset_meta = shiny::observeEvent(input$reset_meta, {
    print('Reseting metadata table')
    r6$set_raw_meta()
    update_sample_filters(input = input, session = session, r6 = r6)
  })

  # Drop button
  session$userData[[id]]$selection_drop = shiny::observeEvent(input$selection_drop, {
    print('Dropping selected samples')
    selected_rows = sample_row_selection(input = input, r6 = r6)
    if (!is.null(selected_rows)){
      r6$tables$raw_meta = drop_rows(data_table = r6$tables$raw_meta,
                                     rows = selected_rows)
    }
    reset_sample_filters(input = input, session = session, r6 = r6)
    update_sample_filters(input = input, session = session, r6 = r6)
  })

  # Keep button
  session$userData[[id]]$selection_keep = shiny::observeEvent(input$selection_keep, {
    print('Keeping selected samples')
    selected_rows = sample_row_selection(input = input, r6 = r6)
    if (!is.null(selected_rows)){
      r6$tables$raw_meta = keep_rows(data_table = r6$tables$raw_meta,
                                     rows = selected_rows)
    }
    reset_sample_filters(input = input, session = session, r6 = r6)
    update_sample_filters(input = input, session = session, r6 = r6)
  })

  # Row count progress bar
  session$userData[[id]]$row_count_bar = shiny::observeEvent(c(input$selection_keep, input$selection_drop, input$reset_meta, input$select_meta_table), {
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    shinyWidgets::updateProgressBar(
      session = session,
      id = "row_count_bar",
      value = nrow(data_table),
      total = nrow(r6$tables$imp_meta)
    )
  })

}
