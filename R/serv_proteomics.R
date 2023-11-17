plot_one_prot_or = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_or_plotbox_switch_ui(selection_list = selection_list)

  output$or_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_or_plotbox_switch_server(selection_list = input$show_plots_or)

  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_two_prot_or = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_or_plotbox_switch_ui(selection_list = selection_list)
  output$or_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_or_plotbox_switch_server(selection_list = input$show_plots_or)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_three_prot_or = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_or_plotbox_switch_ui(selection_list = selection_list)
  output$or_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_or_plotbox_switch_server(selection_list = input$show_plots_or)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_four_prot_or = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_or_plotbox_switch_ui(selection_list = selection_list)
  output$or_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_or_plotbox_switch_server(selection_list = input$show_plots_or)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_one_prot_gsea = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_gsea_plotbox_switch_ui(selection_list = selection_list)

  output$gsea_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_gsea_plotbox_switch_server(selection_list = input$show_plots_gsea)

  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_two_prot_gsea = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_gsea_plotbox_switch_ui(selection_list = selection_list)
  output$gsea_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_gsea_plotbox_switch_server(selection_list = input$show_plots_gsea)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_three_prot_gsea = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_gsea_plotbox_switch_ui(selection_list = selection_list)
  output$gsea_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_gsea_plotbox_switch_server(selection_list = input$show_plots_gsea)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_four_prot_gsea = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_gsea_plotbox_switch_ui(selection_list = selection_list)
  output$gsea_plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_gsea_plotbox_switch_server(selection_list = input$show_plots_gsea)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}



plot_one_prot = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_plotbox_switch_ui(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_two_prot = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_three_prot = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_four_prot = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })

  plot_servers = prot_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}



# switches

prot_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_pca" = prot_pca_ui,
                                          "select_heatmap" = prot_heatmap_ui,
                                          "select_volcano_plot" = prot_volcano_plot_ui
    )
    )
  }
  return(ui_functions)
}

prot_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_pca" = prot_pca_server,
                                                  "select_heatmap" = prot_heatmap_server,
                                                  "select_volcano_plot" = prot_volcano_plot_server
    )
    )
  }
  return(server_functions)
}

prot_gsea_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_dot_plot" = prot_dot_plot_ui,
                                          "select_ridge_plot" = prot_ridge_plot_ui,
                                          "select_cnet_plot" = prot_cnet_plot_ui,
                                          "select_emap_plot" = prot_emap_plot_ui
    )
    )
  }
  return(ui_functions)
}

prot_gsea_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_dot_plot" = prot_dot_plot_server,
                                                  "select_ridge_plot" = prot_ridge_plot_server,
                                                  "select_cnet_plot" = prot_cnet_plot_server,
                                                  "select_emap_plot" = prot_emap_plot_server)
    )
  }
  return(server_functions)
}


prot_or_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_dot_plot" = prot_or_dot_plot_ui,
                                          "select_bar_plot" = prot_or_bar_plot_ui,
                                          "select_cnet_plot" = prot_or_cnet_plot_ui,
                                          "select_emap_plot" = prot_or_emap_plot_ui
    )
    )
  }
  return(ui_functions)
}

prot_or_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_dot_plot" = prot_or_dot_plot_server,
                                                  "select_bar_plot" = prot_or_bar_plot_server,
                                                  "select_cnet_plot" = prot_or_cnet_plot_server,
                                                  "select_emap_plot" = prot_or_emap_plot_server)
    )
  }
  return(server_functions)
}


#-------------------------------------------------------- Proteomics server ----

proteomics_server = function(id, ns, input, output, session, module_controler) {

  # Extract some values and update the module controler
  r6 = module_controler$exp_r6[[stringr::str_replace(id, 'mod_', '')]]
  m = r6$name
  slot = r6$slot

  # Render skeleton UI
  output$omics_ui = shiny::renderUI({
    bs4Dash::tabsetPanel(
      id = ns('skeleton_ui'),
      type = "tabs",
      shiny::tabPanel(
        title = "Sample annotations",
        shiny::uiOutput(
          outputId = ns('up_sample_metadata_ui')
        )
      ),
      shiny::tabPanel(
        title = "Data",
        shiny::uiOutput(
          outputId = ns('up_data_ui')
        )
      ),
      shiny::tabPanel(
        title = "Feature annotations",
        shiny::uiOutput(
          outputId = ns('up_feature_metadata_ui')
        )
      ),
      shiny::tabPanel(
        title = "Interactive visualization",
        shiny::uiOutput(
          outputId = ns('visualize_data_ui')
        )
      ),
      shiny::tabPanel(
        title = "Functional analysis",
        shiny::uiOutput(
          outputId = ns('functional_analysis_ui')
        )
      ),
      shiny::tabPanel(
        title = "Geneset enrichment",
        shiny::uiOutput(
          outputId = ns('geneset_enrichment_ui')
        )
      ),
      shiny::tabPanel(
        title = "Over-representation",
        shiny::uiOutput(
          outputId = ns('over_representation_ui')
        )
      )
    )
  })

  #---------------------------------------------- Metadata upload rendering ----

  output$up_sample_metadata_ui = shiny::renderUI({
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


  #------------------------------------------------- Metadata upload server ----

  # Upload metadata
  session$userData[[id]]$upload_meta = shiny::observeEvent(input$file_meta, {
    file_path = input$file_meta$datapath
    data_table = soda_read_table(file_path = file_path)
    if (ncol(data_table) > 70) {
      print_tm(m, 'ERROR: uploaded file has more than 70 columns, unlikely to be a metadata file')
      return()
    }
    r6$tables$imp_meta = data_table
    # Preview table
    output$metadata_preview_table = renderDataTable({
      DT::datatable(data_table, options = list(paging = TRUE))
    })

    if (input$table_box_meta$collapsed) {
      bs4Dash::updateBox(id = 'table_box_meta', action = 'toggle')
    }
    if (input$summary_box_meta$collapsed) {
      bs4Dash::updateBox(id = 'summary_box_meta', action = 'toggle')
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
  session$userData[[id]]$select_meta_table = shiny::observeEvent(input$select_meta_table, {
    shiny::req(r6$tables$imp_meta)
    if (r6$preloaded_data) {return()}
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    output$metadata_preview_table = renderDataTable({
      DT::datatable(data_table, options = list(paging = TRUE))
    })

  })

  # Get ID
  session$userData[[id]]$id_select_meta = shiny::observeEvent(input$select_id_meta, {
    shiny::req(r6$tables$imp_meta)
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Setting ID column')
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
      print_tm(m, 'ERROR: Non-unique IDs in ID column')
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
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Setting group column')
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

    shiny::updateSelectInput(
      inputId = 'gseaprep_group_col',
      choices = colnames(r6$tables$raw_meta),
      selected = input$select_group_col
    )

  })

  # Batch col selection
  session$userData[[id]]$select_batch_col = shiny::observeEvent(input$select_batch_col, {
    shiny::req(r6$tables$imp_meta)
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Setting batch column')
    r6$indices$batch_col = input$select_batch_col
  })

  # Type col selection
  session$userData[[id]]$select_type_col = shiny::observeEvent(c(input$select_type_col, input$blank_pattern, input$qc_pattern, input$pool_pattern, input$select_id_meta), {
    shiny::req(c(r6$tables$imp_meta, input$blank_pattern, input$qc_pattern, input$pool_pattern))
    if (r6$preloaded_data) {return()}
    if ((input$select_type_col != "") & (!is.null(input$blank_pattern)) & (!is.null(input$qc_pattern)) & (!is.null(input$pool_pattern))) {
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
    }
  })



  # Type col selection
  session$userData[[id]]$select_type_col = shiny::observeEvent(c(input$select_type_col, input$blank_pattern, input$qc_pattern, input$pool_pattern, input$select_id_meta, input$select_meta_table, input$selection_drop, input$selection_keep, input$reset_meta), {
    shiny::req(r6$tables$raw_meta)
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Updating type plot.')

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
    if (r6$preloaded_data) {return()}
    shiny::updateSelectInput(
      session = session,
      inputId = "exclusion_meta_val",
      choices = unique(r6$tables$raw_meta[,input$exclusion_meta_col]),
      selected = character(0)
    )
  })


  # Update the rows to filter once a metadata value is selected
  session$userData[[id]]$exclusion_meta_val = shiny::observeEvent(c(input$exclusion_meta_val),{
    if (r6$preloaded_data) {return()}
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
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Clearing metadata filters')
    reset_sample_filters(input = input, session = session, r6 = r6)
  })

  # Reset button
  session$userData[[id]]$reset_meta = shiny::observeEvent(input$reset_meta, {
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Reseting metadata table')
    r6$set_raw_meta()
    update_sample_filters(input = input, session = session, r6 = r6)
  })

  # Drop button
  session$userData[[id]]$selection_drop = shiny::observeEvent(input$selection_drop, {
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Dropping selected samples')
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
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Keeping selected samples')
    selected_rows = sample_row_selection(input = input, r6 = r6)
    if (!is.null(selected_rows)){
      r6$tables$raw_meta = keep_rows(data_table = r6$tables$raw_meta,
                                     rows = selected_rows)
    }
    reset_sample_filters(input = input, session = session, r6 = r6)
    update_sample_filters(input = input, session = session, r6 = r6)
  })

  # Row count progress bar
  session$userData[[id]]$row_count_bar_meta = shiny::observeEvent(c(input$selection_keep, input$selection_drop, input$reset_meta, input$select_meta_table), {
    if (r6$preloaded_data) {return()}
    data_table = table_switch(table_name = input$select_meta_table, r6 = r6)
    shinyWidgets::updateProgressBar(
      session = session,
      id = "row_count_bar_meta",
      value = nrow(data_table),
      total = nrow(r6$tables$imp_meta)
    )
  })

  # Download meta table
  dl_meta_table = shiny::reactiveValues(
    name = NULL,
    table = NULL
  )

  session$userData[[id]]$download_metatable = shiny::observeEvent(c(input$select_meta_table, input$reset_meta, input$selection_keep, input$selection_drop) , {
    shiny::req(r6$tables$raw_meta)
    if (r6$preloaded_data) {return()}
    dl_meta_table$name = timestamped_name(paste0(stringr::str_replace_all(input$select_meta_table, " ", "_"), ".csv"))
    dl_meta_table$table = table_switch(input$select_meta_table, r6)
  })

  output$download_metatable = shiny::downloadHandler(
    filename = shiny::reactive(dl_meta_table$name),
    content = function(file_name) {
      write.csv(dl_meta_table$table, file_name, na = "")
    }
  )


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
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::selectInput(inputId = ns("select_id_data"), choices = NULL, label = "Sample IDs", multiple = F, width = "100%")
          ),
          shiny::column(
            width = 6,
            shiny::selectInput(
              inputId = ns('select_feature_type'),
              label = 'Feature ID type',
              choices = c('UNIPROT', 'SYMBOL', 'ENTREZID'),
              selected = 'SYMBOL',
              multiple = F
            )
          )
        ),

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

  #----------------------------------------------------- Data upload server ----

  # Upload metadata
  session$userData[[id]]$upload_data = shiny::observeEvent(input$file_data, {
    file_path = input$file_data$datapath
    data_table = soda_read_table(file_path = file_path)
    r6$tables$imp_data = data_table
    # Preview table
    output$data_preview_table = renderDataTable({
      DT::datatable(t(data_table), options = list(paging = TRUE, pageLength = 25))
    })

    if (input$table_box_data$collapsed) {
      bs4Dash::updateBox(id = 'table_box_data', action = 'toggle')
    }
    if (input$summary_box_data$collapsed) {
      bs4Dash::updateBox(id = 'summary_box_data', action = 'toggle')
    }
    # if (input$feat_table_preview_box$collapsed) {
    #   bs4Dash::updateBox(id = 'feat_table_preview_box', action = 'toggle')
    # }



    # Update select inputs
    shiny::updateSelectInput(
      inputId = 'select_id_data',
      choices = colnames(r6$tables$imp_data),
      selected = colnames(r6$tables$imp_data)[1]
    )

    # Update normalise to column
    shiny::updateSelectInput(
      inputId = 'normalise_to_col',
      choices = colnames(r6$tables$raw_meta),
      selected = character(0)
    )

    shinyjs::disable("file_data")
  })

  # Preview all / subset switch
  session$userData[[id]]$select_data_table = shiny::observeEvent(input$select_data_table, {
    shiny::req(r6$tables$imp_data)

    data_table = table_switch(table_name = input$select_data_table, r6 = r6)

    if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {


      shinyWidgets::updateProgressBar(
        session = session,
        id = "col_count_bar",
        value = ncol(data_table),
        total = ncol(r6$tables$imp_data)
      )

      shinyWidgets::updateProgressBar(
        session = session,
        id = "row_count_bar_data",
        value = nrow(data_table),
        total = nrow(r6$tables$imp_data)
      )
    }

    if (!is.null(data_table)) {
      if (ncol(data_table) > 1000) {
        data_table = t(data_table)
      }
    }

    output$data_preview_table = renderDataTable({
      DT::datatable(data_table, options = list(paging = TRUE, pageLength = 25))
    })

  })


  # Get ID
  session$userData[[id]]$id_select_data = shiny::observeEvent(input$select_id_data, {
    shiny::req(r6$tables$imp_data)
    if (r6$preloaded_data) {return()}
    print_tm(m, 'Setting ID column')
    if (length(r6$tables$imp_data[,input$select_id_data]) == length(unique(r6$tables$imp_data[,input$select_id_data]))) {
      r6$indices$id_col_data = input$select_id_data
      r6$get_blank_table()
      r6$set_raw_data(apply_imputation = input$apply_imputation,
                      impute_before = input$impute_before,
                      apply_filtering = input$apply_filtering,
                      imputation_function = input$na_imputation,
                      val_threshold = as.numeric(input$imputation_min_values),
                      blank_multiplier = as.numeric(input$blank_multiplier),
                      sample_threshold = as.numeric(input$sample_threshold),
                      group_threshold = as.numeric(input$group_threshold),
                      norm_col = input$normalise_to_col)

      r6$derive_data_tables()

      shiny::updateSelectInput(
        inputId = 'select_data_table',
        choices = c('Imported data table', 'Raw data table', 'Blank table', 'Total normalized table', 'Z-scored table', 'Z-scored total normalized table'),
        selected = 'Raw data table'
      )

      shiny::updateSelectInput(
        inputId = 'gseaprep_table_select',
        choices = c('Raw data table', 'Total normalized table', 'Z-scored table', 'Z-scored total normalized table'),
        selected = 'Total normalized table'
      )

      shiny::updateSelectInput(
        inputId = 'gseaprep_group_col',
        choices = colnames(r6$tables$raw_meta),
        selected = input$select_group_col
      )


    } else {
      print_tm(m, 'ERROR: Non-unique IDs in ID column')
      r6$tables$raw_meta = NULL
      shiny::updateSelectInput(
        inputId = 'select_meta_table',
        choices = c('Imported metadata table'),
        selected = 'Imported metadata table'
      )
    }
  })

  # GSEA groups
  session$userData[[id]]$gsea_groups = shiny::observeEvent(input$gseaprep_group_col,{
    shiny::req(input$gseaprep_group_col)
    shiny::updateSelectInput(
      inputId = 'gseaprep_groups',
      choices = unique(r6$tables$raw_meta[,input$gseaprep_group_col]),
      selected = unique(r6$tables$raw_meta[,input$gseaprep_group_col])[c(1,2)]
    )
  })

  # Feature filters
  session$userData[[id]]$row_col_data = shiny::observeEvent(
    c(input$apply_imputation,
      input$impute_before,
      input$na_imputation,
      input$imputation_min_values,
      input$apply_filtering,
      input$blank_multiplier,
      input$sample_threshold,
      input$group_threshold,
      input$normalise_to_col,
      # input$reset_data_table,
      input$selection_drop,
      input$selection_keep,
      input$reset_meta), {

        shiny::req(r6$tables$raw_data)
        if (r6$preloaded_data) {return()}
        print_tm(m, 'Updating data tables')
        r6$set_raw_data(apply_imputation = input$apply_imputation,
                        impute_before = input$impute_before,
                        apply_filtering = input$apply_filtering,
                        imputation_function = input$na_imputation,
                        val_threshold = as.numeric(input$imputation_min_values),
                        blank_multiplier = as.numeric(input$blank_multiplier),
                        sample_threshold = as.numeric(input$sample_threshold),
                        group_threshold = as.numeric(input$group_threshold),
                        norm_col = input$normalise_to_col)

        r6$derive_data_tables()

        # # Update class selection
        # shiny::updateSelectizeInput(
        #   session = session,
        #   inputId = "class_selection",
        #   choices = unique(r6$tables$feature_table$lipid_class),
        #   selected = character(0)
        # )
        #
        # # Update manual selection
        # shiny::updateSelectizeInput(
        #   session = session,
        #   inputId = "manual_selection",
        #   choices = colnames(r6$tables$raw_data),
        #   selected = character(0)
        # )

        data_table = table_switch(table_name = input$select_data_table, r6 = r6)

        if (input$select_data_table %in% c('Imported data table', 'Raw data table')) {


          shinyWidgets::updateProgressBar(
            session = session,
            id = "col_count_bar",
            value = ncol(data_table),
            total = ncol(r6$tables$imp_data)
          )

          shinyWidgets::updateProgressBar(
            session = session,
            id = "row_count_bar_data",
            value = nrow(data_table),
            total = nrow(r6$tables$imp_data)
          )
        }
        output$data_preview_table = renderDataTable({
          DT::datatable(t(data_table), options = list(paging = TRUE, pageLength = 25))
        })
      })

  #--------------------------------------------- Feature metadata rendering ----
  output$up_feature_metadata_ui = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 8,
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              width = 3,
              shiny::fileInput(
                inputId = ns("feat_add"),
                label = NULL,
                multiple = F,
                accept = c(".csv", ".tsv", ".txt", ".xlsx"),
                width = "100%"
              )
            ),
            shiny::column(
              width = 3,
              shiny::textInput(
                inputId = ns('feat_name_add'),
                label = NULL,
                width = '100%',
                placeholder = 'ex: feat_1'
              )
            ),
            shiny::column(
              width = 3,
              shiny::selectInput(
                inputId = ns('feat_table_select'),
                label = NULL,
                choices = c('Imported feature table', 'Feature table'),
                width = '100%'
              )
            ),
            shiny::column(
              width = 3,
              shiny::downloadButton(
                outputId = ns("download_feature_table"),
                label = "Download",
                style = "width:100%;"
              )
            )
          ),
          shiny::fluidRow(
            bs4Dash::box(
              id = ns('feat_table_preview_box'),
              title = 'Table preview',
              width = 12,
              DT::dataTableOutput(ns("feature_table_preview_table")),
              style = "height:400px; overflow-y: scroll;overflow-x: scroll;",
              collapsible = T,
              collapsed  = T,
              maximizable = T,
              headerBorder = T
            )
          )
        ),
        shiny::column(
          width = 4,
          shiny::h3('Remove feature table'),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(inputId = ns('feat_name_del'),
                                 label = NULL,
                                 choices = names(r6$tables$external_feature_tables),
                                 selected = NULL,
                                 width = '100%')
            ),
            shiny::column(
              width = 6,
              shiny::actionButton(inputId = ns('feat_del'),
                                  label = 'Remove',
                                  width = '100%')
            )
          )
        )
      )
    )
  })

  #------------------------------------------------ Feature metadata server ----
  # Manage Feature tables
  session$userData[[id]]$feat_add = shiny::observeEvent(input$feat_add, {
    if (input$feat_name_add == "") {
      counter = 1
      name = paste0("feat_", counter)
      while (name %in% names(r6$tables$external_feature_tables)) {
        counter = counter + 1
        name = paste0("feat_", counter)
      }
    } else {
      name = input$feat_name_add
    }
    r6$add_feature_table(name = name,
                         feature_file = input$feat_add$datapath)

    shiny::updateSelectInput(
      inputId = 'feat_name_del',
      choices = names(r6$tables$external_feature_tables)
    )
    r6$derive_data_tables()

    if (input$feat_table_preview_box$collapsed) {
      bs4Dash::updateBox(id = 'feat_table_preview_box', action = 'toggle')
    }

    shiny::updateSelectInput(
      inputId = 'feat_table_select',
      selected = 'Feature table'
    )

  })

  session$userData[[id]]$feat_go_ont = shiny::observeEvent(input$feat_go_ont, {
    shiny::updateTextInput(
      inputId = 'go_name_add',
      placeholder = paste0('ex: ', input$feat_go_ont)
    )
  })

  session$userData[[id]]$add_go_table = shiny::observeEvent(input$add_go_table, {
    shinyjs::disable("add_go_table")
    table_name = input$go_name_add
    if (table_name == '') {
      counter = 1
      while (paste0(input$feat_go_ont, counter) %in% names(r6$tables$external_enrichment_tables)) {
        counter = counter + 1
      }
      table_name = paste0(input$feat_go_ont, counter)
    }
    r6$add_go_data(name = table_name,
                   feature_names = rownames(r6$tables$imp_feature_table),
                   keyType = input$select_feature_type,
                   ont = input$feat_go_ont,
                   pvalueCutoff = as.numeric(input$feat_go_ont_cutoff))
    shiny::updateSelectInput(
      inputId = 'go_remove_table_select',
      choices = names(r6$tables$external_enrichment_tables)
    )

    shiny::updateSelectInput(
      inputId = 'annotations_table_select',
      choices = names(r6$tables$external_enrichment_tables)
    )


    r6$derive_data_tables()
    shinyjs::enable("add_go_table")
  })

  session$userData[[id]]$feat_del = shiny::observeEvent(input$feat_del, {
    r6$del_feature_table(name = input$feat_name_del)
    r6$derive_data_tables()

    names_left = names(r6$tables$external_feature_tables)
    if (is.null(names_left)) {
      names_left = character(0)
    }
    shiny::updateSelectInput(
      inputId = 'feat_name_del',
      choices = names_left
    )
  })

  # Preview all / subset switch
  session$userData[[id]]$enrichment_upload_button = shiny::observeEvent(input$enrichment_upload_button, {
    print('upload enrichment table')
    shinyjs::disable("enrichment_upload_button")

    table_name = input$enrich_name_add
    if (table_name == '') {
      counter = 1
      while (paste0('ENR', counter) %in% names(r6$tables$external_enrichment_tables)) {
        counter = counter + 1
      }
      table_name = paste0('ENR', counter)
    }


    if (!is.null(input$go_association_table$datapath)) {
      association_table = soda_read_table(input$go_association_table$datapath, first_column_as_index = T)
    } else {
      association_table = NULL
    }

    if (!is.null(input$go_terms_table$datapath)) {
      terms_table = soda_read_table(input$go_terms_table$datapath, first_column_as_index = T)
    } else {
      terms_table = NULL
    }

    r6$upload_enrichment_data(name = table_name,
                              association_table = association_table,
                              terms_table = terms_table,
                              sep = '|')

    shiny::updateSelectInput(
      inputId = 'go_remove_table_select',
      choices = names(r6$tables$external_enrichment_tables)
    )

    shiny::updateSelectInput(
      inputId = 'annotations_table_select',
      choices = names(r6$tables$external_enrichment_tables)
    )

    shinyjs::enable("enrichment_upload_button")

  })


  # Preview all / subset switch
  session$userData[[id]]$feat_table_select = shiny::observeEvent(input$feat_table_select, {
    shiny::req(r6$tables$imp_data)

    data_table = table_switch(table_name = input$feat_table_select, r6 = r6)


    if (!is.null(data_table)) {
      if (ncol(data_table) > 1000) {
        data_table = t(data_table)
      }
    }

    output$feature_table_preview_table = renderDataTable({
      DT::datatable(data_table, options = list(paging = TRUE, pageLength = 25))
    })

  })

  # Preview all / subset switch
  session$userData[[id]]$annotations_table_select = shiny::observeEvent(input$annotations_table_select, {
    shiny::req(r6$tables$imp_data)

    association_table = r6$tables$external_enrichment_tables[[input$annotations_table_select]]$association_table
    terms_table = r6$tables$external_enrichment_tables[[input$annotations_table_select]]$terms_table

    if (!is.null(association_table)) {
      if (ncol(association_table) > 1000) {
        association_table = t(association_table)
      }
    }

    if (!is.null(terms_table)) {
      if (ncol(terms_table) > 1000) {
        terms_table = t(terms_table)
      }
    }

    output$association_table_preview_table = renderDataTable({
      DT::datatable(association_table, options = list(paging = TRUE, pageLength = 25))
    })

    output$terms_table_preview_table = renderDataTable({
      DT::datatable(terms_table, options = list(paging = TRUE, pageLength = 25))
    })

  })

  # Download associations table
  dl_feature_table = shiny::reactiveValues(
    name = NULL,
    table = NULL
  )

  session$userData[[id]]$download_feature_table = shiny::observeEvent(c(input$feat_table_select) , {
    dl_feature_table$name = timestamped_name("feature_table.csv")
    dl_feature_table$table = table_switch(table_name = input$feat_table_select,
                                          r6 = r6)
  })

  output$download_feature_table = shiny::downloadHandler(
    filename = shiny::reactive(dl_feature_table$name),
    content = function(file_name) {
      write.csv(dl_feature_table$table, file_name, na = "")
    }
  )

  # Download terms table
  dl_terms_table = shiny::reactiveValues(
    name = NULL,
    table = NULL
  )

  session$userData[[id]]$download_terms_table = shiny::observeEvent(c(input$annotations_table_select) , {
    dl_terms_table$name = timestamped_name(paste0(input$annotations_table_select, "_terms.csv"))
    dl_terms_table$table = r6$tables$external_enrichment_tables[[input$annotations_table_select]]$terms_table
  })

  output$download_terms_table = shiny::downloadHandler(
    filename = shiny::reactive(dl_terms_table$name),
    content = function(file_name) {
      write.csv(dl_terms_table$table, file_name, na = "")
    }
  )

  # Remove annotations table
  session$userData[[id]]$remove_annotations_table = shiny::observeEvent(c(input$remove_annotations_table) , {
    print(input$annotations_table_select)
    r6$tables$external_enrichment_tables[[input$annotations_table_select]] = NULL
    if (length(r6$tables$external_enrichment_tables[[input$annotations_table_select]]) > 0) {
      shiny::updateSelectInput(
        inputId = 'annotations_table_select',
        choices = names(r6$tables$external_feature_tables)
      )
    } else {
      shiny::updateSelectInput(
        inputId = 'annotations_table_select',
        choices = character(0)
      )

      # association_table_preview_table

    }

  })


  # Download data table
  dl_data_table = shiny::reactiveValues(
    name = NULL,
    table = NULL
  )

  session$userData[[id]]$download_datatable = shiny::observeEvent(c(input$select_data_table) , {
    shiny::req(r6$tables$raw_data)
    dl_data_table$name = timestamped_name(paste0(stringr::str_replace_all(input$select_data_table, " ", "_"), ".csv"))
    dl_data_table$table = table_switch(input$select_data_table, r6)
  })

  output$download_datatable = shiny::downloadHandler(
    filename = shiny::reactive(dl_data_table$name),
    content = function(file_name) {
      write.csv(dl_data_table$table, file_name, na = "")
    }
  )




  #------------------------------------------ Functional analysis rendering ----
  output$functional_analysis_ui = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h4('Data preparation'),
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width= 3,
          shiny::selectInput(
            inputId = ns('gseaprep_table_select'),
            label = 'Select table',
            choices = NULL,
            width = '100%'
          )
        ),
        shiny::column(
          width= 3,
          shiny::selectInput(
            inputId = ns('gseaprep_group_col'),
            label = 'Group column',
            choices = NULL,
            width = '100%'
          )
        ),
        shiny::column(
          width = 3,
          shiny::selectInput(
            inputId = ns('gseaprep_groups'),
            label = 'Select two groups',
            choices = NULL,
            width = '100%',
            multiple = T
          )
        ),
        shiny::column(
          width = 3,
          shiny::selectInput(
            inputId = ns('gseaprep_method'),
            label = 'FC method',
            choices = c('median', 'mean'),
            selected = 'mean',
            width = '100%'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectInput(
            inputId = ns('gseaprep_test'),
            label = 'Test',
            choices = c('Wilcoxon', 't-Test'),
            selected = 't-Test',
            width = '100%'
          )
        ),
        shiny::column(
          width = 6,
          shiny::selectInput(
            inputId = ns('gseaprep_adjustment'),
            label = 'Adjustment',
            choices = c('None', 'Benjamini-Hochberg'),
            selected = 'Benjamini-Hochberg'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::sliderInput(
            inputId = ns('gseaprep_pval'),
            label = 'p-value cutoff (Features, only for ORA)',
            min = 0.01,
            max = 0.9,
            value = 0.05,
            step = 0.01,
            width = '100%'
          ),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;")
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h4('Geneset enrichment analysis'),

          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                inputId = ns('gsea_go'),
                label = 'GO ontology',
                choices = c('ALL', 'BP', 'MF', 'CC'),
                selected = 'ALL'
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectInput(
                inputId = ns('gsea_adjustment'),
                label = 'Adjustment',
                choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                selected = 'BH'
              )
            )
          ),

          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::textInput(
                inputId = ns('gsea_min_size'),
                label = 'Min. geneset size',
                value = 3
              )
            ),
            shiny::column(
              width = 4,
              shiny::textInput(
                inputId = ns('gsea_max_size'),
                label = 'Max. geneset size',
                value = 800
              )
            ),
            shiny::column(
              width = 4,
              shiny::textInput(
                inputId = ns('gsea_showcat'),
                label = 'Show category',
                value = 200,
                width = '100%'
              )
            )
          ),

          shiny::sliderInput(
            inputId = ns('gsea_pval'),
            label = 'p-value cutoff (GO terms)',
            min = 0.01,
            max = 0.9,
            value = 0.05,
            step = 0.01,
            width = '100%'
          ),
          shinyWidgets::actionBttn(
            inputId = ns('run_gsea'),
            label = "Run GSEA",
            style = "material-flat",
            color = 'success',
            block = T,
            icon = icon("play")
          )
        ),
        shiny::column(
          width = 6,
          shiny::h4('Over representation analysis'),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(
                inputId = ns('or_go_ont'),
                label = 'GO ontology',
                choices = c('ALL', 'BP', 'MF', 'CC'),
                selected = 'ALL',
                width = '100%'
              )
            ),
            shiny::column(
              width = 6,
              shiny::selectInput(
                inputId = ns('or_pval_adjustment'),
                label = 'Adjustment',
                choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                selected = "BH",
                width = '100%'
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 4,
              shiny::textInput(
                inputId = ns('or_min_gssize'),
                label = 'Min. geneset size',
                value = 10,
                width = '100%'
              )
            ),
            shiny::column(
              width = 4,
              shiny::textInput(
                inputId = ns('or_max_gssize'),
                label = 'Max. geneset size',
                value = 500,
                width = '100%'
              )
            ),
            shiny::column(
              width = 4,
              shiny::textInput(
                inputId = ns('or_fc_threshold'),
                label = 'FC cutoff',
                value = 2,
                width = '100%'
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::sliderInput(
                inputId = ns('or_pval_cutoff'),
                label = 'p-value cutoff (GO terms)',
                min = 0.01,
                max = 0.9,
                value = 0.05,
                step = 0.01,
                width = '100%'
              )
            ),
            shiny::column(
              width = 6,
              shiny::sliderInput(
                inputId = ns('or_qval_cutoff'),
                label = 'q-value cutoff (GO terms)',
                min = 0.01,
                max = 0.9,
                value = 0.05,
                step = 0.01,
                width = '100%'
              )
            )

          ),
          shiny::fluidRow(
            shinyWidgets::actionBttn(
              inputId = ns('run_or'),
              label = "Run ORA",
              style = "material-flat",
              color = 'success',
              block = T,
              icon = icon("play")
            )
          )
        )
      )
    )
  })
  #--------------------------------------------- Functional analysis server ----

  shiny::observe({
    shiny::req(input$skeleton_ui)
    if (input$skeleton_ui == "Functional analysis") {

      shiny::updateSelectInput(
        inputId = 'gseaprep_table_select',
        choices = c('Raw data table', 'Total normalized table', 'Z-scored table', 'Z-scored total normalized table'),
        selected = 'Total normalized table'
      )

      shiny::updateSelectInput(
        inputId = 'gseaprep_group_col',
        choices = colnames(r6$tables$raw_meta),
        selected = input$select_group_col
      )

      shiny::updateSelectInput(
        inputId = 'gsea_go',
        choices = c('ALL', 'BP', 'MF', 'CC', names(r6$tables$feature_list))
      )

      shiny::updateSelectInput(
        inputId = 'or_go_ont',
        choices = c('ALL', 'BP', 'MF', 'CC', names(r6$tables$feature_list))
      )

    }
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
                                             choices = proteomics_plot_list(),
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






  #-------------------------------------------------- Visualize data server ----

  # Initialise dimensions object
  dimensions_obj = shiny::reactiveValues(
    x_box = module_controler$dims$x_box,
    y_box = module_controler$dims$y_box,
    x_plot = module_controler$dims$x_plot,
    y_plot = module_controler$dims$y_plot,
    x_plot_full = module_controler$dims$x_plot_full,
    y_plot_full = module_controler$dims$y_plot_full,
    xpx_total = shinybrowser::get_width(),
    ypx_total = shinybrowser::get_height(),
    xbs = 12,
    xpx = shinybrowser::get_width(),
    ypx = shinybrowser::get_height()
  )

  color_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = 'Spectral'))(40)

  # Plotting events
  prot_volcano_plot_events(r6, dimensions_obj, color_palette, input, output, session)
  prot_heatmap_events(r6, dimensions_obj, color_palette, input, output, session)
  prot_pca_events(r6, dimensions_obj, color_palette, input, output, session)
  prot_dot_plot_events(r6, dimensions_obj, color_palette, input, output, session)
  prot_cnet_plot_events(r6, dimensions_obj, color_palette, input, output, session)
  prot_ridge_plot_events(r6, dimensions_obj, color_palette, input, output, session)
  prot_emap_plot_events(r6, dimensions_obj, color_palette, input, output, session)



  session$userData[[id]]$showPlots = shiny::observeEvent(input$showPlots,{

    # Update x dimensions in px and bs, and y in px
    if (length(input$showPlots) < 2) {
      dimensions_obj$xbs = 12
      dimensions_obj$xpx = shinybrowser::get_width()
      dimensions_obj$ypx = shinybrowser::get_height()
    } else if (length(input$showPlots) == 2) {
      dimensions_obj$xbs  = 6
      dimensions_obj$xpx = shinybrowser::get_width()/2
      dimensions_obj$ypx = shinybrowser::get_height()
    } else {
      dimensions_obj$xbs  = 6
      dimensions_obj$xpx = shinybrowser::get_width()/2
      dimensions_obj$ypx = shinybrowser::get_height()/2.2
    }

    # Display plot boxes
    print_tm(m, paste0("Plot selection: ", paste(input$showPlots, collapse = ", ")))
    if (length(input$showPlots) == 1) {
      plot_one_prot(r6 = r6,
                    dimensions_obj = dimensions_obj,
                    selection_list = input$showPlots,
                    input = input,
                    output = output,
                    session = session)
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "showPlots",
        disabledChoices = input$showPlots
      )

    } else if (length(input$showPlots) == 2) {
      plot_two_prot(r6 = r6,
                    dimensions_obj = dimensions_obj,
                    selection_list = input$showPlots,
                    input = input,
                    output = output,
                    session = session)

    } else if (length(input$showPlots) == 3) {
      plot_three_prot(r6 = r6,
                      dimensions_obj = dimensions_obj,
                      selection_list = input$showPlots,
                      input = input,
                      output = output,
                      session = session)

    } else if (length(input$showPlots) >= 4) {
      plot_four_prot(r6 = r6,
                     dimensions_obj = dimensions_obj,
                     selection_list = input$showPlots,
                     input = input,
                     output = output,
                     session = session)

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "showPlots",
        disabledChoices = setdiff(unname(lipidomics_plot_list()), input$showPlots)
      )

    }



    if ((length(input$showPlots) > 1) & (length(input$showPlots) < 4)) {
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "showPlots",
        disabledChoices = NULL
      )
    }

  })


  session$userData[[id]]$clear_plots = shiny::observeEvent(input$clear_plots, {
    print_tm(m, "Clearing plots")
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "showPlots",
      disabled = FALSE,
      selected = character(0))
    output$plotbox_field = shiny::renderUI(
      NULL
    )
  })


  #------------------------------------------- Geneset enrichment rendering ----

  output$geneset_enrichment_ui = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 11,
          shinyWidgets::checkboxGroupButtons(inputId = ns("show_plots_gsea"),
                                             label = NULL,
                                             status = "default",
                                             choices = gsea_plot_list(),
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                             size = "normal",
                                             justified = TRUE)
        ),
        shiny::column(
          width = 1,
          shinyWidgets::actionBttn(inputId = ns("clear_plots_gsea"),
                                   label = "Clear",
                                   style = "material-flat",
                                   color = "danger",
                                   block = T,
                                   icon = icon("x"))
        )
      ),
      shiny::uiOutput(
        outputId = ns("gsea_plotbox_field")
      )
    )
  })

  #---------------------------------------------- Geneset enrichment server ----

  session$userData[[id]]$select_feature_type = shiny::observeEvent(input$select_feature_type, {
    if (r6$preloaded_data) {return()}
    print_tm(m, paste0('GSEA: feature ID type set to ', input$select_feature_type))
    r6$indices$feature_id_type = input$select_feature_type
  })


  session$userData[[id]]$run_gsea = shiny::observeEvent(input$run_gsea, {
    shiny::req(length(input$gseaprep_groups) == 2)
    print_tm(m, "GSEA started")
    shinyjs::disable("run_gsea")

    base::tryCatch({
      r6$get_prot_list(data_table = table_switch(input$gseaprep_table_select, r6),
                       group_col = input$gseaprep_group_col,
                       group_1 = input$gseaprep_groups[1],
                       group_2 = input$gseaprep_groups[2],
                       used_function = input$gseaprep_method,
                       test = input$gseaprep_test,
                       context = 'gsea')

      if (input$gsea_go %in% c('ALL', 'BP', 'MF', 'CC')) {
        ont = input$gsea_go
        custom_col = NULL
      } else if(ont == "") {
        ont = NULL
        custom_col = NULL
      } else {
        ont = NULL
        custom_col = input$gsea_go
      }
      r6$get_gsea_object(ont = ont,
                         custom_col = custom_col,
                         minGSSize = as.numeric(input$gsea_min_size),
                         maxGSSize = as.numeric(input$gsea_max_size),
                         p_value_cutoff = input$gsea_pval,
                         verbose = TRUE,
                         OrgDb = "org.Hs.eg.db",
                         pAdjustMethod = input$gsea_adjustment,
                         termsim_showcat = as.numeric(input$gsea_showcat))


      if (nrow(r6$tables$gsea_object@result) == 0) {
        print_tm(m, "GSEA failed: no term enriched under specific pvalueCutoff")
      } else {
        print_tm(m, "GSEA finished")
      }
    },error=function(e){
      print_tm(r6$name, 'GSEA failed.')
    },finally={}
    )

    shinyjs::enable("run_gsea")
  })

  session$userData[[id]]$run_or = shiny::observeEvent(input$run_or, {
    shiny::req(length(input$gseaprep_groups) == 2)
    print_tm(m, "OR started")
    shinyjs::disable("run_or")
    r6$get_prot_list(data_table = table_switch(input$gseaprep_table_select, r6),
                     group_col = input$gseaprep_group_col,
                     group_1 = input$gseaprep_groups[1],
                     group_2 = input$gseaprep_groups[2],
                     used_function = input$gseaprep_method,
                     test = input$gseaprep_test,
                     context = 'ora')

    if (input$or_go_ont %in% c('ALL', 'BP', 'MF', 'CC')) {
      ont = input$or_go_ont
      custom_col = NULL
    } else if (ont == "") {
      ont = NULL
      custom_col = NULL
    } else {
      ont = NULL
      custom_col = input$or_go_ont
    }


    r6$over_representation_analysis(custom_col = custom_col,
                                    ont = ont,
                                    pval_cutoff_features = input$gseaprep_pval,
                                    padjust_features = input$gseaprep_adjustment,
                                    pval_cutoff = input$or_pval_cutoff,
                                    pAdjustMethod = input$or_pval_adjustment,
                                    fc_threshold = as.numeric(input$or_fc_threshold),
                                    qval_cutoff = input$or_qval_cutoff,
                                    minGSSize = as.numeric(input$or_min_gssize),
                                    maxGSSize = as.numeric(input$or_max_gssize))

    if (!is.null(r6$tables$go_enrich)) {
      results = nrow(r6$tables$go_enrich@result)
      if (results == 0) {
        print_tm(m, 'WARNING: no over-representation under selected parameters')
      } else {
        print_tm(m, paste0('Over-representation successful: ', results, ' terms'))
      }
      print_tm(m, "OR finished")
    } else {
      print_tm(m, 'No over represented features, returning.')
    }

    shinyjs::enable("run_or")

  })


  # Initialise dimensions object
  dimensions_obj_gsea = shiny::reactiveValues(
    x_box = module_controler$dims$x_box,
    y_box = module_controler$dims$y_box,
    x_plot = module_controler$dims$x_plot,
    y_plot = module_controler$dims$y_plot,
    x_plot_full = module_controler$dims$x_plot_full,
    y_plot_full = module_controler$dims$y_plot_full,
    xpx_total = shinybrowser::get_width(),
    ypx_total = shinybrowser::get_height(),
    xbs = 12,
    xpx = shinybrowser::get_width(),
    ypx = shinybrowser::get_height()
  )

  # Plot selection
  prot_dot_plot_events(r6, dimensions_obj_gsea, color_palette, input, output, session)
  prot_ridge_plot_events(r6, dimensions_obj_gsea, color_palette, input, output, session)
  prot_cnet_plot_events(r6, dimensions_obj_gsea, color_palette, input, output, session)
  prot_emap_plot_events(r6, dimensions_obj_gsea, color_palette, input, output, session)

  # Plot selection
  session$userData[[id]]$show_plots_gsea = shiny::observeEvent(input$show_plots_gsea, {
    shiny::req(r6$tables$gsea_object)

    # Update x dimensions in px and bs, and y in px
    if (length(input$show_plots_gsea) < 2) {
      dimensions_obj_gsea$xbs = 12
      dimensions_obj_gsea$xpx = shinybrowser::get_width()
      dimensions_obj_gsea$ypx = shinybrowser::get_height()
    } else if (length(input$show_plots_gsea) == 2) {
      dimensions_obj_gsea$xbs  = 6
      dimensions_obj_gsea$xpx = shinybrowser::get_width()/2
      dimensions_obj_gsea$ypx = shinybrowser::get_height()
    } else {
      dimensions_obj_gsea$xbs  = 6
      dimensions_obj_gsea$xpx = shinybrowser::get_width()/2
      dimensions_obj_gsea$ypx = shinybrowser::get_height()/2.2
    }

    # Plots selected: 1 to 4
    print_tm(m, paste0("Plot selection: ", paste(input$show_plots_gsea, collapse = ", ")))
    if (length(input$show_plots_gsea) == 1) {
      plot_one_prot_gsea(r6 = r6,
                         dimensions_obj = dimensions_obj_gsea,
                         selection_list = input$show_plots_gsea,
                         input = input,
                         output = output,
                         session = session)
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_gsea",
        disabledChoices = input$show_plots_gsea
      )

    } else if (length(input$show_plots_gsea) == 2) {
      plot_two_prot_gsea(r6 = r6,
                         dimensions_obj = dimensions_obj_gsea,
                         selection_list = input$show_plots_gsea,
                         input = input,
                         output = output,
                         session = session)

    } else if (length(input$show_plots_gsea) == 3) {
      plot_three_prot_gsea(r6 = r6,
                           dimensions_obj = dimensions_obj_gsea,
                           selection_list = input$show_plots_gsea,
                           input = input,
                           output = output,
                           session = session)

    } else if (length(input$show_plots_gsea) >= 4) {
      plot_four_prot_gsea(r6 = r6,
                          dimensions_obj = dimensions_obj_gsea,
                          selection_list = input$show_plots_gsea,
                          input = input,
                          output = output,
                          session = session)

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_gsea",
        disabledChoices = setdiff(unname(gsea_plot_list()), input$show_plots_gsea)
      )

    }
    if ((length(input$show_plots_gsea) > 1) & (length(input$show_plots_gsea) < 4)) {
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_gsea",
        disabledChoices = NULL
      )
    }
  })


  session$userData[[id]]$clear_plots_gsea = shiny::observeEvent(input$clear_plots_gsea, {
    print_tm(m, "Clearing plots")
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "show_plots_gsea",
      disabled = FALSE,
      selected = character(0))
    output$gsea_plotbox_field = shiny::renderUI(
      NULL
    )
  })



  #------------------------------------------ Over-representation rendering ----

  output$over_representation_ui = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        shiny::column(
          width = 11,
          shinyWidgets::checkboxGroupButtons(inputId = ns("show_plots_or"),
                                             label = NULL,
                                             status = "default",
                                             choices = or_plot_list(),
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                             size = "normal",
                                             justified = TRUE)
        ),
        shiny::column(
          width = 1,
          shinyWidgets::actionBttn(inputId = ns("clear_plots_or"),
                                   label = "Clear",
                                   style = "material-flat",
                                   color = "danger",
                                   block = T,
                                   icon = icon("x"))
        )
      ),
      shiny::uiOutput(
        outputId = ns("or_plotbox_field")
      )
    )
  })

  #--------------------------------------------- Over-representation server ----

  # Initialise dimensions object
  dimensions_obj_or = shiny::reactiveValues(
    x_box = module_controler$dims$x_box,
    y_box = module_controler$dims$y_box,
    x_plot = module_controler$dims$x_plot,
    y_plot = module_controler$dims$y_plot,
    x_plot_full = module_controler$dims$x_plot_full,
    y_plot_full = module_controler$dims$y_plot_full,
    xpx_total = shinybrowser::get_width(),
    ypx_total = shinybrowser::get_height(),
    xbs = 12,
    xpx = shinybrowser::get_width(),
    ypx = shinybrowser::get_height()
  )

  # Plot selection
  prot_or_dot_plot_events(r6, dimensions_obj_or, color_palette, input, output, session)
  prot_or_bar_plot_events(r6, dimensions_obj_or, color_palette, input, output, session)
  prot_or_cnet_plot_events(r6, dimensions_obj_or, color_palette, input, output, session)
  prot_or_emap_plot_events(r6, dimensions_obj_or, color_palette, input, output, session)

  # Plot selection
  session$userData[[id]]$show_plots_or = shiny::observeEvent(input$show_plots_or, {
    shiny::req(r6$tables$go_enrich)

    # Update x dimensions in px and bs, and y in px
    if (length(input$show_plots_or) < 2) {
      dimensions_obj_or$xbs = 12
      dimensions_obj_or$xpx = shinybrowser::get_width()
      dimensions_obj_or$ypx = shinybrowser::get_height()
    } else if (length(input$show_plots_or) == 2) {
      dimensions_obj_or$xbs  = 6
      dimensions_obj_or$xpx = shinybrowser::get_width()/2
      dimensions_obj_or$ypx = shinybrowser::get_height()
    } else {
      dimensions_obj_or$xbs  = 6
      dimensions_obj_or$xpx = shinybrowser::get_width()/2
      dimensions_obj_or$ypx = shinybrowser::get_height()/2.2
    }

    # Plots selected: 1 to 4
    print_tm(m, paste0("Plot selection: ", paste(input$show_plots_or, collapse = ", ")))
    if (length(input$show_plots_or) == 1) {
      plot_one_prot_or(r6 = r6,
                       dimensions_obj = dimensions_obj_or,
                       selection_list = input$show_plots_or,
                       input = input,
                       output = output,
                       session = session)
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_or",
        disabledChoices = input$show_plots_or
      )

    } else if (length(input$show_plots_or) == 2) {
      plot_two_prot_or(r6 = r6,
                       dimensions_obj = dimensions_obj_or,
                       selection_list = input$show_plots_or,
                       input = input,
                       output = output,
                       session = session)

    } else if (length(input$show_plots_or) == 3) {
      plot_three_prot_or(r6 = r6,
                         dimensions_obj = dimensions_obj_or,
                         selection_list = input$show_plots_or,
                         input = input,
                         output = output,
                         session = session)

    } else if (length(input$show_plots_or) >= 4) {
      plot_four_prot_or(r6 = r6,
                        dimensions_obj = dimensions_obj_or,
                        selection_list = input$show_plots_or,
                        input = input,
                        output = output,
                        session = session)

      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_or",
        disabledChoices = setdiff(unname(or_plot_list()), input$show_plots_or)
      )

    }
    if ((length(input$show_plots_or) > 1) & (length(input$show_plots_or) < 4)) {
      shinyWidgets::updateCheckboxGroupButtons(
        session = session,
        inputId = "show_plots_or",
        disabledChoices = NULL
      )
    }
  })


  session$userData[[id]]$clear_plots_or = shiny::observeEvent(input$clear_plots_or, {
    print_tm(m, "Clearing plots")
    shinyWidgets::updateCheckboxGroupButtons(
      session = session,
      inputId = "show_plots_or",
      disabled = FALSE,
      selected = character(0))
    output$or_plotbox_field = shiny::renderUI(
      NULL
    )
  })



}
