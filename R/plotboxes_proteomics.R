
#------------------------------------------------------------- Volcano plot ----

prot_volcano_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Volcano plot: generating plot.")

  if (input$volcano_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$get_volcano_table(data_table = table_switch(input$volcano_plot_tables, r6),
                       group_col = input$volcano_plot_metacol,
                       used_function =  input$volcano_plot_function,
                       test = input$volcano_plot_test,
                       group_1 = input$volcano_plot_metagroup[1],
                       group_2 = input$volcano_plot_metagroup[2])

  r6$plot_volcano(data_table = r6$tables$volcano_table,
                  adjustment = adjustment_switch(input$volcano_plot_adjustment),
                  colour_list = colour_list,
                  group_1 = input$volcano_plot_metagroup[1],
                  group_2 = input$volcano_plot_metagroup[2],
                  width = width,
                  height = height)

}


prot_volcano_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Volcano plot: spawning plot.")
  output$volcano_plot_plot = plotly::renderPlotly({
    r6$plots$volcano_plot
    plotly::config(r6$plots$volcano_plot, toImageButtonOptions = list(format= format,
                                                                 filename= timestamped_name('volcano_plot'),
                                                                 height= NULL,
                                                                 width= NULL,
                                                                 scale= 1))
  })
}

prot_volcano_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "volcano_plot",
                 label = "Volcano plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


prot_volcano_plot_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "Volcano plot: START.")

  # Set UI
  output$volcano_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("volcano_plot_tables"),
        label = "Select data table",
        choices = c('Raw data table', 'Total normalized table'),
        selected = r6$params$volcano_plot$data_table
      ),
      shiny::selectInput(
        inputId = ns("volcano_plot_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$volcano_plot$group_col
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_metagroup"),
        label = "Select two groups to compare",
        choices = unique(r6$tables$raw_meta[,r6$params$volcano_plot$group_col]),
        selected = r6$params$volcano_plot$groups,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_function"),
        label = "FC function",
        choices = c("median", "mean"),
        selected = r6$params$volcano_plot$selected_function,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_test"),
        label = "Select test",
        choices = c("Wilcoxon", "t-Test"),
        selected = r6$params$volcano_plot$selected_test,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_adjustment"),
        label = "Select adjustment",
        choices = c("None", "Benjamini-Hochberg"),
        selected = r6$params$volcano_plot$adjustment,
        multiple = FALSE
      ),
      shiny::actionButton(
        inputId = ns("volcano_feature_select"),
        label = "Save selection",
        width = "100%"
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("volcano_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$volcano_plot$img_format,
        width = "100%"),

      shiny::fluidRow(
        shiny::downloadButton(
          outputId = ns("download_volcano_table"),
          label = "Download associated table",
          style = "width:50%;"
        ),
        shiny::downloadButton(
          outputId = ns("download_volcano_subtable"),
          label = "Download selection",
          style = "width:50%;"
        )
      )


    )
  })
}

prot_volcano_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # auto-update selected groups
  shiny::observeEvent(input$volcano_plot_metacol,{
    shiny::updateSelectizeInput(
      inputId = "volcano_plot_metagroup",
      session = session,
      choices = unique(r6$tables$raw_meta[,input$volcano_plot_metacol]),
      selected = unique(r6$tables$raw_meta[,input$volcano_plot_metacol])[c(1,2)]
    )
  })

  shiny::observeEvent(c(shiny::req(length(input$volcano_plot_metagroup) == 2), input$volcano_plot_tables, input$volcano_plot_function, input$volcano_plot_adjustment, input$volcano_plot_test, input$volcano_plot_img_format), {
    print_tm(r6$name, "Volcano plot: Updating params...")

    r6$param_volcano_plot(data_table = input$volcano_plot_tables,
                          adjustment = input$volcano_plot_adjustment,
                          group_col = input$volcano_plot_metacol,
                          groups = input$volcano_plot_metagroup,
                          selected_function = input$volcano_plot_function,
                          selected_test = input$volcano_plot_test,
                          img_format = input$volcano_plot_img_format)

    base::tryCatch({
      prot_volcano_plot_generate(r6, color_palette, dimensions_obj, input)
      prot_volcano_plot_spawn(r6, format = input$volcano_plot_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Volcano plot: error, missing data.')
    },finally={}
    )
  })

  # Save selection
  shiny::observeEvent(input$volcano_feature_select, {
    print_tm(r6$name, "Volcano plot: saving selection.")
    volcano_selection = plotly::event_data(
      "plotly_selected"
    )
    if (is.null(volcano_selection)){
      print_tm(r6$name, "Brushed points appear here (double-click to clear)")
    }else {
      r6$slice_volcano_table(
        x = volcano_selection[3][[1]],
        y = volcano_selection[4][[1]],
        x_col = "log2_fold_change",
        y_col = adjustment_switch(input$volcano_plot_adjustment)
      )
    }
  })

  # Export volcano table
  output$download_volcano_table = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_table, file_name)
    }
  )

  output$download_volcano_subtable = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_table_selection.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_table_slice, file_name)
    }
  )


  # Expanded boxes
  volcano_plot_proxy = plotly::plotlyProxy(outputId = "volcano_plot_plot",
                                           session = session)

  shiny::observeEvent(input$volcano_plot_plotbox,{
    if (input$volcano_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = volcano_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = volcano_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#----------------------------------------------------------------- Heat map ----

prot_heatmap_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Heatmap: generating plot.")
  if (input$heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_heatmap(data_table = table_switch(input$heatmap_dataset, r6),
                  impute = input$heatmap_impute,
                  meta_table = r6$tables$raw_meta,
                  cluster_rows = input$heatmap_cluster_samples,
                  cluster_cols = input$heatmap_cluster_features,
                  row_annotations = input$heatmap_map_rows,
                  apply_da = input$heatmap_apply_da,
                  group_column_da = input$heatmap_group_col_da,
                  alpha_da = input$heatmap_alpha_da,
                  width = dimensions_obj$xpx * dimensions_obj$x_plot,
                  height = dimensions_obj$ypx * dimensions_obj$y_plot)

}

prot_heatmap_spawn = function(r6, format, output) {
  print_tm(r6$name, "Heatmap: spawning plot.")
  output$heatmap_plot = plotly::renderPlotly({
    r6$plots$heatmap
    plotly::config(r6$plots$heatmap, toImageButtonOptions = list(format= format,
                                                                      filename= timestamped_name('heatmap_plot'),
                                                                      height= NULL,
                                                                      width= NULL,
                                                                      scale= 1),
                   modeBarButtonsToRemove  = list("autoScale"))
  })
}


prot_heatmap_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "heatmap",
                 label = "Heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


prot_heatmap_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "Heatmap: START.")

  output$heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("heatmap_dataset"),
        label = "Select dataset",
        choices = c('Z-scored table'),
        selected = r6$params$heatmap$dataset
      ),

      shiny::fluidRow(
        shiny::column(
          width = 4,
          shinyWidgets::switchInput(inputId = ns("heatmap_impute"),
                                    label = "Impute missing",
                                    value = r6$params$heatmap$imputation,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::switchInput(inputId = ns("heatmap_cluster_samples"),
                                    label = "Cluster samples",
                                    value = r6$params$heatmap$cluster_samples,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::switchInput(inputId = ns("heatmap_cluster_features"),
                                    label = "Cluster features",
                                    value = r6$params$heatmap$cluster_features,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
          )
        )
      ),
      shiny::selectizeInput(
        inputId = ns("heatmap_map_rows"),
        label = "Map sample data",
        multiple = TRUE,
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$heatmap$map_sample_data
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h3("Discriminant analysis")
        ),
        shiny::column(
          width = 6,
          shinyWidgets::switchInput(inputId = ns("heatmap_apply_da"),
                                    label = "Apply",
                                    value = r6$params$heatmap$apply_da,
                                    disabled = TRUE)
        )
      ),

      shiny::selectizeInput(inputId = ns("heatmap_group_col_da"),
                            label = "Group column",
                            choices = colnames(r6$tables$raw_meta),
                            selected = r6$params$heatmap$group_column_da,
                            multiple = FALSE,
                            width = "100%"),
      shiny::sliderInput(inputId = ns("heatmap_alpha_da"),
                         label = "Alpha",
                         min = 0,
                         max = 0.99,
                         value = r6$params$heatmap$alpha_da,
                         step = 0.01,
                         width = "100%"),
      shiny::actionButton(
        inputId = ns("heatmap_run"),
        label = "Generate heatmap",
        width = "100%"
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("heatmap_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$heatmap$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_heatmap_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

prot_heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(input$heatmap_run,{
    shinyjs::disable("heatmap_run")
    print_tm(r6$name, "Heatmap: Updating params...")

    r6$param_heatmap(dataset = input$heatmap_dataset,
                     impute = input$heatmap_impute,
                     cluster_samples = input$heatmap_cluster_samples,
                     cluster_features = input$heatmap_cluster_features,
                     map_sample_data = input$heatmap_map_rows,
                     group_column_da = input$heatmap_group_col_da,
                     apply_da = input$heatmap_apply_da,
                     alpha_da = input$heatmap_alpha_da,
                     img_format = input$heatmap_img_format)

    base::tryCatch({
      prot_heatmap_generate(r6, color_palette, dimensions_obj, input)
      prot_heatmap_spawn(r6, input$heatmap_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Heatmap: error, missing data.')
      shinyjs::enable("heatmap_run")
    },finally={}
    )

    shinyjs::enable("heatmap_run")
  })


  # Download associated table
  output$download_heatmap_table = shiny::downloadHandler(
    filename = function(){timestamped_name("heatmap_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$heatmap_table, file_name)
    }
  )

  # Expanded boxes
  heatmap_proxy = plotly::plotlyProxy(outputId = "heatmap_plot",
                                      session = session)

  shiny::observeEvent(input$heatmap_plotbox,{
    if (input$heatmap_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}



#---------------------------------------------------------------------- PCA ----

prot_pca_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "PCA: generating plot.")

  if (input$pca_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_pca(data_table = table_switch(input$pca_dataset, r6),
              group_column = input$pca_metacol,
              apply_da = input$pca_apply_da,
              alpha_da = input$pca_alpha_da,
              width = width,
              height = height,
              colour_list = colour_list)

}

prot_pca_spawn = function(r6, format, output) {
  print_tm(r6$name, "PCA: spawning plot.")
  output$pca_plot = plotly::renderPlotly({
    r6$plots$pca_plot
    plotly::config(r6$plots$pca_plot, toImageButtonOptions = list(format= format,
                                                                 filename= timestamped_name('pca_plot'),
                                                                 height= NULL,
                                                                 width= NULL,
                                                                 scale= 1))
  })
}

prot_pca_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "pca",
                 label = "PCA",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


prot_pca_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "PCA: START.")

  output$pca_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("pca_dataset"),
        label = "Select dataset",
        choices = c('Z-scored table', 'Z-scored total normalized table'),
        selected = r6$params$pca$dataset
      ),
      shiny::selectInput(
        inputId = ns("pca_metacol"),
        label = "Select metadata column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$pca$group_column
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::h4("Discriminant analysis"),

      shinyWidgets::switchInput(inputId = ns("pca_apply_da"),
                                label = "Apply discriminant analysis",
                                value = r6$params$pca$apply_da,
                                width = "100%"),

      shiny::sliderInput(inputId = ns("pca_alpha_da"),
                         label = "Alpha",
                         min = 0,
                         max = 0.99,
                         value = r6$params$pca$alpha_da,
                         step = 0.01,
                         width = "100%"),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::selectInput(
          inputId = ns("pca_img_format"),
          label = "Image format",
          choices = c("png", "svg", "jpeg", "webp"),
          selected = r6$params$pca$img_format,
          width = "100%"),
        shiny::downloadButton(
          outputId = ns("download_pca_scores_table"),
          label = "Download scores table",
          style = "width:50%;"
        ),
        shiny::downloadButton(
          outputId = ns("download_pca_loadings_table"),
          label = "Download loadings table",
          style = "width:50%;"
        )
      )

    )
  })
}

prot_pca_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(c(input$pca_dataset, input$pca_metacol, input$pca_apply_da, input$pca_alpha_da, input$pca_img_format),{
    print_tm(r6$name, "PCA: Updating params...")

    r6$param_pca(dataset = input$pca_dataset,
                 group_column = input$pca_metacol,
                 apply_da = input$pca_apply_da,
                 alpha_da = input$pca_alpha_da,
                 img_format = input$pca_img_format)

    base::tryCatch({
      prot_pca_generate(r6, color_palette, dimensions_obj, input)
      prot_pca_spawn(r6, input$pca_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'PCA: error, missing data.')
    },finally={}
    )

  })


  # Download associated tables
  output$download_pca_scores_table = shiny::downloadHandler(
    filename = function(){timestamped_name("pca_scores_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$pca_scores_table, file_name)
    }
  )
  output$download_pca_loadings_table = shiny::downloadHandler(
    filename = function(){timestamped_name("pca_loadings_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$pca_loadings_table, file_name)
    }
  )

  # Expanded boxes
  pca_proxy = plotly::plotlyProxy(outputId = "pca_plot",
                                      session = session)

  shiny::observeEvent(input$pca_plotbox,{
    if (input$pca_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = pca_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = pca_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#------------------------------------------------------------- gsea dotplot ----
prot_dot_plot_generate = function(r6, colour_list, dimensions_obj, input) {

  if (input$dot_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  print_tm(r6$name, "Dot plot: generating plot.")
  r6$plot_dot_plot(showCategory = as.numeric(input$dot_plot_showcat),
                   mode = input$dot_plot_mode,
                   width = width,
                   height = height)
}

prot_dot_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Dot plot: spawning plot.")

  output$dot_plot_plot = plotly::renderPlotly({
    r6$plots$dotplot
    plotly::config(r6$plots$dotplot, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_name('dotplot'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}

prot_dot_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "dot_plot",
                 label = "Dot plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


prot_dot_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Dot plot: START.")

  output$dot_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns("dot_plot_showcat"),
        label = "Show category",
        value = r6$params$dot_plot$showCategory
      ),
      shiny::selectInput(
        inputId = ns("dot_plot_mode"),
        label = "Mode",
        choices = c("Both", "Activated", "Suppressed"),
        selected = r6$params$dot_plot$mode
      ),
      shiny::selectInput(
        inputId = ns("dot_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$dot_plot$img_format,
        width = "100%"),
    )
  })
}

prot_dot_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(c(input$dot_plot_showcat, input$dot_plot_mode, input$dot_plot_img_format),{

    # Update parameters
    print_tm(r6$name, "Dot plot: Updating params...")

    r6$param_dot_plot(showCategory = as.numeric(input$dot_plot_showcat),
                      mode = input$dot_plot_mode,
                      img_format = input$dot_plot_img_format)

    # Produce the plot
    base::tryCatch({
      prot_dot_plot_generate(r6, color_palette, dimensions_obj, input)
      prot_dot_plot_spawn(r6, input$dot_plot_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Dot plot: ERROR')
    },finally={}
    )
  })

#
#   # Download associated tables
#   output$download_pca_scores_table = shiny::downloadHandler(
#     filename = function(){timestamped_name("pca_scores_table.csv")},
#     content = function(file_name){
#       write.csv(r6$tables$pca_scores_table, file_name)
#     }
#   )
#   output$download_pca_loadings_table = shiny::downloadHandler(
#     filename = function(){timestamped_name("pca_loadings_table.csv")},
#     content = function(file_name){
#       write.csv(r6$tables$pca_loadings_table, file_name)
#     }
#   )
#
  # Expanded boxes
  dot_plot_proxy = plotly::plotlyProxy(outputId = "dot_plot_plot",
                                  session = session)

  shiny::observeEvent(input$dot_plot_plotbox,{
    if (input$dot_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = dot_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = dot_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#----------------------------------------------------------- gsea cnet plot ----
prot_cnet_plot_generate = function(r6, colour_list, dimensions_obj, input) {

  print_tm(r6$name, "CNET plot: generating plot.")

  if (input$cnet_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_cnet_plot(showCategory = as.numeric(input$cnet_plot_showcat),
                    enable_physics = input$cnet_plot_enable_physics)
}

prot_cnet_plot_spawn = function(r6, output) {
  print_tm(r6$name, "CNET plot: spawning plot.")

  output$cnet_plot_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$cnetplot,
  )
}

prot_cnet_plot_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "cnet_plot",
                 label = "CNET plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


prot_cnet_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Cnet plot: START.")

  output$cnet_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns("cnet_plot_showcat"),
        label = "Show category",
        value = r6$params$cnet_plot$showCategory
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("cnet_plot_enable_physics"),
        label = 'Enable physics',
        value = r6$params$cnet_plot$enable_physics,
        fill = TRUE,
        status = "primary"
      )
    )
  })
}

prot_cnet_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(c(input$cnet_plot_showcat, input$cnet_plot_enable_physics),{

    # Update parameters
    print_tm(r6$name, "CNET plot: Updating params...")

    r6$param_cnet_plot(showCategory = as.numeric(input$cnet_plot_showcat),
                       enable_physics = input$cnet_plot_enable_physics)

    base::tryCatch({
      prot_cnet_plot_generate(r6, color_palette, dimensions_obj, input)
      prot_cnet_plot_spawn(r6, output)
    },error=function(e){
      print_tm(r6$name, 'CNET plot: ERROR.')
    },finally={}
    )

  })

  #
  #   # Download associated tables
  #   output$download_pca_scores_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_scores_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_scores_table, file_name)
  #     }
  #   )
  #   output$download_pca_loadings_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_loadings_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_loadings_table, file_name)
  #     }
  #   )
  #
  # Expanded boxes

  cnet_plot_proxy = visNetwork::visNetworkProxy(shinyId = "cnet_plot_plot",
                              session = session)

  shiny::observeEvent(input$cnet_plot_plotbox,{
    if (input$cnet_plot_plotbox$maximized) {
      visNetwork::visOptions(graph = cnet_plot_proxy,
                             width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                             height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full)
    } else {
      visNetwork::visOptions(graph = cnet_plot_proxy,
                             width = dimensions_obj$xpx * dimensions_obj$x_plot,
                             height = dimensions_obj$ypx * dimensions_obj$y_plot)
    }
  })

}

#----------------------------------------------------------- gsea Ridge plot ----
prot_ridge_plot_generate = function(r6, colour_list, dimensions_obj, input) {

  if (input$ridge_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  print_tm(r6$name, "Ridge plot: generating plot.")
  r6$plot_ridge_plot(showCategory = as.numeric(input$ridge_plot_showcat),
                     width = width,
                     height = height)
}

prot_ridge_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Ridge plot: spawning plot.")
  output$ridge_plot_plot = plotly::renderPlotly({
    r6$plots$ridgeplot
    plotly::config(r6$plots$ridgeplot, toImageButtonOptions = list(format= format,
                                                                   filename= timestamped_name('ridgeplot'),
                                                                   height= NULL,
                                                                   width= NULL,
                                                                   scale= 1))
  })
}

prot_ridge_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "ridge_plot",
                 label = "Ridge plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


prot_ridge_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Ridge plot: START.")

  output$ridge_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns("ridge_plot_showcat"),
        label = "Show category",
        value = 30
      ),
      shiny::selectInput(
        inputId = ns("ridge_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$ridge_plot$img_format,
        width = "100%"),
    )
  })
}

prot_ridge_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(c(input$ridge_plot_showcat, input$ridge_plot_img_format),{

    # Update parameters
    print_tm(r6$name, "Ridge plot: Updating params...")

    r6$param_ridge_plot(showCategory = as.numeric(input$ridge_plot_showcat),
                        img_format = input$ridge_plot_img_format)

    base::tryCatch({
      prot_ridge_plot_generate(r6, colour_list, dimensions_obj, input)
      prot_ridge_plot_spawn(r6, input$ridge_plot_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Ridge plot: error, missing data.')
    },finally={}
    )

  })

  #
  #   # Download associated tables
  #   output$download_pca_scores_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_scores_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_scores_table, file_name)
  #     }
  #   )
  #   output$download_pca_loadings_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_loadings_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_loadings_table, file_name)
  #     }
  #   )
  #
  # Expanded boxes
  ridge_plot_proxy = plotly::plotlyProxy(outputId = "ridge_plot_plot",
                                  session = session)

  shiny::observeEvent(input$ridge_plot_plotbox,{
    if (input$ridge_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = ridge_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = ridge_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#----------------------------------------------------------- GSEA emap plot ----
prot_emap_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Prot emap plot: generating plot.")

  if (input$prot_emap_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_emap_plot(x = r6$tables$gsea_object,
                    showCategory = r6$params$emap_plot$showCategory,
                    color = r6$params$emap_plot$color,
                    size = r6$params$emap_plot$size,
                    score_threshold = r6$params$emap_plot$score_threshold,
                    similarity_score = r6$params$emap_plot$similarity_score,
                    edge_magnifier = r6$params$emap_plot$edge_magnifier,
                    node_magnifier = r6$params$emap_plot$node_magnifier,
                    enable_physics = r6$params$emap_plot$enable_physics,
                    context = 'gsea')
}
prot_emap_plot_spawn = function(r6, output) {
  print_tm(r6$name, "Prot emap plot: spawning plot.")
  output$prot_emap_plot_plot = visNetwork::renderVisNetwork({
    r6$plots$emap_plot
  })
}


prot_emap_plot_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "prot_emap_plot",
                 label = "eMap plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


prot_emap_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Prot emap plot: START.")
  # Generate UI
  output$prot_emap_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns('prot_emap_plot_showCategory'),
        label = 'Showcategory',
        value = r6$params$emap_plot$showCategory,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("prot_emap_plot_color"),
        label = 'Color',
        choices = c('p.adjust', 'pvalue', 'qvalue', 'Count', 'total_count', 'gene_ratio'),
        selected = r6$params$emap_plot$color,
        width = "100%"),
      shiny::selectInput(
        inputId = ns("prot_emap_plot_size"),
        label = 'Size',
        choices = c('p.adjust', 'pvalue', 'qvalue', 'Count', 'total_count', 'gene_ratio'),
        selected = r6$params$emap_plot$size,
        width = "100%"),
      shiny::sliderInput(
        inputId = ns("prot_emap_plot_score_threshold"),
        label = 'Score threshold',
        min = 0.0,
        max = 1.0,
        value = r6$params$emap_plot$score_threshold,
        step = 0.01,
        width = "100%"
      )
      ,
      shiny::selectInput(
        inputId = ns("prot_emap_plot_similarity_score"),
        label = 'Similarity score',
        choices = c('JC', 'Wang', 'Jiang', 'Rel', 'Lin', 'Resnik'),
        selected = r6$params$emap_plot$similarity_score,
        width = "100%"),
      shiny::textInput(
        inputId = ns('prot_emap_plot_edge_magnifier'),
        label = 'Edge magnifier',
        value = r6$params$emap_plot$edge_magnifier,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('prot_emap_plot_node_magnifier'),
        label = 'Node magnifier',
        value = r6$params$emap_plot$node_magnifier,
        width = '100%'
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("prot_emap_plot_enable_physics"),
        label = 'Enable physics',
        value = r6$params$emap_plot$enable_physics,
        fill = TRUE,
        status = "primary"
      )
    )
  })

}


prot_emap_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$prot_emap_plot_showCategory, input$prot_emap_plot_color, input$prot_emap_plot_size, input$prot_emap_plot_score_threshold, input$prot_emap_plot_similarity_score, input$prot_emap_plot_edge_magnifier, input$prot_emap_plot_node_magnifier, input$prot_emap_plot_enable_physics), {
    print_tm(r6$name, "Prot emap plot: Updating params...")

    r6$param_emap_plot(showCategory = input$prot_emap_plot_showCategory,
                            color = input$prot_emap_plot_color,
                            size = input$prot_emap_plot_size,
                            score_threshold = input$prot_emap_plot_score_threshold,
                            similarity_score = input$prot_emap_plot_similarity_score,
                            edge_magnifier = input$prot_emap_plot_edge_magnifier,
                            node_magnifier = input$prot_emap_plot_node_magnifier,
                            enable_physics = input$prot_emap_plot_enable_physics)

    base::tryCatch({
      prot_emap_plot_generate(r6, color_palette, dimensions_obj, input)
      prot_emap_plot_spawn(r6, output)
    },error=function(e){
      print_tm(r6$name, 'eMap plot: ERROR.')
    },finally={}
    )
  })

  # # Download associated table
  # output$prot_emap_plot_dl_table = shiny::downloadHandler(
  #   filename = function(){timestamped_name("prot_emap_plot_table.csv")},
  #   content = function(file_name){
  #     write.csv(r6$tables$prot_emap_plot, file_name)
  #   }
  # )
  #
  # # Expanded boxes
  # prot_emap_plot_proxy = plotly::plotlyProxy(outputId = "prot_emap_plot_plot",
  #                                            session = session)
  #
  # shiny::observeEvent(input$prot_emap_plot_plotbox,{
  #   if (input$prot_emap_plot_plotbox$maximized) {
  #     plotly::plotlyProxyInvoke(p = prot_emap_plot_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
  #                                    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  #                               ))
  #   } else {
  #     plotly::plotlyProxyInvoke(p = prot_emap_plot_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
  #                                    height = dimensions_obj$ypx * dimensions_obj$y_plot
  #                               ))
  #   }
  # })
}


#------------------------------------------------------------- or dotplot ----
prot_or_dot_plot_generate = function(r6, colour_list, dimensions_obj, input) {

  if (input$or_dot_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  print_tm(r6$name, "Dot plot: generating plot.")
  r6$plot_or_dot_plot(showCategory = as.numeric(input$or_dot_plot_showcat),
                   width = width,
                   height = height)
}

prot_or_dot_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Dot plot: spawning plot.")

  output$or_dot_plot_plot = plotly::renderPlotly({
    r6$plots$or_dotplot
    plotly::config(r6$plots$or_dotplot, toImageButtonOptions = list(format= format,
                                                                 filename= timestamped_name('dotplot'),
                                                                 height= NULL,
                                                                 width= NULL,
                                                                 scale= 1))
  })
}

prot_or_dot_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "or_dot_plot",
                 label = "Dot plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


prot_or_dot_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Dot plot: START.")

  output$or_dot_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns("or_dot_plot_showcat"),
        label = "Show category",
        value = r6$params$or_dot_plot$showCategory
      ),
      shiny::selectInput(
        inputId = ns("or_dot_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$or_dot_plot$img_format,
        width = "100%"),
    )
  })
}

prot_or_dot_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(c(input$or_dot_plot_showcat, input$or_dot_plot_img_format),{

    # Update parameters
    print_tm(r6$name, "Dot plot: Updating params...")

    r6$param_or_dot_plot(showCategory = as.numeric(input$or_dot_plot_showcat),
                         img_format = input$or_dot_plot_img_format)

    # Produce the plot
    base::tryCatch({
      prot_or_dot_plot_generate(r6, color_palette, dimensions_obj, input)
      prot_or_dot_plot_spawn(r6, input$dot_plot_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Dot plot: ERROR')
    },finally={}
    )
  })

  #
  #   # Download associated tables
  #   output$download_pca_scores_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_scores_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_scores_table, file_name)
  #     }
  #   )
  #   output$download_pca_loadings_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_loadings_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_loadings_table, file_name)
  #     }
  #   )
  #
  # Expanded boxes
  or_dot_plot_proxy = plotly::plotlyProxy(outputId = "or_dot_plot_plot",
                                       session = session)

  shiny::observeEvent(input$or_dot_plot_plotbox,{
    if (input$or_dot_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = or_dot_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = or_dot_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#-------------------------------------------------------------- or bar plot ----
prot_or_bar_plot_generate = function(r6, colour_list, dimensions_obj, input) {

  if (input$or_bar_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  print_tm(r6$name, "Bar plot: generating plot.")
  r6$plot_or_bar_plot(x = input$or_bar_plot_x,
                      color = input$or_bar_plot_color,
                      showCategory = as.numeric(input$or_bar_plot_showcat),
                      width = width,
                      height = height)
}

prot_or_bar_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Bar plot: spawning plot.")

  output$or_bar_plot_plot = plotly::renderPlotly({
    r6$plots$or_barplot
    plotly::config(r6$plots$or_barplot, toImageButtonOptions = list(format= format,
                                                                    filename= timestamped_name('barplot'),
                                                                    height= NULL,
                                                                    width= NULL,
                                                                    scale= 1))
  })
}

prot_or_bar_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "or_bar_plot",
                 label = "Bar plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


prot_or_bar_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Bar plot: START.")

  output$or_bar_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns("or_bar_plot_showcat"),
        label = "Show category",
        value = r6$params$or_bar_plot$showCategory
      ),
      shiny::selectInput(
        inputId = ns('or_bar_plot_x'),
        label = "X-axis",
        choices = c('Count'),
        selected = r6$params$or_bar_plot$x,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('or_bar_plot_color'),
        label = "Color",
        choices = c('p.adjust'),
        selected = r6$params$or_bar_plot$color,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("or_bar_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$or_bar_plot$img_format,
        width = "100%"),
    )
  })
}

prot_or_bar_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(c(input$or_bar_plot_x, input$or_bar_plot_color, input$or_bar_plot_showcat, input$or_bar_plot_img_format),{

    # Update parameters
    print_tm(r6$name, "Bar plot: Updating params...")

    r6$param_or_bar_plot(x = input$or_bar_plot_x,
                         color = input$or_bar_plot_color,
                         showCategory = input$or_bar_plot_showcat,
                         img_format = input$or_bar_plot_img_format)

    # Produce the plot
    base::tryCatch({
      prot_or_bar_plot_generate(r6, color_palette, dimensions_obj, input)
      prot_or_bar_plot_spawn(r6, input$dot_plot_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Bar plot: ERROR')
    },finally={}
    )
  })

  #
  #   # Download associated tables
  #   output$download_pca_scores_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_scores_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_scores_table, file_name)
  #     }
  #   )
  #   output$download_pca_loadings_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_loadings_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_loadings_table, file_name)
  #     }
  #   )
  #
  # Expanded boxes
  or_bar_plot_proxy = plotly::plotlyProxy(outputId = "or_bar_plot_plot",
                                          session = session)

  shiny::observeEvent(input$or_bar_plot_plotbox,{
    if (input$or_bar_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = or_bar_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = or_bar_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#----------------------------------------------------------- or cnet plot ----
prot_or_cnet_plot_generate = function(r6, colour_list, dimensions_obj, input) {

  print_tm(r6$name, "CNET plot: generating plot.")

  if (input$or_cnet_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_cnet_plot(x = r6$tables$go_enrich,
                    showCategory = as.numeric(input$or_cnet_plot_showcat),
                    enable_physics = input$or_cnet_plot_enable_physics,
                    context = "or")
}

prot_or_cnet_plot_spawn = function(r6, output) {
  print_tm(r6$name, "CNET plot: spawning plot.")

  output$or_cnet_plot_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$or_cnetplot,
  )
}

prot_or_cnet_plot_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "or_cnet_plot",
                 label = "CNET plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


prot_or_cnet_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Cnet plot: START.")

  output$or_cnet_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns("or_cnet_plot_showcat"),
        label = "Show category",
        value = r6$params$or_cnet_plot$showCategory
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("or_cnet_plot_enable_physics"),
        label = 'Enable physics',
        value = r6$params$or_cnet_plot$enable_physics,
        fill = TRUE,
        status = "primary"
      )
    )
  })
}

prot_or_cnet_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(c(input$or_cnet_plot_showcat, input$or_cnet_plot_enable_physics),{

    # Update parameters
    print_tm(r6$name, "CNET plot: Updating params...")

    r6$param_or_cnet_plot(showCategory = as.numeric(input$cnet_plot_showcat),
                          enable_physics = input$or_cnet_plot_enable_physics)

    base::tryCatch({
      prot_or_cnet_plot_generate(r6, color_palette, dimensions_obj, input)
      prot_or_cnet_plot_spawn(r6, output)
    },error=function(e){
      print_tm(r6$name, 'CNET plot: ERROR.')
    },finally={}
    )

  })

  #
  #   # Download associated tables
  #   output$download_pca_scores_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_scores_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_scores_table, file_name)
  #     }
  #   )
  #   output$download_pca_loadings_table = shiny::downloadHandler(
  #     filename = function(){timestamped_name("pca_loadings_table.csv")},
  #     content = function(file_name){
  #       write.csv(r6$tables$pca_loadings_table, file_name)
  #     }
  #   )
  #
  # Expanded boxes

  or_cnet_plot_proxy = visNetwork::visNetworkProxy(shinyId = "or_cnet_plot_plot",
                                                session = session)

  shiny::observeEvent(input$or_cnet_plot_plotbox,{
    if (input$or_cnet_plot_plotbox$maximized) {
      visNetwork::visOptions(graph = or_cnet_plot_proxy,
                             width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                             height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full)
    } else {
      visNetwork::visOptions(graph = or_cnet_plot_proxy,
                             width = dimensions_obj$xpx * dimensions_obj$x_plot,
                             height = dimensions_obj$ypx * dimensions_obj$y_plot)
    }
  })

}




#------------------------------------------------------------ ORA emap plot ----
prot_or_emap_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "OR emap plot: generating plot.")

  if (input$prot_or_emap_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_emap_plot(x = r6$tables$go_enrich,
                    showCategory = r6$params$or_emap_plot$showCategory,
                    color = r6$params$or_emap_plot$color,
                    size = r6$params$or_emap_plot$size,
                    score_threshold = r6$params$or_emap_plot$score_threshold,
                    similarity_score = r6$params$or_emap_plot$similarity_score,
                    edge_magnifier = r6$params$or_emap_plot$edge_magnifier,
                    node_magnifier = r6$params$or_emap_plot$node_magnifier,
                    enable_physics = r6$params$or_emap_plot$enable_physics,
                    context = 'or')
}
prot_or_emap_plot_spawn = function(r6, output) {
  print_tm(r6$name, "Prot emap plot: spawning plot.")
  output$prot_or_emap_plot_plot = visNetwork::renderVisNetwork({
    r6$plots$or_emap_plot
  })
}


prot_or_emap_plot_ui = function(dimensions_obj, session) {

  get_visnet_box(id = "prot_or_emap_plot",
                 label = "eMap plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


prot_or_emap_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Prot emap plot: START.")
  # Generate UI
  output$prot_or_emap_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns('prot_or_emap_plot_showCategory'),
        label = 'Showcategory',
        value = r6$params$or_emap_plot$showCategory,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("prot_or_emap_plot_color"),
        label = 'Color',
        choices = c('p.adjust', 'pvalue', 'qvalue', 'Count', 'total_count', 'gene_ratio'),
        selected = r6$params$or_emap_plot$color,
        width = "100%"),
      shiny::selectInput(
        inputId = ns("prot_or_emap_plot_size"),
        label = 'Size',
        choices = c('p.adjust', 'pvalue', 'qvalue', 'Count', 'total_count', 'gene_ratio'),
        selected = r6$params$or_emap_plot$size,
        width = "100%"),
      shiny::sliderInput(
        inputId = ns("prot_or_emap_plot_score_threshold"),
        label = 'Score threshold',
        min = 0.0,
        max = 1.0,
        value = r6$params$or_emap_plot$score_threshold,
        step = 0.01,
        width = "100%"
      ),
      shiny::selectInput(
        inputId = ns("prot_or_emap_plot_similarity_score"),
        label = 'Similarity score',
        choices = c('JC', 'Wang', 'Jiang', 'Rel', 'Lin', 'Resnik'),
        selected = r6$params$or_emap_plot$similarity_score,
        width = "100%"),
      shiny::textInput(
        inputId = ns('prot_or_emap_plot_edge_magnifier'),
        label = 'Edge magnifier',
        value = r6$params$or_emap_plot$edge_magnifier,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('prot_or_emap_plot_node_magnifier'),
        label = 'Node magnifier',
        value = r6$params$or_emap_plot$node_magnifier,
        width = '100%'
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("prot_or_emap_plot_enable_physics"),
        label = 'Enable physics',
        value = r6$params$or_emap_plot$enable_physics,
        fill = TRUE,
        status = "primary"
      )
    )
  })

}


prot_or_emap_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$prot_or_emap_plot_showCategory, input$prot_or_emap_plot_color, input$prot_or_emap_plot_size, input$prot_or_emap_plot_score_threshold, input$prot_or_emap_plot_similarity_score, input$prot_or_emap_plot_edge_magnifier, input$prot_or_emap_plot_node_magnifier, input$prot_or_emap_plot_enable_physics), {
    print_tm(r6$name, "Prot emap plot: Updating params...")

    r6$param_or_emap_plot(showCategory = input$prot_or_emap_plot_showCategory,
                       color = input$prot_or_emap_plot_color,
                       size = input$prot_or_emap_plot_size,
                       score_threshold = input$prot_or_emap_plot_score_threshold,
                       similarity_score = input$prot_or_emap_plot_similarity_score,
                       edge_magnifier = input$prot_or_emap_plot_edge_magnifier,
                       node_magnifier = input$prot_or_emap_plot_node_magnifier,
                       enable_physics = input$prot_or_emap_plot_enable_physics)

    prot_or_emap_plot_generate(r6, color_palette, dimensions_obj, input)
    prot_or_emap_plot_spawn(r6, output)

    # base::tryCatch({
    #   prot_emap_plot_generate(r6, color_palette, dimensions_obj, input)
    #   prot_emap_plot_spawn(r6, output)
    # },error=function(e){
    #   print_tm(r6$name, 'eMap plot: ERROR.')
    # },finally={}
    # )
  })

  # # Download associated table
  # output$prot_emap_plot_dl_table = shiny::downloadHandler(
  #   filename = function(){timestamped_name("prot_emap_plot_table.csv")},
  #   content = function(file_name){
  #     write.csv(r6$tables$prot_emap_plot, file_name)
  #   }
  # )
  #
  # # Expanded boxes
  # prot_emap_plot_proxy = plotly::plotlyProxy(outputId = "prot_emap_plot_plot",
  #                                            session = session)
  #
  # shiny::observeEvent(input$prot_emap_plot_plotbox,{
  #   if (input$prot_emap_plot_plotbox$maximized) {
  #     plotly::plotlyProxyInvoke(p = prot_emap_plot_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
  #                                    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  #                               ))
  #   } else {
  #     plotly::plotlyProxyInvoke(p = prot_emap_plot_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
  #                                    height = dimensions_obj$ypx * dimensions_obj$y_plot
  #                               ))
  #   }
  # })
}





