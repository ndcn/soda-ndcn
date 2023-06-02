


#----------------------------------------------- Plotting function controls ----

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

#------------------------------------------------------------- Volcano plot ----

prot_volcano_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_time("Volcano plot: generating plot.")
  
  if (input$volcano_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  
  r6$get_volcano_table(data_table = table_switch(selection = input$volcano_plot_tables, r6 = r6),
                       col_group = input$volcano_plot_metacol,
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
  print_time("Volcano plot: spawning plot.")
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
  print_time("Volcano plot: START.")
  
  # Set UI
  output$volcano_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("volcano_plot_tables"),
        label = "Select data table",
        choices = c("Filtered data table", "Total normalised data table"),
        selected = r6$params$volcano_plot$data_table
      ),
      shiny::selectInput(
        inputId = ns("volcano_plot_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$params$volcano_plot$group_column
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_metagroup"),
        label = "Select two groups to compare",
        choices = unique(r6$tables$meta_filtered[,r6$params$volcano_plot$group_column]),
        selected = r6$params$volcano_plot$groups,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_function"),
        label = "Select function",
        choices = c("median", "mean"),
        selected = r6$params$volcano_plot$selected_function,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_test"),
        label = "Select test",
        choices = c("Wilcoxon", "T-test"),
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
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("volcano_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$volcano_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_volcano_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

prot_volcano_plot_events = function(r6, dimensions_obj, r6_settings, input, output, session) {
  
  # auto-update selected groups
  shiny::observeEvent(input$volcano_plot_metacol,{
    r6$params$volcano_plot$groups = unique(r6$tables$meta_filtered[,input$volcano_plot_metacol])[c(1,2)]
    shiny::updateSelectizeInput(
      inputId = "volcano_plot_metagroup",
      session = session,
      choices = unique(r6$tables$meta_filtered[,input$volcano_plot_metacol]),
      selected = r6$params$volcano_plot$groups
    )
  })
  
  shiny::observeEvent(c(shiny::req(length(input$volcano_plot_metagroup) == 2), input$volcano_plot_tables, input$volcano_plot_function, input$volcano_plot_adjustment, input$volcano_plot_test, input$volcano_plot_img_format), {
    print_time("Volcano plot: Updating params...")
    
    r6$params$volcano_plot$data_table = input$volcano_plot_tables
    r6$params$volcano_plot$group_column = input$volcano_plot_metacol
    r6$params$volcano_plot$groups = input$volcano_plot_metagroup
    r6$params$volcano_plot$selected_function = input$volcano_plot_function
    r6$params$volcano_plot$adjustment = input$volcano_plot_adjustment
    r6$params$volcano_plot$selected_test = input$volcano_plot_test
    r6$params$volcano_plot$img_format = input$volcano_plot_img_format
    
    base::tryCatch({
      prot_volcano_plot_generate(r6, r6_settings$color_settings$color_palette, dimensions_obj, input)
      prot_volcano_plot_spawn(r6, format = input$volcano_plot_img_format, output)
    },error=function(e){
      print_time('Volcano plot: error, missing data.')
    },finally={}
    )
  })
  
  # Export volcano table
  output$download_volcano_table = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_table, file_name)
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
  print_time("Heatmap: generating plot.")
  if (input$heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  
  if (input$heatmap_apply_da) {
    data_table = table_switch(input$heatmap_dataset, r6)
    data_table = apply_discriminant_analysis(data_table = data_table,
                                             group_list = r6$tables$meta_filtered[,input$heatmap_group_col_da],
                                             nlambda = 100,
                                             alpha = input$heatmap_alpha_da)
    
    meta_table_features = r6$tables$feat_filtered[colnames(data_table), ]
  } else {
    data_table = table_switch(input$heatmap_dataset, r6)
    meta_table_features = r6$tables$feat_filtered
  }
  
  if ("cluster_rows" %in% input$heatmap_clustering){
    cluster_rows = TRUE
  } else {
    cluster_rows = FALSE
  }
  
  if ("cluster_columns" %in% input$heatmap_clustering){
    cluster_cols = TRUE
  } else {
    cluster_cols = FALSE
  }
  
  r6$plot_heatmap(data_table = data_table,
                  meta_table = r6$tables$meta_filtered,
                  percentile = input$heatmap_percentile,
                  cluster_rows = cluster_rows,
                  cluster_cols = cluster_cols,
                  row_annotations = input$heatmap_map_rows,
                  width = dimensions_obj$xpx * dimensions_obj$x_plot,
                  height = dimensions_obj$ypx * dimensions_obj$y_plot)
}

prot_heatmap_spawn = function(r6, format, output) {
  print_time("Heatmap: spawning plot.")
  output$heatmap_plot = plotly::renderPlotly({
    r6$plots$heatmap
    plotly::config(r6$plots$heatmap, toImageButtonOptions = list(format= format,
                                                                      filename= timestamped_name('heatmap_plot'),
                                                                      height= NULL,
                                                                      width= NULL,
                                                                      scale= 1))
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
  print_time("Heatmap: START.")
  
  output$heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("heatmap_dataset"),
        label = "Select dataset",
        choices = c("Z-scored data table", "Z-scored total normalised data table"),
        selected = r6$params$heatmap$dataset
      ),
      shiny::checkboxGroupInput(
        label = "Clustering",
        inputId = ns("heatmap_clustering"),
        choices = c("Cluster samples" = "cluster_rows",
                    "Cluster features" = "cluster_columns"),
        selected = r6$params$heatmap$clustering
      ),
      shiny::selectizeInput(
        inputId = ns("heatmap_map_rows"),
        label = "Map sample data",
        multiple = TRUE,
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$params$heatmap$map_sample_data
      ),
      shiny::sliderInput(inputId = ns("heatmap_percentile"),
                         label = "Percentile",
                         min = 90,
                         max = 100,
                         value = r6$params$heatmap$percentile,
                         step = 1
      ),
      
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::h4("Discriminant analysis"),
      
      shinyWidgets::switchInput(inputId = ns("heatmap_apply_da"),
                                label = "Apply discriminant analysis",
                                value = r6$params$heatmap$apply_da,
                                width = "100%",
                                disabled = TRUE),
      
      shiny::selectizeInput(inputId = ns("heatmap_group_col_da"),
                            label = "Group column",
                            choices = colnames(r6$tables$meta_filtered),
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

prot_heatmap_events = function(r6, dimensions_obj, r6_settings, input, output, session) {
  
  shiny::observeEvent(input$heatmap_run,{
    shinyjs::disable("heatmap_run")
    print_time("Heatmap: Updating params...")
    r6$params$heatmap$dataset = input$heatmap_dataset
    r6$params$heatmap$clustering = input$heatmap_clustering
    r6$params$heatmap$map_sample_data = input$heatmap_map_rows
    r6$params$heatmap$percentile = input$heatmap_percentile
    r6$params$heatmap$apply_da = input$heatmap_apply_da
    r6$params$heatmap$group_column_da = input$heatmap_group_col_da
    r6$params$heatmap$alpha_da = input$heatmap_alpha_da
    r6$params$heatmap$img_format = input$heatmap_img_format
    
    base::tryCatch({
      prot_heatmap_generate(r6, r6_settings$color_settings$color_palette, dimensions_obj, input)
      prot_heatmap_spawn(r6, input$heatmap_img_format, output)
    },error=function(e){
      print_time('Heatmap: error, missing data.')
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
  print_time("PCA: generating plot.")
  
  if (input$pca_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  
  if (input$pca_apply_da) {
    data_table = table_switch(input$pca_dataset, r6)
    data_table = apply_discriminant_analysis(data_table = data_table,
                                                group_list = r6$tables$meta_filtered[,input$pca_metacol],
                                                nlambda = 100,
                                                alpha = input$pca_alpha_da)
    
  } else {
    data_table = table_switch(input$pca_dataset, r6)
  }
  r6$plot_pca(data_table = data_table,
              col_group = input$pca_metacol,
              width = width,
              height = height,
              colour_list = colour_list)
}

prot_pca_spawn = function(r6, format, output) {
  print_time("PCA: spawning plot.")
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
  print_time("PCA: START.")

  output$pca_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("pca_dataset"),
        label = "Select dataset",
        choices = c("Z-scored data table", "Z-scored total normalised data table"),
        selected = r6$params$pca$dataset
      ),
      shiny::selectInput(
        inputId = ns("pca_metacol"),
        label = "Select metadata column",
        choices = colnames(r6$tables$meta_filtered),
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

prot_pca_events = function(r6, dimensions_obj, r6_settings, input, output, session) {

  shiny::observeEvent(c(input$pca_dataset, input$pca_metacol, input$pca_apply_da, input$pca_alpha_da, input$pca_img_format),{
    print_time("PCA: Updating params...")
    r6$params$pca$dataset = input$pca_dataset
    r6$params$pca$group_column = input$pca_metacol
    r6$params$pca$apply_da = input$pca_apply_da
    r6$params$pca$alpha_da = input$pca_alpha_da
    r6$params$pca$img_format = input$pca_img_format
    
    base::tryCatch({
      prot_pca_generate(r6, r6_settings$color_settings$color_palette, dimensions_obj, input)
      prot_pca_spawn(r6, input$pca_img_format, output)
    },error=function(e){
      print_time('PCA: error, missing data.')
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
  
  print_time("Dot plot: generating plot.")
  r6$plot_dot_plot(showCategory = as.numeric(input$dot_plot_showcat),
                   mode = input$dot_plot_mode,
                   width = width,
                   height = height)
}

prot_dot_plot_spawn = function(r6, format, output) {
  print_time("Dot plot: spawning plot.")
  
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
 
  print_time("Dot plot: START.")
  
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

prot_dot_plot_events = function(r6, dimensions_obj, r6_settings, input, output, session) {

  shiny::observeEvent(c(input$dot_plot_showcat, input$dot_plot_mode, input$dot_plot_img_format),{
    
    # Update parameters
    print_time("Dot plot: Updating params...")
    r6$params$dot_plot$showCategory = as.numeric(input$dot_plot_showcat)
    r6$params$dot_plot$mode = input$dot_plot_mode
    r6$params$dot_plot$img_format = input$dot_plot_img_format
    
    # Produce the plot
    base::tryCatch({
      prot_dot_plot_generate(r6, colour_list, dimensions_obj, input)
      prot_dot_plot_spawn(r6, input$dot_plot_img_format, output)
    },error=function(e){
      print_time('Dot plot: error, missing data.')
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
  
  print_time("CNET plot: generating plot.")
  
  if (input$cnet_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  
  r6$plot_cnet_plot(showCategory = as.numeric(input$cnet_plot_showcat))
}

prot_cnet_plot_spawn = function(r6, output) {
  print_time("CNET plot: spawning plot.")
  
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
  
  print_time("Cnet plot: START.")
  
  output$cnet_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns("cnet_plot_showcat"),
        label = "Show category",
        value = r6$params$cnet_plot$showCategory
      )
    )
  })
}

prot_cnet_plot_events = function(r6, dimensions_obj, r6_settings, input, output, session) {
  
  shiny::observeEvent(input$cnet_plot_showcat,{
    
    # Update parameters
    print_time("CNET plot: Updating params...")
    r6$params$cnet_plot$showCategory = as.numeric(input$cnet_plot_showcat)
    
    base::tryCatch({
      prot_cnet_plot_generate(r6, colour_list, dimensions_obj, input)
      prot_cnet_plot_spawn(r6, output)
    },error=function(e){
      print_time('CNET plot: error, missing data.')
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
  
  print_time("Ridge plot: generating plot.")
  r6$plot_ridge_plot(showCategory = as.numeric(input$ridge_plot_showcat),
                     width = width,
                     height = height)
}

prot_ridge_plot_spawn = function(r6, format, output) {
  print_time("Ridge plot: spawning plot.")
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
  
  print_time("Ridge plot: START.")
  
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

prot_ridge_plot_events = function(r6, dimensions_obj, r6_settings, input, output, session) {
  
  shiny::observeEvent(c(input$ridge_plot_showcat, input$ridge_plot_img_format),{
    
    # Update parameters
    print_time("Ridge plot: Updating params...")
    r6$params$ridge_plot$showCategory = as.numeric(input$ridge_plot_showcat)
    r6$params$ridge_plot$img_format = input$ridge_plot_img_format
    
    base::tryCatch({
      prot_ridge_plot_generate(r6, colour_list, dimensions_obj, input)
      prot_ridge_plot_spawn(r6, input$ridge_plot_img_format, output)
    },error=function(e){
      print_time('Ridge plot: error, missing data.')
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

#----------------------------------------------------------- gsea eMap plot ----


prot_emap_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_time("eMap plot: generating plot.")
  r6$plot_emap_plot(showCategory = as.numeric(input$emap_plot_showcat))
}

prot_emap_plot_spawn = function(r6, output) {
  print_time("eMap plot: spawning plot.")
  
  output$emap_plot_plot = visNetwork::renderVisNetwork(
    expr = r6$plots$emapplot,
  )
}

prot_emap_plot_ui = function(dimensions_obj, session) {
  
  get_visnet_box(id = "emap_plot",
                 label = "eMap plot",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


prot_emap_plot_server = function(r6, output, session) {
  
  ns = session$ns
  
  print_time("eMap plot: START.")
  
  output$emap_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns("emap_plot_showcat"),
        label = "Show category",
        value = r6$params$emap_plot$showCategory
      )
    )
  })
}

prot_emap_plot_events = function(r6, dimensions_obj, r6_settings, input, output, session) {
  
  shiny::observeEvent(input$emap_plot_showcat,{
    
    # Update parameters
    print_time("eMap plot: Updating params...")
    r6$params$emap_plot$showCategory = as.numeric(input$emap_plot_showcat)
    
    base::tryCatch({
      prot_emap_plot_generate(r6, colour_list, dimensions_obj, input)
      prot_emap_plot_spawn(r6, output)
    },error=function(e){
      print_time('eMap plot: error, missing data.')
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
  
  emap_plot_proxy = visNetwork::visNetworkProxy(shinyId = "emap_plot_plot",
                                                session = session)
  
  shiny::observeEvent(input$emap_plot_plotbox,{
    if (input$emap_plot_plotbox$maximized) {
      print("max")
      visNetwork::visOptions(graph = emap_plot_proxy,
                             width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                             height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full)
    } else {
      print("min")
      visNetwork::visOptions(graph = emap_plot_proxy,
                             width = dimensions_obj$xpx * dimensions_obj$x_plot,
                             height = dimensions_obj$ypx * dimensions_obj$y_plot)
    }
  })
  
}

