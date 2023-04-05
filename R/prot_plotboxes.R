#----------------------------------------------------------- Table switches ----
table_switch = function(selection, r6){
  switch(EXPR = selection,
         "Filtered data table" = r6$tables$data_filtered,
         "Class normalised data table" = r6$tables$data_class_norm,
         "Total normalised data table" = r6$tables$data_total_norm,
         "Raw class table" = r6$tables$data_class_table_raw,
         "Total normalised class table" = r6$tables$data_class_table_total_norm,
         "Z-scored data table" = r6$tables$data_z_scored,
         "Z-scored class normalised data table" = r6$tables$data_class_norm_z_scored,
         "Z-scored total normalised data table" = r6$tables$data_total_norm_z_scored,
         "Z-scored total normalised class table" = r6$tables$data_class_table_z_scored,
         "Raw feature table" = r6$tables$feat_raw,
         "Filtered feature table" = r6$tables$feat_filtered
  )
}

#-------------------------------------------------- P-val adjustment switch ----


adjustment_switch = function(selection){
  switch(EXPR = selection,
         "None" = "minus_log10_p_value",
         "Benjamini-Hochberg" = "minus_log10_p_value_bh_adj"
  )
}

adjustment_title_switch = function(selection) {
  switch(EXPR = selection,
         "minus_log10_p_value" = "-Log10(p-value)",
         "minus_log10_p_value_bh_adj" = "-Log10(BH(p-value))"
  )
}

#----------------------------------------------- Plotting function controls ----

prot_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_pca" = prot_pca_ui,
                                          "select_heatmap" = prot_heatmap_ui,
                                          "select_volcano_plot" = prot_volcano_plot_ui,
                                          "select_class_distribution" = prot_class_distribution_ui
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
                                                  "select_volcano_plot" = prot_volcano_plot_server,
                                                  "select_class_distribution" = prot_class_distribution_server
                                                  )
                         )
  }
  return(server_functions)
}



#------------------------------------------------------- Class distribution ----

prot_class_distribution_generate = function(r6, colour_list, dimensions_obj, input, plot_name) {
  print_time(paste0(plot_name, ": generating plot."))
  
  if (input$class_distribution_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  
  r6$plot_class_distribution(table = table_switch(input$class_distribution_dataset, r6),
                             col_group = input$class_distribution_metacol,
                             colour_list = colour_list,
                             width = width,
                             height = height)
}


prot_class_distribution_spawn = function(r6, output, plot_name) {
  print_time(paste0(plot_name, ": spawning plot."))
  output$class_distribution_plot = plotly::renderPlotly(
    r6$plots$class_distribution
  )
}


prot_class_distribution_ui = function(dimensions_obj, session) {
  
  get_plotly_box(id = "class_distribution",
                 label = "Class distribution",
                 dimensions_obj = dimensions_obj,
                 session = session)
  
}


prot_class_distribution_server = function(r6, output, session) {
  
  ns = session$ns
  
  print_time("Class distribution : START.")
  # Generate UI
  output$class_distribution_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("class_distribution_dataset"),
        label = "Select table",
        choices = c("Raw class table", "Total normalised class table"),
        selected = r6$params$class_distribution$dataset
      ),
      shiny::selectInput(
        inputId = ns("class_distribution_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$params$class_distribution$group_col
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::downloadButton(
        outputId = ns("download_class_distribution_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

prot_class_distribution_events = function(r6, dimensions_obj, colour_list, input, output, session) {
  
  # Generate the plot
  shiny::observeEvent(c(input$class_distribution_dataset, input$class_distribution_metacol), {
    print_time("Class distribution: Updating params...")
    r6$params$class_distribution$dataset = input$class_distribution_dataset
    r6$set_params_class_distribution(val = input$class_distribution_metacol)
    prot_class_distribution_generate(r6, colour_list, dimensions_obj, input, "Class distribution")
    prot_class_distribution_spawn(r6, output, "Class distribution")
  })
  
  # Download associated table
  output$download_class_distribution_table = shiny::downloadHandler(
    filename = function(){timestamped_name("class_distribution_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$class_distribution_table, file_name)
    }
  )
  
  # Expanded boxes
  class_distribution_proxy = plotly::plotlyProxy(outputId = "class_distribution_plot",
                                                 session = session)
  
  shiny::observeEvent(input$class_distribution_plotbox,{
    if (input$class_distribution_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = class_distribution_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = class_distribution_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
  
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


prot_volcano_plot_spawn = function(r6, output) {
  print_time("Volcano plot: spawning plot.")
  output$volcano_plot_plot = plotly::renderPlotly(
    r6$plots$volcano_plot
  )
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
      shiny::downloadButton(
        outputId = ns("download_volcano_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

prot_volcano_plot_events = function(r6, dimensions_obj, colour_list, input, output, session) {
  
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
  
  shiny::observeEvent(c(shiny::req(length(input$volcano_plot_metagroup) == 2), input$volcano_plot_tables, input$volcano_plot_function, input$volcano_plot_adjustment, input$volcano_plot_test), {
    print_time("Volcano plot: Updating params...")
    
    r6$params$volcano_plot$data_table = input$volcano_plot_tables
    r6$params$volcano_plot$group_column = input$volcano_plot_metacol
    r6$params$volcano_plot$groups = input$volcano_plot_metagroup
    r6$params$volcano_plot$selected_function = input$volcano_plot_function
    r6$params$volcano_plot$adjustment = input$volcano_plot_adjustment
    r6$params$volcano_plot$selected_test = input$volcano_plot_test
    
    prot_volcano_plot_generate(r6, colour_list, dimensions_obj, input)
    prot_volcano_plot_spawn(r6, output)
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
  
  r6$plot_heatmap(data_table = table_switch(input$heatmap_dataset, r6),
                  percentile = input$heatmap_percentile,
                  width = dimensions_obj$xpx * dimensions_obj$x_plot,
                  height = dimensions_obj$ypx * dimensions_obj$y_plot)
}

prot_heatmap_spawn = function(r6, output) {
  print_time("Heatmap: spawning plot.")
  output$heatmap_plot = plotly::renderPlotly(
    r6$plots$heatmap
  )
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
        choices = c("Z-scored total normalised data table"),
        selected = r6$params$heatmap$dataset
      ),
      shiny::sliderInput(inputId = ns("heatmap_percentile"),
                         label = "Percentile",
                         min = 90,
                         max = 100,
                         value = r6$params$heatmap$percentile,
                         step = 1
      ),
      shiny::actionButton(
        inputId = ns("heatmap_run"),
        label = "Generate heatmap"
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::downloadButton(
        outputId = ns("download_heatmap_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

prot_heatmap_events = function(r6, dimensions_obj, colour_list, input, output, session) {
  
  shiny::observeEvent(input$heatmap_run,{
    print_time("Heatmap: Updating params...")
    r6$params$heatmap$dataset = input$heatmap_dataset
    r6$params$heatmap$percentile = input$heatmap_percentile
    prot_heatmap_generate(r6, colour_list, dimensions_obj, input)
    prot_heatmap_spawn(r6, output)
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



#-------------------------------------------------------- Default plotboxes ----

get_plotly_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    plotly::plotlyOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
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
  
  r6$plot_pca(data_table = table_switch(input$pca_dataset, r6),
              col_group = input$pca_metacol,
              width = width,
              height = height,
              colour_list = colour_list)
}

prot_pca_spawn = function(r6, output) {
  print_time("PCA: spawning plot.")
  output$pca_plot = plotly::renderPlotly(
    r6$plots$pca_plot
  )
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
        choices = c("Z-scored total normalised data table"),
        selected = r6$params$pca$dataset
      ),
      shiny::selectInput(
        inputId = ns("pca_metacol"),
        label = "Select metadata column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$params$pca$group_column
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
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

prot_pca_events = function(r6, dimensions_obj, colour_list, input, output, session) {

  shiny::observeEvent(c(input$pca_dataset, input$pca_metacol),{
    print_time("PCA: Updating params...")
    r6$params$pca$dataset = input$pca_dataset
    r6$params$pca$group_column = input$pca_metacol
    prot_pca_generate(r6, colour_list, dimensions_obj, input)
    prot_pca_spawn(r6, output)
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




