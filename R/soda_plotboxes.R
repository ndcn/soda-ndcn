#----------------------------------------------------------- Table switches ----
table_switch = function(selection, r6){
  switch(EXPR = selection,
         "Filtered data table" = r6$tables$data_filtered,
         "Class normalised data table" = r6$tables$data_class_norm,
         "Total normalised data table" = r6$tables$data_total_norm
  )
}

z_score_table_switch = function(selection, r6){
  switch(EXPR = selection,
         "Filtered data table" = r6$tables$data_z_scored,
         "Class normalised data table" = r6$tables$data_class_norm_z_scored,
         "Total normalised data table" = r6$tables$data_total_norm_z_scored
  )
}

#----------------------------------------------- Plotting function controls ----

plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_class_distribution" = class_distribution_ui,
                                          "select_class_comparison" = class_comparison_ui,
                                          "select_volcano_plot" = volcano_plot_ui,
                                          "select_heatmap" = heatmap_ui,
                                          "select_pca" = pca_ui,
                                          "select_double_bond_plot" = double_bonds_ui
    )
    )
  }
  return(ui_functions)
}

plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_class_distribution" = class_distribution_server,
                                                  "select_class_comparison" = class_comparison_server,
                                                  "select_volcano_plot" = volcano_plot_server,
                                                  "select_heatmap" = heatmap_server,
                                                  "select_pca" = pca_server,
                                                  "select_double_bond_plot" = double_bonds_server
                                                  )
                         )
  }
  return(server_functions)
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

#------------------------------------------------------- Class distribution ----

class_distribution_generate = function(r6, colour_list, dimensions_obj, input, plot_name) {
  print_time(paste0(plot_name, ": generating plot."))

  if (input$class_distribution_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_class_distribution(col_group = input$class_distribution_metacol,
                             colour_list = colour_list,
                             width = width,
                             height = height)
}


class_distribution_spawn = function(r6, output, plot_name) {
  print_time(paste0(plot_name, ": spawning plot."))
  output$class_distribution_plot = plotly::renderPlotly(
    r6$plots$class_distribution
  )
}


class_distribution_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "class_distribution",
                 label = "Class distribution",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


class_distribution_server = function(r6, output, session) {

  ns = session$ns
  
  print_time("Class distribution : START.")
  # Generate UI
  output$class_distribution_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("class_distribution_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$params$class_distribution$group_col()
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
  
class_distribution_events = function(r6, dimensions_obj, colour_list, input, output, session) {
  
  # Generate the plot
  shiny::observeEvent(input$class_distribution_metacol, {
    print_time(paste0("Class distribution: Updating params to ", input$class_distribution_metacol))
    r6$set_params_class_distribution(val = input$class_distribution_metacol)
    class_distribution_generate(r6, colour_list, dimensions_obj, input, "Class distribution")
    class_distribution_spawn(r6, output, "Class distribution")
  })

  # Download associated table
  output$download_class_distribution_table = shiny::downloadHandler(
    filename = function(){"class_distribution_table.csv"},
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


#--------------------------------------------------------- Class comparison ----

class_comparison_generate = function(r6, colour_list, dimensions_obj, input, plot_name) {
  print_time(paste0(plot_name, ": generating plot."))

  if (input$class_comparison_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }



  r6$plot_class_comparison(col_group = input$class_comparison_metacol,
                             colour_list = colour_list,
                             width = width,
                             height = height)
}

class_comparison_spawn = function(r6, output, plot_name) {
  print_time(paste0(plot_name, ": spawning plot."))
  output$class_comparison_plot = plotly::renderPlotly(
    r6$plots$class_comparison
  )
}




class_comparison_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "class_comparison",
                 label = "Class comparison",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


class_comparison_server = function(r6, output, session) {

  ns = session$ns
  print_time("Class comparison: START.")

  output$class_comparison_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("class_comparison_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$params$class_comparison$group_col()
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::downloadButton(
        outputId = ns("download_class_comparison_table"),
        label = "Download unavailable for now",
        style = "width:100%;"
      )
    )
  })
}
class_comparison_events = function(r6, dimensions_obj, colour_list, input, output, session) {
  
  # Generate the plot
  shiny::observeEvent(input$class_comparison_metacol, {
    print_time(paste0("Class comparison: Updating params to ", input$class_comparison_metacol))
    r6$set_params_class_comparison(val = input$class_comparison_metacol)
    class_comparison_generate(r6, colour_list, dimensions_obj, input, "Class comparison")
    class_comparison_spawn(r6, output, "Class comparison")
  })

  
  # # Download associated table
  # output$download_class_comparison_table = shiny::downloadHandler(
  #   filename = function(){"class_comparison_table.csv"},
  #   content = function(file_name){
  #     write.csv(r6$tables$class_distribution_table, file_name)
  #   }
  # )


  # Expanded boxes
  class_comparison_proxy = plotly::plotlyProxy(outputId = "class_comparison_plot",
                                               session = session)

  shiny::observeEvent(input$class_comparison_plotbox,{
    if (input$class_comparison_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = class_comparison_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = class_comparison_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#------------------------------------------------------------- Volcano plot ----

volcano_plot_generate = function(r6, colour_list, dimensions_obj, input) {
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
                       group_1 = input$volcano_plot_metagroup[1],
                       group_2 = input$volcano_plot_metagroup[2])

  r6$plot_volcano(data_table = r6$tables$volcano_table,
                  colour_list = colour_list,
                  group_1 = input$volcano_plot_metagroup[1],
                  group_2 = input$volcano_plot_metagroup[2],
                  displayed_classes = input$volcano_plot_lipclass,
                  colouring = input$volcano_plot_colouring,
                  width = width,
                  height = height)

}


volcano_plot_spawn = function(r6, output) {
  print(paste0(get_time(), " - spawning plot."))
  output$volcano_plot_plot = plotly::renderPlotly(
    r6$plots$volcano_plot
  )
}

volcano_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "volcano_plot",
                 label = "Volcano plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


volcano_plot_server = function(r6, colour_list, dimensions_obj, input, output, session) {

  ns = session$ns

  # Set UI
  output$volcano_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("volcano_plot_tables"),
        label = "Select data table",
        choices = c("Filtered data table", "Class normalised data table", "Total normalised data table"),
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
        choices = NULL,
        selected = r6$params$volcano_plot$groups,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_lipclass"),
        label = "Classes to display",
        choices = unique(r6$tables$feat_filtered[,"lipid_class"]),
        selected = r6$params$volcano_plot$classes,
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
        inputId = ns("volcano_plot_colouring"),
        label = "Select colouring",
        choices = c("Lipid class", "Double bonds", "Total carbons"),
        selected = r6$params$volcano_plot$colouring,
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



  # Generate the plot
  first_run = shiny::reactiveVal(value = TRUE)

  shiny::observeEvent(c(shiny::req(length(input$volcano_plot_metagroup) == 2), input$volcano_plot_tables, input$volcano_plot_function, input$volcano_plot_colouring, input$volcano_plot_lipclass), {

    if (first_run()) {
      # If this is the first run : do nothing and switch first run off
      first_run(FALSE)

      if (is.null(r6$plots$volcano_plot)) {
        # If this is the first run but no plot is stored, create the plot
        volcano_plot_generate(r6, colour_list, dimensions_obj, input)
        volcano_plot_spawn(r6, output)
      } else {
        # If this is the first run but no plot is stored, spawn the stored plot
        volcano_plot_spawn(r6, output)
      }
    } else {
      # If this is not first run and parameters are changed, refresh plot and update parameters
      print(paste0(get_time(), " - Updating params."))

      r6$params$volcano_plot$data_table = input$volcano_plot_tables
      r6$params$volcano_plot$group_column = input$volcano_plot_metacol
      r6$params$volcano_plot$groups = input$volcano_plot_metagroup
      r6$params$volcano_plot$classes = input$volcano_plot_lipclass
      r6$params$volcano_plot$selected_function = input$volcano_plot_function
      r6$params$volcano_plot$colouring = input$volcano_plot_colouring

      volcano_plot_generate(r6, colour_list, dimensions_obj, input)
      volcano_plot_spawn(r6, output)

    }

  })




  # Export volcano table
  output$download_volcano_table = shiny::downloadHandler(
    filename = function(){"volcano_table.csv"},
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

heatmap_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "heatmap",
                 label = "Heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


heatmap_server = function(r6, colour_list, dimensions_obj, input, output, session) {

  ns = session$ns

  output$heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("heatmap_dataset"),
        label = "Select dataset",
        choices = c("Lipid species", "Lipid classes"),
        selected = "Lipid species"
      ),
      shiny::checkboxGroupInput(
        label = "Clustering",
        inputId = ns("heatmap_clustering"),
        choices = c("Cluster samples" = "cluster_rows",
                    "Cluster features" = "cluster_columns"),
        selected = NULL
      ),
      shiny::selectizeInput(
        inputId = ns("heatmap_map_rows"),
        label = "Map sample data",
        multiple = TRUE,
        choices = colnames(r6$tables$meta_filtered),
        selected = character(0)
      ),
      shiny::selectizeInput(
        inputId = ns("heatmap_map_cols"),
        label = "Map feature data",
        multiple = TRUE,
        choices = c("Class", "Carbon count", "Unsaturation count"),
        selected = character(0)
      ),
      shiny::sliderInput(inputId = ns("heatmap_percentile"),
                         label = "Percentile",
                         min = 90,
                         max = 100,
                         value = 99,
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


  shiny::observeEvent(input$heatmap_run,{
    if (input$heatmap_dataset == "Lipid species"){
      data_table = r6$tables$data_total_norm_z_scored
      col_annotations = input$heatmap_map_cols
    } else {
      data_table = r6$tables$data_class_table_z_scored
      col_annotations = NULL
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


    if (input$heatmap_plotbox$maximized) {
      width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
      height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
    } else {
      width = dimensions_obj$xpx * dimensions_obj$x_plot
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    }



    r6$plot_heatmap(data_table = data_table,
                    meta_table = r6$tables$meta_filtered,
                    meta_table_features = r6$tables$feat_filtered,
                    percentile = input$heatmap_percentile,
                    cluster_rows = cluster_rows,
                    cluster_cols = cluster_cols,
                    row_annotations = input$heatmap_map_rows,
                    col_annotations = col_annotations,
                    width = dimensions_obj$xpx * dimensions_obj$x_plot,
                    height = dimensions_obj$ypx * dimensions_obj$y_plot)

    output$heatmap_plot = plotly::renderPlotly(
      r6$plots$heatmap
    )

  })


  # Download associated table
  output$download_heatmap_table = shiny::downloadHandler(
    filename = function(){"heatmap_table.csv"},
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

pca_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "pca",
                 label = "PCA",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


pca_server = function(r6, colour_list, dimensions_obj, input, output, session) {

  ns = session$ns

  output$pca_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("pca_dataset"),
        label = "Select dataset",
        choices = c("Lipid species normalised", "Lipid class normalised"),
        selected = "Lipid species normalised"
      ),
      shiny::selectInput(
        inputId = ns("pca_metacol"),
        label = "Select metadata column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$texts$col_group
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

  shiny::observeEvent(c(input$pca_dataset, input$pca_metacol),{

    if (input$pca_dataset == "Lipid species normalised"){
      data_table = r6$tables$data_total_norm_z_scored
    } else {
      data_table = r6$tables$data_class_table_z_scored
    }

    if (input$pca_plotbox$maximized) {
      width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
      height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
    } else {
      width = dimensions_obj$xpx * dimensions_obj$x_plot
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    }



    r6$plot_pca(data_table = data_table,
                col_group = input$pca_metacol,
                width = width,
                height = height,
                colour_list = colour_list)


    output$pca_plot = plotly::renderPlotly(
      r6$plots$pca_plot
    )
  })


  # Download associated tables
  output$download_pca_scores_table = shiny::downloadHandler(
    filename = function(){"pca_scores_table.csv"},
    content = function(file_name){
      write.csv(r6$tables$pca_scores_table, file_name)
    }
  )
  output$download_pca_loadings_table = shiny::downloadHandler(
    filename = function(){"pca_loadings_table.csv"},
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


#-------------------------------------------------------- Double bonds plot ----

double_bonds_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "double_bonds",
                 label = "Double bonds plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


double_bonds_server = function(r6, colour_list, dimensions_obj, input, output, session) {

  ns = session$ns

  output$double_bonds_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("double_bonds_tables"),
        label = "Select data table",
        choices = c("Filtered data table", "Class normalised data table", "Total normalised data table"),
        selected = "Filtered data table"
      ),
      shiny::selectInput(
        inputId = ns("double_bonds_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$texts$col_group
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_metagroup"),
        label = "Select group(s)",
        choices = NULL,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_class"),
        label = "Select lipid class",
        choices = unique(r6$tables$feat_filtered$lipid_class),
        selected = unique(r6$tables$feat_filtered$lipid_class)[1],
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_function"),
        label = "Select function",
        choices = c("median", "mean"),
        selected = "median",
        multiple = FALSE
      ),
      shiny::sliderInput(
        inputId = ns("log2_fc_slider"),
        label = "Coloring : Log2(Fold change) slider",
        min = -5,
        max = 5,
        value = c(-1, 1),
        step = 0.1
      ),
      
      shiny::sliderInput(
        inputId = ns("min_log10_bh_pval_slider"),
        label = "Size : -Log10(BH(p-value)) slider",
        min = 0,
        max = 5,
        value = c(0,5),
        step = 0.1
      ),
      
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::downloadButton(
        outputId = ns("download_double_bond_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

  shiny::observeEvent(input$double_bonds_metacol,{
    shiny::updateSelectizeInput(
      inputId = "double_bonds_metagroup",
      session = session,
      choices = unique(r6$tables$meta_filtered[,input$double_bonds_metacol]),
      selected = unique(r6$tables$meta_filtered[,input$double_bonds_metacol])[c(1,2)]
    )
  })

  

  shiny::observeEvent(c(shiny::req(length(input$double_bonds_metagroup) == 1), input$double_bonds_class, input$double_bonds_tables, input$double_bonds_function), {

    if (input$double_bonds_plotbox$maximized) {
      width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
      height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
    } else {
      width = dimensions_obj$xpx * dimensions_obj$x_plot
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    }

    r6$get_dbplot_table_single(data_table = table_switch(selection = input$double_bonds_tables, r6 = r6),
                               dbplot_table = r6$tables$feat_filtered,
                               col_group = input$double_bonds_metacol,
                               used_function =  input$double_bonds_function,
                               group_1 = input$double_bonds_metagroup[1])

    r6$plot_doublebonds_single(lipid_class = input$double_bonds_class,
                               group_1 = input$double_bonds_metagroup[1],
                               width = width,
                               height = height)
    output$double_bonds_plot = plotly::renderPlotly(
      r6$plots$double_bond_plot
    )
    
  })
  
  
  shiny::observeEvent(c(shiny::req(length(input$double_bonds_metagroup) == 2), input$double_bonds_class, input$log2_fc_slider, input$min_log10_bh_pval_slider, input$double_bonds_tables, input$double_bonds_function),{
    
    if (input$double_bonds_plotbox$maximized) {
      width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
      height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
    } else {
      width = dimensions_obj$xpx * dimensions_obj$x_plot
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    }
    
    r6$get_dbplot_table_double(data_table = table_switch(selection = input$double_bonds_tables, r6 = r6),
                               dbplot_table = r6$tables$feat_filtered,
                               col_group = input$double_bonds_metacol,
                               used_function =  input$double_bonds_function,
                               group_1 = input$double_bonds_metagroup[1],
                               group_2 = input$double_bonds_metagroup[2])
    
    selected_rows = rownames(r6$tables$dbplot_table)[r6$tables$dbplot_table["lipid_class"] == input$double_bonds_class]
    fc_limits = round(max(abs(r6$tables$dbplot_table[selected_rows, "log2_fold_change"])), 1) + 1
    pval_limit = round(max(r6$tables$dbplot_table[selected_rows, "minus_log10_p_value_bh_adj"]), 1) + 1

    shiny::updateSliderInput(
      session = session,
      inputId = "log2_fc_slider",
      min = -fc_limits,
      max = fc_limits
    )

    shiny::updateSliderInput(
      session = session,
      inputId = "min_log10_bh_pval_slider",
      max = pval_limit
    )
    r6$plot_doublebonds_double(lipid_class = input$double_bonds_class,
                               fc_limits = input$log2_fc_slider,
                               pval_limits = input$min_log10_bh_pval_slider,
                               group_1 = input$double_bonds_metagroup[1],
                               group_2 = input$double_bonds_metagroup[2],
                               width = width,
                               height = height)
    output$double_bonds_plot = plotly::renderPlotly(
      r6$plots$double_bond_plot
    )

    
  })

  # Download associated tables
  output$download_double_bond_table = shiny::downloadHandler(
    filename = function(){"double_bond_table.csv"},
    content = function(file_name){
      write.csv(r6$tables$dbplot_table, file_name)
    }
  )
  
  # Expanded boxes
  double_bonds_proxy = plotly::plotlyProxy(outputId = "double_bonds_plot",
                                  session = session)

  shiny::observeEvent(input$double_bonds_plotbox,{
    if (input$double_bonds_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = double_bonds_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = double_bonds_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



