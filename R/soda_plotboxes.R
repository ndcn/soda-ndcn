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




#------------------------------------------------------- Class distribution ----

class_distribution_ui = function(dimensions_obj, output, session) {
  
  ns = session$ns
  
  bs4Dash::box(
    id = ns("class_distribution_plotbox"),
    title = "Class distribution",
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns("class_distribution_sidebar"),
      width = 40,
      shiny::uiOutput(
        outputId = ns("class_distribution_sidebar_ui")
      )
    ),
    plotly::plotlyOutput(
      outputId = ns("class_distribution_plot"),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}
  
  
class_distribution_server = function(r6, colour_list, dimensions_obj, input, output, session) {
  
  ns = session$ns
  
  
  
  output$class_distribution_sidebar_ui = shiny::renderUI({
    shiny::selectInput(
      inputId = ns("class_distribution_metacol"),
      label = "Select group column",
      choices = colnames(r6$tables$meta_filtered),
      selected = r6$col_group
    )
  })
  shiny::observeEvent(input$class_distribution_metacol, {
    
    if (input$class_distribution_plotbox$maximized) {
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

    output$class_distribution_plot = plotly::renderPlotly(
      r6$class_distribution
    )
  })
  

  
  
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

class_comparison_ui = function(dimensions_obj, output, session) {
  
  ns = session$ns
  
  bs4Dash::box(
    id = ns("class_comparison_plotbox"),
    title = "Class comparison",
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns("class_comparison_sidebar"),
      width = 40,
      shiny::uiOutput(
        outputId = ns("class_comparison_sidebar_ui")
      )
    ),
    plotly::plotlyOutput(
      outputId = ns("class_comparison_plot"),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}


class_comparison_server = function(r6, colour_list, dimensions_obj, input, output, session) {
  
  ns = session$ns
  
  
  
  output$class_comparison_sidebar_ui = shiny::renderUI({
    shiny::selectInput(
      inputId = ns("class_comparison_metacol"),
      label = "Select group column",
      choices = colnames(r6$tables$meta_filtered),
      selected = r6$col_group
    )
  })
  shiny::observeEvent(input$class_comparison_metacol, {
    
    if (input$class_comparison_plotbox$maximized) {
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
    
    output$class_comparison_plot = plotly::renderPlotly(
      r6$class_comparison
    )
  })
  

  
  
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

volcano_plot_ui = function(dimensions_obj, output, session) {
  
  ns = session$ns
  
  bs4Dash::box(
    id = ns("volcano_plot_plotbox"),
    title = "Volcano plot",
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns("volcano_plot_sidebar"),
      width = 40,
      shiny::uiOutput(
        outputId = ns("volcano_plot_sidebar_ui")
      )
    ),
    plotly::plotlyOutput(
      outputId = ns("volcano_plot_plot"),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}


volcano_plot_server = function(r6, colour_list, dimensions_obj, input, output, session) {
  
  ns = session$ns
  
  output$volcano_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("volcano_plot_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$col_group
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_metagroup"),
        label = "Select groups to compare(2)",
        choices = NULL,
        multiple = TRUE
      ),
      shiny::br(),
      
      shiny::downloadButton(
        outputId = ns("volcano_download"),
        label = "Download volcano table",
        style = "width:100%;"
      )
    )
  })
  
  shiny::observeEvent(input$volcano_plot_metacol,{
    shiny::updateSelectizeInput(
      inputId = "volcano_plot_metagroup",
      session = session,
      choices = unique(r6$tables$meta_filtered[,input$volcano_plot_metacol]),
      selected = unique(r6$tables$meta_filtered[,input$volcano_plot_metacol])[c(1,2)]
    )
  })
  
  
  shiny::observeEvent(input$volcano_plot_metagroup, {
    if (length(input$volcano_plot_metagroup) == 2) {
      
      if (input$volcano_plot_plotbox$maximized) {
        width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
        height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
      } else {
        width = dimensions_obj$xpx * dimensions_obj$x_plot
        height = dimensions_obj$ypx * dimensions_obj$y_plot
      }
      
      r6$get_volcano_table(data_table = r6$tables$data_filtered,
                           data_table_normalised = r6$tables$data_z_scored,
                           col_group = input$volcano_plot_metacol,
                           group_1 = input$volcano_plot_metagroup[1],
                           group_2 = input$volcano_plot_metagroup[2])
      r6$plot_volcano(data_table = r6$tables$volcano_table,
                      colour_list = colour_list,
                      width = width,
                      height = height)
      output$volcano_plot_plot = plotly::renderPlotly(
        r6$volcano_plot
      )
    }
  })
  
  
  
  # Export volcano table
  output$volcano_download = shiny::downloadHandler(
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

heatmap_ui = function(dimensions_obj, output, session) {
  
  ns = session$ns
  
  bs4Dash::box(
    id = ns("heatmap_plotbox"),
    title = "Heat map",
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns("heatmap_sidebar"),
      width = 40,
      shiny::uiOutput(
        outputId = ns("heatmap_sidebar_ui")
      )
    ),
    plotly::plotlyOutput(
      outputId = ns("heatmap_plot"),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
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
                    "Cluster features" = "cluster_columns")
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
      shiny::actionButton(
        inputId = ns("heatmap_run"),
        label = "Generate heatmap"
      ),
      shiny::sliderInput(inputId = ns("heatmap_percentile"),
                         label = "Percentile",
                         min = 90,
                         max = 100,
                         value = 99,
                         step = 1
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
      r6$heatmap
    )

  })
  
  
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

pca_ui = function(dimensions_obj, output, session) {
  
  ns = session$ns
  
  bs4Dash::box(
    id = ns("pca_plotbox"),
    title = "PCA",
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns("pca_sidebar"),
      width = 40,
      shiny::uiOutput(
        outputId = ns("pca_sidebar_ui")
      )
    ),
    plotly::plotlyOutput(
      outputId = ns("pca_plot"),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
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
        selected = r6$col_group
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
      r6$pca_plot
    )   
  })
  
  
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

double_bonds_ui = function(dimensions_obj, output, session) {
  
  ns = session$ns
  
  bs4Dash::box(
    id = ns("double_bonds_plotbox"),
    title = "Double bonds plot",
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns("double_bonds_sidebar"),
      width = 40,
      shiny::uiOutput(
        outputId = ns("double_bonds_sidebar_ui")
      )
    ),
    plotly::plotlyOutput(
      outputId = ns("double_bonds_plot"),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}


double_bonds_server = function(r6, colour_list, dimensions_obj, input, output, session) {
  
  ns = session$ns
  
  output$double_bonds_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("double_bonds_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$meta_filtered),
        selected = r6$col_group
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_metagroup"),
        label = "Select groups to compare(2)",
        choices = NULL,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_class"),
        label = "Select lipid class",
        choices = unique(r6$tables$feat_filtered$lipid_class),
        selected = unique(r6$tables$feat_filtered$lipid_class)[1],
        multiple = FALSE
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
  
  shiny::observeEvent(c(input$double_bonds_metacol, input$double_bonds_metagroup, input$double_bonds_class),{
    
    if (length(input$double_bonds_metagroup) == 2) {
      
      if (input$double_bonds_plotbox$maximized) {
        width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
        height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
      } else {
        width = dimensions_obj$xpx * dimensions_obj$x_plot
        height = dimensions_obj$ypx * dimensions_obj$y_plot
      }
      
      r6$get_dbplot_table(data_table = r6$tables$data_filtered,
                          data_table_normalised = r6$tables$data_z_scored,
                          dbplot_table = r6$tables$feat_filtered,
                          col_group = input$double_bonds_metacol,
                          group_1 = input$double_bonds_metagroup[1],
                          group_2 = input$double_bonds_metagroup[2])
      
      r6$plot_doublebonds(lipid_class = input$double_bonds_class,
                          width = width,
                          height = height)
      
      output$double_bonds_plot = plotly::renderPlotly(
        r6$double_bond_plot
      )
    }
  })
  
  
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

    

