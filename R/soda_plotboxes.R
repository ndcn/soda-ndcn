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
    status = "primary",
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
      choices = colnames(r6$meta_filtered),
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
  

  
  
  # Expanded boxes tests
  
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
    status = "primary",
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
      choices = colnames(r6$meta_filtered),
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
  

  
  
  # Expanded boxes tests
  
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
    status = "primary",
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
        choices = colnames(r6$meta_filtered),
        selected = r6$col_group
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_metagroup"),
        label = "Select groups to compare(2)",
        choices = NULL,
        multiple = TRUE
      )
    )
  })
  
  shiny::observeEvent(input$volcano_plot_metacol,{
    shiny::updateSelectizeInput(
      inputId = "volcano_plot_metagroup",
      session = session,
      choices = unique(r6$meta_filtered[,input$volcano_plot_metacol]),
      selected = unique(r6$meta_filtered[,input$volcano_plot_metacol])[c(1,2)]
    )
  })
  
  
  shiny::observeEvent(input$volcano_plot_metagroup, {
    if (length(input$volcano_plot_metagroup) == 2) {
      
      if (input$class_comparison_plotbox$maximized) {
        width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
        height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
      } else {
        width = dimensions_obj$xpx * dimensions_obj$x_plot
        height = dimensions_obj$ypx * dimensions_obj$y_plot
      }
      
      r6$get_volcano_table(data_table = r6$data_filtered,
                           data_table_normalised = r6$data_z_scored,
                           col_group = input$volcano_plot_metacol,
                           group_1 = input$volcano_plot_metagroup[1],
                           group_2 = input$volcano_plot_metagroup[2])
      r6$plot_volcano(data_table = r6$volcano_table,
                      colour_list = colour_list,
                      width = width,
                      height = height)
      output$volcano_plot_plot = plotly::renderPlotly(
        r6$volcano_plot
      )
    }
  })
  
  # Expanded boxes tests
  
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


