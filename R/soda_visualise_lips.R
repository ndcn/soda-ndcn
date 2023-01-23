library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinybrowser)
library(plotly)

spawn_empty_plotbox = function(id_box, id_sidebar, id_sidebar_ui, id_plot, label, width_bs, height_px, y_box, y_plot, session){
  ns = session$ns
  bs4Dash::box(
    id = ns(id_box),
    title = label,
    width = width_bs,
    height = height_px * y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "primary",
    sidebar = bs4Dash::boxSidebar(
      id = ns(id_sidebar),
      width = 40,
      shiny::uiOutput(
        outputId = ns(id_sidebar_ui)
      )
    ),
    plotly::plotlyOutput(
      outputId = ns(id_plot),
      width = width_bs,
      height = height_px * y_plot
    )
  )
}

spawn_grid = function(plot_list, width_bs, reactive_ypx, y_box, y_plot, session, output) {
  if (length(plot_list) == 0) {
    output$plotbox_field = shiny::renderUI({NULL})
  } else if (length(plot_list) == 1) {
    output$plotbox_field = shiny::renderUI({
      spawn_empty_plotbox(id_box = plot_switch(plot_list[1])[2],
                          id_sidebar = plot_switch(plot_list[1])[4],
                          id_sidebar_ui = plot_switch(plot_list[1])[5],
                          id_plot = plot_switch(plot_list[1])[3],
                          label = plot_switch(plot_list[1])[1],
                          width_bs = width_bs,
                          height_px = reactive_ypx,
                          y_box = y_box,
                          y_plot = y_plot,
                          session = session)
    })
  } else if (length(plot_list) == 2) {
    output$plotbox_field = shiny::renderUI({
      shiny::fluidRow(
        spawn_empty_plotbox(id_box = plot_switch(plot_list[1])[2],
                            id_sidebar = plot_switch(plot_list[1])[4],
                            id_sidebar_ui = plot_switch(plot_list[1])[5],
                            id_plot = plot_switch(plot_list[1])[3],
                            label = plot_switch(plot_list[1])[1],
                            width_bs = width_bs,
                            height_px = reactive_ypx,
                            y_box = y_box,
                            y_plot = y_plot,
                            session = session),
        spawn_empty_plotbox(id_box = plot_switch(plot_list[2])[2],
                            id_sidebar = plot_switch(plot_list[2])[4],
                            id_sidebar_ui = plot_switch(plot_list[2])[5],
                            id_plot = plot_switch(plot_list[2])[3],
                            label = plot_switch(plot_list[2])[1],
                            width_bs = width_bs,
                            height_px = reactive_ypx,
                            y_box = y_box,
                            y_plot = y_plot,
                            session = session)
      )
    })
  } else if (length(plot_list) == 3) {
    output$plotbox_field = shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          spawn_empty_plotbox(id_box = plot_switch(plot_list[1])[2],
                              id_sidebar = plot_switch(plot_list[1])[4],
                              id_sidebar_ui = plot_switch(plot_list[1])[5],
                              id_plot = plot_switch(plot_list[1])[3],
                              label = plot_switch(plot_list[1])[1],
                              width_bs = width_bs,
                              height_px = reactive_ypx,
                              y_box = y_box,
                              y_plot = y_plot,
                              session = session),
          spawn_empty_plotbox(id_box = plot_switch(plot_list[2])[2],
                              id_sidebar = plot_switch(plot_list[2])[4],
                              id_sidebar_ui = plot_switch(plot_list[2])[5],
                              id_plot = plot_switch(plot_list[2])[3],
                              label = plot_switch(plot_list[2])[1],
                              width_bs = width_bs,
                              height_px = reactive_ypx,
                              y_box = y_box,
                              y_plot = y_plot,
                              session = session)
        ),
        shiny::fluidRow(
          spawn_empty_plotbox(id_box = plot_switch(plot_list[3])[2],
                              id_sidebar = plot_switch(plot_list[3])[4],
                              id_sidebar_ui = plot_switch(plot_list[3])[5],
                              id_plot = plot_switch(plot_list[3])[3],
                              label = plot_switch(plot_list[3])[1],
                              width_bs = width_bs,
                              height_px = reactive_ypx,
                              y_box = y_box,
                              y_plot = y_plot,
                              session = session)
        )
      )
    })
  } else if (length(plot_list) == 4){
    output$plotbox_field = shiny::renderUI({
      shiny::tagList(
        shiny::fluidRow(
          spawn_empty_plotbox(id_box = plot_switch(plot_list[1])[2],
                              id_sidebar = plot_switch(plot_list[1])[4],
                              id_sidebar_ui = plot_switch(plot_list[1])[5],
                              id_plot = plot_switch(plot_list[1])[3],
                              label = plot_switch(plot_list[1])[1],
                              width_bs = width_bs,
                              height_px = reactive_ypx,
                              y_box = y_box,
                              y_plot = y_plot,
                              session = session),
          spawn_empty_plotbox(id_box = plot_switch(plot_list[2])[2],
                              id_sidebar = plot_switch(plot_list[2])[4],
                              id_sidebar_ui = plot_switch(plot_list[2])[5],
                              id_plot = plot_switch(plot_list[2])[3],
                              label = plot_switch(plot_list[2])[1],
                              width_bs = width_bs,
                              height_px = reactive_ypx,
                              y_box = y_box,
                              y_plot = y_plot,
                              session = session)
        ),
        shiny::fluidRow(
          spawn_empty_plotbox(id_box = plot_switch(plot_list[3])[2],
                              id_sidebar = plot_switch(plot_list[3])[4],
                              id_sidebar_ui = plot_switch(plot_list[3])[5],
                              id_plot = plot_switch(plot_list[3])[3],
                              label = plot_switch(plot_list[3])[1],
                              width_bs = width_bs,
                              height_px = reactive_ypx,
                              y_box = y_box,
                              y_plot = y_plot,
                              session = session),
          spawn_empty_plotbox(id_box = plot_switch(plot_list[4])[2],
                              id_sidebar = plot_switch(plot_list[4])[4],
                              id_sidebar_ui = plot_switch(plot_list[4])[5],
                              id_plot = plot_switch(plot_list[4])[3],
                              label = plot_switch(plot_list[4])[1],
                              width_bs = width_bs,
                              height_px = reactive_ypx,
                              y_box = y_box,
                              y_plot = y_plot,
                              session = session)
        )
      )
    })
  }
}

spawn_plotbox = function(plot_list, colour_list, width_bs, reactive_xpx, reactive_ypx, x_plot, y_plot, y_box, r6, input, output, session) {
  
  ns = session$ns
  
  # Spawn an empty plotbox
  spawn_grid(plot_list = plot_list,
             width_bs = width_bs,
             reactive_ypx = reactive_ypx,
             y_box = y_box,
             y_plot = y_plot,
             session = session,
             output = output)
  
  for (plot_data in plot_list) {
    plot_data = plot_switch(plot_data)
    id_box = plot_data[2]
    id_plot = plot_data[3]
    label = plot_data[1]
    
    # Insert appropriate plot
    if (id_plot == "spawn_class_distribution"){
      output$sidebar_class_distribution_ui = shiny::renderUI({
        shiny::selectInput(
          inputId = ns("sidebar_class_distribution_meta_select"),
          label = "Select group column",
          choices = colnames(r6$meta_filtered),
          selected = r6$col_group
        )
      })
      shiny::observeEvent(input$sidebar_class_distribution_meta_select, {
        r6$plot_class_distribution(col_group = input$sidebar_class_distribution_meta_select,
                                   colour_list = colour_list,
                                   width = reactive_xpx * x_plot,
                                   height = reactive_ypx * y_plot)
        
        output$spawn_class_distribution = plotly::renderPlotly(
          r6$class_distribution
        )
      })

    } else if (id_plot == "spawn_class_comparison"){
      
      
      output$sidebar_class_comparison_ui = shiny::renderUI({
        shiny::selectInput(
          inputId = ns("sidebar_class_comparison_meta_select"),
          label = "Select group column",
          choices = colnames(r6$meta_filtered),
          selected = r6$col_group
        )
      })
      
      shiny::observeEvent(input$sidebar_class_comparison_meta_select, {
        r6$plot_class_comparison(col_group = input$sidebar_class_comparison_meta_select,
                                 colour_list = colour_list,
                                 width = reactive_xpx * x_plot,
                                 height = reactive_ypx * y_plot)
        
        output$spawn_class_comparison = plotly::renderPlotly(
          r6$class_comparison
        )
        
      })

    } else if (id_plot == "spawn_volcano_plot"){
      
      output$sidebar_volcano_plot_ui = shiny::renderUI({
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("sidebar_volcano_plot_meta_select"),
            label = "Select group column",
            choices = colnames(r6$meta_filtered),
            selected = r6$col_group
          ),
          shiny::selectizeInput(
            inputId = ns("sidebar_volcano_plot_group_select"),
            label = "Select groups to compare(2)",
            choices = NULL,
            multiple = TRUE
          )
        )
      })
      
      shiny::observeEvent(input$sidebar_volcano_plot_meta_select,{
        shiny::updateSelectizeInput(
          inputId = "sidebar_volcano_plot_group_select",
          session = session,
          choices = unique(r6$meta_filtered[,input$sidebar_volcano_plot_meta_select]),
          selected = unique(r6$meta_filtered[,input$sidebar_volcano_plot_meta_select])[c(1,2)]
        )
      })
      
      
      shiny::observeEvent(input$sidebar_volcano_plot_group_select, {
        if (length(input$sidebar_volcano_plot_group_select) == 2) {
          r6$get_volcano_table(data_table = r6$data_filtered,
                               col_group = input$sidebar_volcano_plot_meta_select,
                               group_1 = input$sidebar_volcano_plot_group_select[1],
                               group_2 = input$sidebar_volcano_plot_group_select[2],
                               impute = NA)
          r6$plot_volcano(data_table = self$volcano_table,
                          colour_list = colour_list,
                          width = reactive_xpx * x_plot,
                          height = reactive_ypx * y_plot)
          output$spawn_volcano_plot = plotly::renderPlotly(
            r6$volcano_plot
          )
        }
      })

    } else if (id_plot == "spawn_heatmap"){
      
      output$sidebar_heatmap_ui = shiny::renderUI({
        shiny::tagList(
          shiny::selectInput(
            inputId = ns("sidebar_heatmap_dataset"),
            label = "Select dataset",
            choices = c("Lipid species", "Lipid classes"),
            selected = character(0)
          ),
          shiny::checkboxGroupInput(
            label = "Clustering",
            inputId = ns("sidebar_heatmap_clustering"),
            choices = c("Cluster samples" = "cluster_rows",
                        "Cluster features" = "cluster_columns")
          ),
          shiny::selectizeInput(
            inputId = ns("sidebar_heatmap_map_rows"),
            label = "Map sample data",
            choices = NULL
          ),
          shiny::selectizeInput(
            inputId = ns("sidebar_heatmap_map_cols"),
            label = "Map feature data",
            choices = NULL
          ),
          shiny::actionButton(
            inputId = ns("sidebar_heatmap_run"),
            label = "Generate heatmap"
          )
        )
      })
      
      
      output$spawn_heatmap = plotly::renderPlotly(
        spawn_plot(width_px = reactive_xpx*x_plot,
                   height_px = reactive_ypx*y_plot,
                   plot_bgcolor = '#E103D1')
      )
    }
  }
}


spawn_plot = function(label = NULL, width_px, height_px, plot_bgcolor='#e5ecf6'){
  plotly::plot_ly(x = c(0,1, 2), y = c(2, 1, 3), type = 'bar', width = width_px, height = height_px) %>%
    layout(title = label,
           plot_bgcolor=plot_bgcolor,
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'))
}

plot_switch = function(plot_selected){
  switch(
    EXPR = plot_selected,
    "select_class_distribution" = c("Class distribution", "box_class_distribution", "spawn_class_distribution", "sidebar_class_distribution", "sidebar_class_distribution_ui"),
    "select_class_comparison" = c("Class comparison", "box_class_comparison", "spawn_class_comparison", "sidebar_class_comparison", "sidebar_class_comparison_ui"),
    "select_volcano_plot" = c("Volcano plot", "box_volcano_plot", "spawn_volcano_plot", "sidebar_volcano_plot", "sidebar_volcano_plot_ui"),
    "select_heatmap" = c("Heatmap", "box_heatmap", "spawn_heatmap", "sidebar_heatmap", "sidebar_heatmap_ui")
  )
}

#----------------------------------------- Lipidomics data visualisation UI ----
soda_visualise_lips_ui = function(id) {
  ns = NS(id)
  
  shiny::tagList(
    shinybrowser::detect(),
    shiny::fluidRow(
      shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                         label = NULL,
                                         status = "primary",
                                         choices = c("Class distribution" = "select_class_distribution",
                                                     "Class comparison" = "select_class_comparison",
                                                     "Volcano plot" = "select_volcano_plot",
                                                     "Heatmap" = "select_heatmap"),
                                         checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))

      

    ),
    shiny::uiOutput(
      outputId = ns("plotbox_field")
    )
  )


  

}

#------------------------------------- Lipidomics data visualisation server ----
soda_visualise_lips_server = function(id, r6, colour_list) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      
      # Constant dimension ratios
      x_box = 0.9
      y_box = 0.7
      x_plot = 0.8
      y_plot = 0.65
      
      # Reactive dimensions
      reactive_xbs = shiny::reactive({
        if (length(input$showPlots) < 2) {
          12
        } else {
          6
        }
      })
      
      reactive_xpx = shiny::reactive({
        if (length(input$showPlots) < 2) {
          shinybrowser::get_width()
        } else {
          shinybrowser::get_width()/2
        }
      })
      
      reactive_ypx = shiny::reactive({
        if (length(input$showPlots) < 3) {
          shinybrowser::get_height()
        } else {
          shinybrowser::get_height()/2
        }
      })      
      
      # Plotting nightmare
      shiny::observeEvent(input$showPlots, {
        
        if (length(input$showPlots) > 0) {
          spawn_plotbox(plot_list = input$showPlots,
                        colour_list = colour_list,
                        width_bs = reactive_xbs(),
                        reactive_xpx = reactive_xpx(),
                        reactive_ypx = reactive_ypx(),
                        x_plot = x_plot,
                        y_plot = y_plot,
                        y_box = y_box,
                        r6 = r6,
                        input = input,
                        output = output,
                        session = session
                        )

        } else {
          output$plotbox_field = shiny::renderUI(
            NULL
          )
        }
      })
    }
  )
}
