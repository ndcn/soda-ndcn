library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinybrowser)
library(plotly)

plot_one = function(dimensions_obj, output, session) {
  ns = session$ns
  print(1)
  
  output$plotbox_field = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        double_bonds_ui(dimensions_obj, output, session) 
      )
    )
  })
}
plot_two = function(dimensions_obj, output, session) {
  ns = session$ns
  print(2)
  output$plotbox_field = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        class_comparison_ui(dimensions_obj, output, session),
        class_distribution_ui(dimensions_obj, output, session)
      )
    )
  })

}
plot_three = function(dimensions_obj, output, session) {
  ns = session$ns
  print(3)
  output$plotbox_field = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        class_comparison_ui(dimensions_obj, output, session),
        class_distribution_ui(dimensions_obj, output, session),
        volcano_plot_ui(dimensions_obj, output, session)
      )
    )
  })
}
plot_four = function(dimensions_obj, output, session) {
  ns = session$ns
  print(4)
  output$plotbox_field = shiny::renderUI({
    shiny::tagList(
      shiny::fluidRow(
        class_comparison_ui(dimensions_obj, output, session),
        class_distribution_ui(dimensions_obj, output, session),
        volcano_plot_ui(dimensions_obj, output, session),
        heatmap_ui(dimensions_obj, output, session) 
      )
    )
  })
}


plot_switch = function(plot_selected){
  switch(
    EXPR = plot_selected,
    "select_class_distribution" = c("Class distribution", "box_class_distribution", "spawn_class_distribution", "sidebar_class_distribution", "sidebar_class_distribution_ui"),
    "select_class_comparison" = c("Class comparison", "box_class_comparison", "spawn_class_comparison", "sidebar_class_comparison", "sidebar_class_comparison_ui"),
    "select_volcano_plot" = c("Volcano plot", "box_volcano_plot", "spawn_volcano_plot", "sidebar_volcano_plot", "sidebar_volcano_plot_ui"),
    "select_heatmap" = c("Heatmap", "box_heatmap", "spawn_heatmap", "sidebar_heatmap", "sidebar_heatmap_ui"),
    "select_pca" = c("PCA", "box_pca", "spawn_pca", "sidebar_pca", "sidebar_pca_ui"),
    "select_dbplot" = c("Double bond plot", "box_dbplot", "spawn_dbplot", "sidebar_dbplot", "sidebar_dbplot_ui")
  )
}

get_plot_list = function() {
  plot_list = c("Class distribution" = "select_class_distribution",
              "Class comparison" = "select_class_comparison",
              "Volcano plot" = "select_volcano_plot",
              "Heatmap" = "select_heatmap",
              "PCA" = "select_pca",
              "Double bond plot" = "select_dbplot"
  )
  return(plot_list)
}

#----------------------------------------- Lipidomics data visualisation UI ----
soda_visualise_lips_ui = function(id) {
  ns = NS(id)
  
  shiny::tagList(
    shinybrowser::detect(),
    shiny::fluidRow(
      shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                         label = NULL,
                                         status = "default",
                                         choices = get_plot_list(),
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
      
      
      ## Create a dimensions object to store browser dimensions
      dimensions_obj = shiny::reactiveValues()
      
      # Constant values
      dimensions_obj$x_box = 0.9
      dimensions_obj$y_box = 0.8
      dimensions_obj$x_plot = 0.8
      dimensions_obj$y_plot = 0.75
      dimensions_obj$x_plot_full = 0.95
      dimensions_obj$y_plot_full = 0.91
      
      shiny::observe({
        dimensions_obj$xpx_total = shinybrowser::get_width()
        dimensions_obj$ypx_total = shinybrowser::get_height()
      })

      # Reactive values
      shiny::observeEvent(input$showPlots,{
        
        # x (width) in BS (Bootstrap) values
        if (length(input$showPlots) < 2) {
          dimensions_obj$xbs = 12
        } else {
          dimensions_obj$xbs = 6
        }
        
        # x (width) in pixels
        if (length(input$showPlots) < 2) {
          dimensions_obj$xpx = shinybrowser::get_width()
        } else {
          dimensions_obj$xpx = shinybrowser::get_width()/2
        }
        
        # y (height) in pixels
        if (length(input$showPlots) < 3) {
          dimensions_obj$ypx = shinybrowser::get_height()
        } else {
          dimensions_obj$ypx = shinybrowser::get_height()/2.1
        }
        
        
      })
      

      
      # Plotting nightmare
      shiny::observeEvent(input$showPlots, {
        
        if (length(input$showPlots) == 0) {
          output$plotbox_field = shiny::renderUI(
            NULL
          )
        } else if (length(input$showPlots) == 1) {
          plot_one(dimensions_obj, output, session)
          double_bonds_server(r6, colour_list, dimensions_obj, input, output, session)
        } else if (length(input$showPlots) == 2) {
          plot_two(dimensions_obj, output, session)
          class_comparison_server(r6, colour_list, dimensions_obj, input, output, session)
          class_distribution_server(r6, colour_list, dimensions_obj, input, output, session)
        } else if (length(input$showPlots) == 3) {
          plot_three(dimensions_obj, output, session)
          class_comparison_server(r6, colour_list, dimensions_obj, input, output, session)
          class_distribution_server(r6, colour_list, dimensions_obj, input, output, session)
          volcano_plot_server(r6, colour_list, dimensions_obj, input, output, session)
        } else if (length(input$showPlots) >= 4) {
          plot_four(dimensions_obj, output, session)
          class_comparison_server(r6, colour_list, dimensions_obj, input, output, session)
          class_distribution_server(r6, colour_list, dimensions_obj, input, output, session)
          volcano_plot_server(r6, colour_list, dimensions_obj, input, output, session)
          heatmap_server(r6, colour_list, dimensions_obj, input, output, session)
          
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = setdiff(unname(get_plot_list()), input$showPlots)
          )
          
        }
        if (length(input$showPlots) < 4) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = NULL
          )
        }
      })
      
      
      # Expanded boxes tests
      shiny::observeEvent(input$truffle_box,{
        if (input$truffle_box$maximized) {
          print("Truffles")
        }
      })
    }
  )
}
