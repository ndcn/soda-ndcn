
plot_one = function(r6, dimensions_obj, selection_list, colour_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


plot_two = function(r6, dimensions_obj, selection_list, colour_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_three = function(r6, dimensions_obj, selection_list, colour_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

plot_four = function(r6, dimensions_obj, selection_list, colour_list, input, output, session) {
  ns = session$ns
  ui_functions = plotbox_switch_ui(selection_list = selection_list)
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

  plot_servers = plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}




get_plot_list = function() {
  plot_list = c("Class distribution" = "select_class_distribution",
              "Class comparison" = "select_class_comparison",
              "Volcano plot" = "select_volcano_plot",
              "Heatmap" = "select_heatmap",
              "PCA" = "select_pca",
              "Double bond plot" = "select_double_bond_plot"
  )
  return(plot_list)
}

#----------------------------------------- Lipidomics data visualisation UI ----
soda_visualise_lips_ui = function(id) {
  ns = NS(id)

  shiny::tagList(
    shinybrowser::detect(),
    shiny::fluidRow(
      shiny::column(
        width = 11,
        shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                           label = NULL,
                                           status = "default",
                                           choices = get_plot_list(),
                                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                           size = "normal",
                                           justified = TRUE
                                           )
      ),
      shiny::column(
        width = 1,
        shinyWidgets::actionBttn(inputId = ns("clear_plots"),
                                 label = "Clear plots",
                                 style = "stretch",
                                 color = "danger")
      )
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

      # Plotting events
      class_distribution_events(r6, dimensions_obj, colour_list, input, output, session)
      class_comparison_events(r6, dimensions_obj, colour_list, input, output, session)
      volcano_plot_events(r6, dimensions_obj, colour_list, input, output, session)
      heatmap_events(r6, dimensions_obj, colour_list, input, output, session)
      pca_events(r6, dimensions_obj, colour_list, input, output, session)
      db_plot_events(r6, dimensions_obj, colour_list, input, output, session)
      
      # Plot selection
      shiny::observeEvent(input$showPlots, {
        
        # Plots selected: 1 to 4
        print_time(paste0("Plot selection: ", paste(input$showPlots, collapse = ", ")))
        if (length(input$showPlots) == 1) {
          plot_one(r6 = r6,
                   dimensions_obj = dimensions_obj,
                   selection_list = input$showPlots,
                   colour_list = colour_list,
                   input = input,
                   output = output,
                   session = session)

        } else if (length(input$showPlots) == 2) {
          plot_two(r6 = r6,
                   dimensions_obj = dimensions_obj,
                   selection_list = input$showPlots,
                   colour_list = colour_list,
                   input = input,
                   output = output,
                   session = session)

        } else if (length(input$showPlots) == 3) {
          plot_three(r6 = r6,
                   dimensions_obj = dimensions_obj,
                   selection_list = input$showPlots,
                   colour_list = colour_list,
                   input = input,
                   output = output,
                   session = session)

        } else if (length(input$showPlots) >= 4) {
          plot_four(r6 = r6,
                   dimensions_obj = dimensions_obj,
                   selection_list = input$showPlots,
                   colour_list = colour_list,
                   input = input,
                   output = output,
                   session = session)

          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = setdiff(unname(get_plot_list()), input$showPlots)
          )

        }
        if (between(length(input$showPlots), 2, 3)) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = NULL
          )
        } else if (length(input$showPlots) == 1) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = input$showPlots
          )
        }
      })
      
      shiny::observeEvent(input$clear_plots, {
        print_time("Clearing plots")
        shinyWidgets::updateCheckboxGroupButtons(inputId = "showPlots",
                                                 disabled = FALSE,
                                                 selected = character(0))
        output$plotbox_field = shiny::renderUI(
          NULL
        )
      })
      
    }
  )
}
