#----------------------------------------------------------- SNF utilities ----

snf_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_clusters_heatmap_1" = clusters_heatmap_1_ui,
                                          "select_clusters_heatmap_2" = clusters_heatmap_2_ui,
                                          'select_similarity_network_1' = similarity_network_1_ui,
                                          'select_similarity_network_2' = similarity_network_2_ui,
                                          'select_fusion_heatmap' = fusion_heatmap_ui,
                                          'select_similarity_network_fusion' = similarity_network_fusion_ui)
    )
  }
  return(ui_functions)
}

snf_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_clusters_heatmap_1" = clusters_heatmap_1_server,
                                                  "select_clusters_heatmap_2" = clusters_heatmap_2_server,
                                                  'select_similarity_network_1' = similarity_network_1_server,
                                                  'select_similarity_network_2' = similarity_network_2_server,
                                                  'select_fusion_heatmap' = fusion_heatmap_server,
                                                  'select_similarity_network_fusion' = similarity_network_fusion_server)
    )
  }
  return(server_functions)
}

snf_plot_one = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = snf_plotbox_switch_ui(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = snf_plotbox_switch_server(selection_list = input$show_plots_snf)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


snf_plot_two = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = snf_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = snf_plotbox_switch_server(selection_list = input$show_plots_snf)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

snf_plot_three = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = snf_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = snf_plotbox_switch_server(selection_list = input$show_plots_snf)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

snf_plot_four = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = snf_plotbox_switch_ui(selection_list = selection_list)
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

  plot_servers = snf_plotbox_switch_server(selection_list = input$show_plots_snf)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

#------------------------------------------------------------------ SNF UI ----
snf_ui = function(id) {
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",

    #------------------------------------------------------------ Setup tab ----
    shiny::tabPanel(
      title = "Setup",
      shiny::fluidRow(
        shiny::column(
          width=12,

          # Modules box
          bs4Dash::box(
            id = ns('module_box'),
            title = 'Available data',
            width = 12,
            shiny::tagList(
              shiny::fluidRow(
                shiny::uiOutput(
                  outputId = ns('exp_1_data')
                )
              ),
              shiny::fluidRow(
                shiny::uiOutput(
                  outputId = ns('exp_2_data')
                )
              ),
              shiny::fluidRow(
                shiny::uiOutput(
                  outputId = ns('exp_3_data')
                )
              ),
              shiny::fluidRow(
                shiny::uiOutput(
                  outputId = ns('exp_4_data')
                )
              ),
              shiny::fluidRow(
                shiny::uiOutput(
                  outputId = ns('exp_5_data')
                )
              ),
              shiny::fluidRow(
                shiny::uiOutput(
                  outputId = ns('exp_6_data')
                ),
              )
            ),
            collapsible = T,
            collapsed  = F,
            maximizable = F,
            headerBorder = T
          )
        )
      ),
      shiny::fluidRow(
        shiny::h4('Parameters')
      ),
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::br(),
          shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
          shiny::br()
        ),
        shiny::column(width = 3),
        shiny::column(
          width = 6,
          shinyWidgets::actionBttn(
            inputId = ns("run_snf"),
            label = "Go!",
            color = "success",
            style = "material-flat",
            icon = icon("sliders"),
            block = TRUE
          )
        ),
        shiny::column(width = 3)

      )
    ),
    shiny::tabPanel(
      title = "Visualization",
      shiny::fluidRow(
        shiny::column(
          width = 11,
          shinyWidgets::checkboxGroupButtons(inputId = ns("show_plots_snf"),
                                             label = NULL,
                                             status = "default",
                                             choices = get_snf_plot_list(),
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                             size = "normal",
                                             justified = TRUE)
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
      shiny::uiOutput(
        outputId = ns("plotbox_field")
      )
    )
  )

}

#--------------------------------------------------------------- SNF server ----
snf_server = function(id, r6, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns

      # Omics modules loaded
      shiny::observe({
        if (!is.null(module_controler$exp_r6$exp_1)) {
          output$exp_1_data = shiny::renderUI({
            shiny::fluidRow(
              shiny::checkboxInput(
                inputId = ns('select_exp_1'),
                label = NULL
              ),
              shiny::p(paste0(' Name: ', module_controler$exp_r6$exp_1$name)),
              shiny::p(paste0(' Type: ', module_controler$exp_r6$exp_1$type)),
              shiny::p(paste0(' Samples: ', as.character(nrow(module_controler$exp_r6$exp_1$tables$raw_data))))
            )
          })
        }

        if (!is.null(module_controler$exp_r6$exp_2)) {
          output$exp_2_data = shiny::renderUI({
            shiny::fluidRow(
              shiny::checkboxInput(
                inputId = ns('select_exp_2'),
                label = NULL
              ),
              shiny::p(paste0(' Name: ', module_controler$exp_r6$exp_2$name)),
              shiny::p(paste0(' Type: ', module_controler$exp_r6$exp_2$type)),
              shiny::p(paste0(' Samples: ', as.character(nrow(module_controler$exp_r6$exp_2$tables$raw_data))))
            )
          })
        }

        if (!is.null(module_controler$exp_r6$exp_3)) {
          output$exp_3_data = shiny::renderUI({
            shiny::fluidRow(
              shiny::checkboxInput(
                inputId = ns('select_exp_3'),
                label = NULL
              ),
              shiny::p(paste0(' Name: ', module_controler$exp_r6$exp_3$name)),
              shiny::p(paste0(' Type: ', module_controler$exp_r6$exp_3$type)),
              shiny::p(paste0(' Samples: ', as.character(nrow(module_controler$exp_r6$exp_3$tables$raw_data))))
            )
          })
        }

        if (!is.null(module_controler$exp_r6$exp_4)) {
          output$exp_4_data = shiny::renderUI({
            shiny::fluidRow(
              shiny::checkboxInput(
                inputId = ns('select_exp_4'),
                label = NULL
              ),
              shiny::p(paste0(' Name: ', module_controler$exp_r6$exp_4$name)),
              shiny::p(paste0(' Type: ', module_controler$exp_r6$exp_4$type)),
              shiny::p(paste0(' Samples: ', as.character(nrow(module_controler$exp_r6$exp_4$tables$raw_data))))
            )
          })
        }

        if (!is.null(module_controler$exp_r6$exp_5)) {
          output$exp_5_data = shiny::renderUI({
            shiny::fluidRow(
              shiny::checkboxInput(
                inputId = ns('select_exp_5'),
                label = NULL
              ),
              shiny::p(paste0(' Name: ', module_controler$exp_r6$exp_5$name)),
              shiny::p(paste0(' Type: ', module_controler$exp_r6$exp_5$type)),
              shiny::p(paste0(' Samples: ', as.character(nrow(module_controler$exp_r6$exp_5$tables$raw_data))))
            )
          })
        }

        if (!is.null(module_controler$exp_r6$exp_6)) {
          output$exp_6_data = shiny::renderUI({
            shiny::fluidRow(
              shiny::checkboxInput(
                inputId = ns('select_exp_6'),
                label = NULL
              ),
              shiny::p(paste0(' Name: ', module_controler$exp_r6$exp_6$name)),
              shiny::p(paste0(' Type: ', module_controler$exp_r6$exp_6$type)),
              shiny::p(paste0(' Samples: ', as.character(nrow(module_controler$exp_r6$exp_6$tables$raw_data))))
            )
          })
        }

      })


      # Start SNF
      session$userData[[id]]$run_snf = shiny::observeEvent(input$run_snf, {

        # Disable button while running
        shinyjs::disable("run_snf")

        r6$tables$metadata = NULL
        r6$tables$omics_tables = list()

        if (!is.null(module_controler$exp_r6$exp_1)) {
          if (input$select_exp_1) {
            print('selected exp 1')
            r6$add_data(name = module_controler$exp_r6$exp_1$name,
                        data_table = module_controler$exp_r6$exp_1$tables$z_scored_data)
            if (is.null(r6$tables$metadata)) {
              r6$add_meta(module_controler$exp_r6$exp_1$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_2)) {
          if (input$select_exp_2) {
            print('selected exp 2')
            r6$add_data(name = module_controler$exp_r6$exp_2$name,
                        data_table = module_controler$exp_r6$exp_2$tables$z_scored_data)
            if (is.null(r6$tables$metadata)) {
              r6$add_meta(module_controler$exp_r6$exp_2$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_3)) {
          if (input$select_exp_3) {
            print('selected exp 3')
            r6$add_data(name = module_controler$exp_r6$exp_3$name,
                        data_table = module_controler$exp_r6$exp_3$tables$z_scored_data)
            if (is.null(r6$tables$metadata)) {
              r6$add_meta(module_controler$exp_r6$exp_3$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_4)) {
          if (input$select_exp_4) {
            print('selected exp 4')
            r6$add_data(name = module_controler$exp_r6$exp_4$name,
                        data_table = module_controler$exp_r6$exp_4$tables$z_scored_data)
            if (is.null(r6$tables$metadata)) {
              r6$add_meta(module_controler$exp_r6$exp_4$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_5)) {
          if (input$select_exp_5) {
            print('selected exp 5')
            r6$add_data(name = module_controler$exp_r6$exp_5$name,
                        data_table = module_controler$exp_r6$exp_5$tables$z_scored_data)
            if (is.null(r6$tables$metadata)) {
              r6$add_meta(module_controler$exp_r6$exp_5$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_6)) {
          if (input$select_exp_6) {
            print('selected exp 6')
            r6$add_data(name = module_controler$exp_r6$exp_6$name,
                        data_table = module_controler$exp_r6$exp_6$tables$z_scored_data)
            if (is.null(r6$tables$metadata)) {
              r6$add_meta(module_controler$exp_r6$exp_6$tables$raw_meta)
            }
          }
        }

        print(names(r6$tables$omics_tables))

        print_t("SNF: ready.")

        # Enable button
        shinyjs::enable("run_snf")
      })

      color_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = 'Spectral'))(40)
      # # Plotting events
      clusters_heatmap_1_events(r6, dimensions_obj, color_palette, input, output, session)
      clusters_heatmap_2_events(r6, dimensions_obj, color_palette, input, output, session)
      similarity_network_1_events(r6, dimensions_obj, color_palette, input, output, session)
      similarity_network_2_events(r6, dimensions_obj, color_palette, input, output, session)
      fusion_heatmap_events(r6, dimensions_obj, color_palette, input, output, session)
      similarity_network_fusion_events(r6, dimensions_obj, color_palette, input, output, session)


      # #----------------------------------------------------------- Plotting ----

      # Initialise dimensions object
      dimensions_obj = shiny::reactiveValues()
      # shiny::observe({
      #   dimensions_obj = shiny::reactiveValues(
      #     x_box = module_controler$dims$x_box,
      #     y_box = module_controler$dims$y_box,
      #     x_plot = module_controler$dims$x_plot,
      #     y_plot = module_controler$dims$y_plot,
      #     x_plot_full = module_controler$dims$x_plot_full,
      #     y_plot_full = module_controler$dims$y_plot_full,
      #     xpx_total = shinybrowser::get_width(),
      #     ypx_total = shinybrowser::get_height(),
      #     xbs = 12,
      #     xpx = shinybrowser::get_width(),
      #     ypx = shinybrowser::get_height()
      #   )
      # })

      # Plot selection
      session$userData[[id]]$show_plots_snf = shiny::observeEvent(input$show_plots_snf, {

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

        # Update x dimensions in px and bs, and y in px
        if (length(input$show_plots_snf) < 2) {
          dimensions_obj$xbs = 12
          dimensions_obj$xpx = shinybrowser::get_width()
          dimensions_obj$ypx = shinybrowser::get_height()
        } else if (length(input$show_plots_snf) == 2) {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()
        } else {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()/2.2
        }

        # Plots selected: 1 to 4
        print_t(paste0("SNF plot selection: ", paste(input$show_plots_snf, collapse = ", ")))
        if (length(input$show_plots_snf) == 1) {
          snf_plot_one(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$show_plots_snf,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$show_plots_snf) == 2) {
          snf_plot_two(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$show_plots_snf,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$show_plots_snf) == 3) {
          snf_plot_three(r6 = r6,
                          dimensions_obj = dimensions_obj,
                          selection_list = input$show_plots_snf,
                          input = input,
                          output = output,
                          session = session)

        } else if (length(input$show_plots_snf) >= 4) {
          snf_plot_four(r6 = r6,
                         dimensions_obj = dimensions_obj,
                         selection_list = input$show_plots_snf,
                         input = input,
                         output = output,
                         session = session)

          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_snf",
            disabledChoices = setdiff(unname(get_snf_plot_list()), input$show_plots_snf)
          )

        }



        if ((length(input$show_plots_snf) > 1) & (length(input$show_plots_snf) < 4)) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_snf",
            disabledChoices = NULL
          )
        } else if (length(input$show_plots_snf) == 1) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_snf",
            disabledChoices = input$show_plots_snf
          )
        }
      })

      session$userData[[id]]$clear_plots = shiny::observeEvent(input$clear_plots, {
        print_t("Clearing plots")
        shinyWidgets::updateCheckboxGroupButtons(
          session = session,
          inputId = "show_plots_snf",
          disabled = FALSE,
          selected = character(0))
        output$plotbox_field = shiny::renderUI(
          NULL
        )
      })


    }
  )
}
