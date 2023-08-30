# mofa_plot_one = function(r6, dimensions_obj, selection_list, input, output, session) {
#   ns = session$ns
#   ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
#
#   output$plotbox_field = shiny::renderUI({
#     shiny::fluidRow(
#       shiny::tagList(
#         ui_functions[[1]](dimensions_obj, session)
#       )
#     )
#   })
#
#   plot_servers = mofa_plotbox_switch_server(selection_list = input$showPlots)
#   for (server_function in plot_servers) {
#     server_function(r6, output, session)
#   }
# }
#
#
# mofa_plot_two = function(r6, dimensions_obj, selection_list, input, output, session) {
#   ns = session$ns
#   ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
#   output$plotbox_field = shiny::renderUI({
#     shiny::fluidRow(
#       shiny::tagList(
#         ui_functions[[1]](dimensions_obj, session),
#         ui_functions[[2]](dimensions_obj, session)
#       )
#     )
#   })
#
#   plot_servers = mofa_plotbox_switch_server(selection_list = input$showPlots)
#   for (server_function in plot_servers) {
#     server_function(r6, output, session)
#   }
# }
#
# mofa_plot_three = function(r6, dimensions_obj, selection_list, input, output, session) {
#   ns = session$ns
#   ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
#   output$plotbox_field = shiny::renderUI({
#     shiny::fluidRow(
#       shiny::tagList(
#         ui_functions[[1]](dimensions_obj, session),
#         ui_functions[[2]](dimensions_obj, session),
#         ui_functions[[3]](dimensions_obj, session)
#       )
#     )
#   })
#
#   plot_servers = mofa_plotbox_switch_server(selection_list = input$showPlots)
#   for (server_function in plot_servers) {
#     server_function(r6, output, session)
#   }
# }
#
# mofa_plot_four = function(r6, dimensions_obj, selection_list, input, output, session) {
#   ns = session$ns
#   ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
#   output$plotbox_field = shiny::renderUI({
#     shiny::fluidRow(
#       shiny::tagList(
#         ui_functions[[1]](dimensions_obj, session),
#         ui_functions[[2]](dimensions_obj, session),
#         ui_functions[[3]](dimensions_obj, session),
#         ui_functions[[4]](dimensions_obj, session)
#       )
#     )
#   })
#
#   plot_servers = mofa_plotbox_switch_server(selection_list = input$showPlots)
#   for (server_function in plot_servers) {
#     server_function(r6, output, session)
#   }
# }
#
#
#
#
#
#
#



#----------------------------------------------------------------- MOFA+ UI ----
mofa_ui = function(id) {
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
              # shiny::fluidRow(
              #   DTOutput("module_table")
              # ),
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
        shiny::column(
          width = 4,
          shiny::h3("Data options"),

          shiny::fluidRow(
            shiny::column(
              width = 4,
              shinyWidgets::prettySwitch(inputId = ns("data_scale_views"),
                                         label = "Scale views",
                                         value = FALSE,
                                         fill = TRUE, status = "primary")
            ),
            shiny::column(
              width = 4,
              shinyWidgets::prettySwitch(inputId = ns("data_scale_groups"),
                                         label = "Scale groups",
                                         value = FALSE,
                                         fill = TRUE, status = "primary")
            ),
            shiny::column(
              width = 4,
              shinyWidgets::prettySwitch(inputId = ns("data_center_groups"),
                                         label = "Center groups",
                                         value = TRUE,
                                         fill = TRUE, status = "primary")
            )
          ),




        ),
        shiny::column(
          width = 4,
          shiny::h3("Model options"),

          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::selectInput(inputId = ns("model_likelihoods"),
                                 label = "Select likelihood",
                                 choices = c("gaussian", "poisson", "bernoulli"),
                                 selected = "gaussian",
                                 width = "100%"),
              shinyWidgets::prettySwitch(inputId = ns("model_spikeslab_factors"),
                                         label = "Spikelab factors",
                                         value = FALSE,
                                         fill = TRUE, status = "primary"),
              shinyWidgets::prettySwitch(inputId = ns("model_ard_factors"),
                                         label = "ARD factors",
                                         value = FALSE,
                                         fill = TRUE, status = "primary")
            ),
            shiny::column(
              width = 6,
              shiny::textInput(inputId = ns("model_num_factors"),
                               label = "Number of factors",
                               value = 6,
                               width = "100%"),
              shinyWidgets::prettySwitch(inputId = ns("model_spikeslab_weights"),
                                         label = "Spikelab weights",
                                         value = FALSE,
                                         fill = TRUE, status = "primary"),
              shinyWidgets::prettySwitch(inputId = ns("model_ard_weights"),
                                         label = "ARD weights",
                                         value = TRUE,
                                         fill = TRUE, status = "primary")
            )
          )
        ),


        shiny::column(
          width = 4,
          shiny::h3("Training options"),

          shiny::fluidRow(
            shiny::column(
              width = 6,
              shiny::textInput(inputId = ns("training_iterations"),
                               label = "Max iterations",
                               value = 1000,
                               width = "100%"),
              shiny::textInput(inputId = ns("training_start_elbo"),
                               label = "startELBO",
                               value = 1,
                               width = "100%"),
              shinyWidgets::prettySwitch(inputId = ns("training_stochastic"),
                                         label = "Stochastic",
                                         value = FALSE,
                                         fill = TRUE, status = "primary")
            ),
            shiny::column(
              width = 6,
              shiny::selectInput(inputId = ns("training_convergence_mode"),
                                 label = "Convergence mode",
                                 choices = c("fast", "medium", "slow"),
                                 selected = "fast",
                                 width = "100%"),
              shiny::textInput(inputId = ns("training_freq_elbo"),
                               label = "freqELBO",
                               value = 5,
                               width = "100%"),
              shinyWidgets::prettySwitch(inputId = ns("training_weight_views"),
                                         label = "Weight views",
                                         value = FALSE,
                                         fill = TRUE, status = "primary")
            )
          )
        )
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
            inputId = ns("run_mofa"),
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
          shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                             label = NULL,
                                             status = "default",
                                             choices = get_mofa_plot_list(),
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                             size = "normal",
                                             justified = TRUE)
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
  )

}


#-------------------------------------------------------------- MOFA server ----
mofa_server = function(id, r6, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns

      # # Initialise dimensions object
      # dimensions_obj = shiny::reactiveValues(
      #   x_box = module_controler$dims$x_box,
      #   y_box = module_controler$dims$y_box,
      #   x_plot = module_controler$dims$x_plot,
      #   y_plot = module_controler$dims$y_plot,
      #   x_plot_full = module_controler$dims$x_plot_full,
      #   y_plot_full = module_controler$dims$y_plot_full,
      #   xpx_total = shinybrowser::get_width(),
      #   ypx_total = shinybrowser::get_height(),
      #   xbs = 12,
      #   xpx = shinybrowser::get_width(),
      #   ypx = shinybrowser::get_height()
      # )

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

      # Start MOFA
      shiny::observeEvent(input$run_mofa, {

        # Disable button while running
        shinyjs::disable("run_mofa")

        r6$tables$metadata = NULL
        r6$tables$omics_tables = list()

        if (!is.null(module_controler$exp_r6$exp_1)) {
          if (input$select_exp_1) {
            print('selected exp 1')
            r6$tables$omics_tables$exp_1 = t(module_controler$exp_r6$exp_1$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_1$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_2)) {
          if (input$select_exp_2) {
            print('selected exp 2')
            r6$tables$omics_tables$exp_2 = t(module_controler$exp_r6$exp_2$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_2$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_3)) {
          if (input$select_exp_3) {
            print('selected exp 3')
            r6$tables$omics_tables$exp_3 = t(module_controler$exp_r6$exp_3$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_3$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_4)) {
          if (input$select_exp_4) {
            print('selected exp 4')
            r6$tables$omics_tables$exp_4 = t(module_controler$exp_r6$exp_4$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_4$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_5)) {
          if (input$select_exp_5) {
            print('selected exp 5')
            r6$tables$omics_tables$exp_5 = t(module_controler$exp_r6$exp_5$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_5$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_6)) {
          if (input$select_exp_6) {
            print('selected exp 6')
            r6$tables$omics_tables$exp_6 = t(module_controler$exp_r6$exp_6$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_6$tables$raw_meta)
            }
          }
        }

        print(names(r6$tables$omics_tables))

        r6$create_mofa_object()




        # print(names(module_controler$exp_r6))
        # for (exp in names(module_controler$exp_r6)) {
        #   print(exp)
        #   print(is.null(module_controler$exp_r6[[exp]]$raw_data))
        #   if (!is.null(module_controler$exp_r6[[exp]]$raw_data)) {
        #     print(nrow(module_controler$exp_r6[[exp]]$raw_data))
        #   }
        # }


        print_t("MOFA: training model...")
        r6$prepare_mofa(scale_views = F,
                        scale_groups = F,
                        center_groups = T,
                        likelihoods = 'gaussian',
                        num_factors = 6,
                        spikeslab_factors = F,
                        spikeslab_weights = F,
                        ard_factors = F,
                        ard_weights = T,
                        maxiter = 1000,
                        convergence_mode = 'fast',
                        startELBO = 1,
                        freqELBO = 5,
                        stochastic = F,
                        weight_views = F)
        r6$train_model(mofa_object = r6$mofa_objects$pretrained,
                       outfile = base::file.path("./models", timestamped_name("model.hdf5")),
                       save_data = T)
        # r6$add_metadata_to_mofa()




        print_t("MOFA: model ready.")

        # Enable button
        shinyjs::enable("run_mofa")
      })


      # # Reactive values
      # shiny::observeEvent(input$showPlots,{
      #
      #   # x (width) in BS (Bootstrap) values
      #   if (length(input$showPlots) < 2) {
      #     dimensions_obj$xbs = 12
      #   } else {
      #     dimensions_obj$xbs = 6
      #   }
      #
      #   # x (width) in pixels
      #   if (length(input$showPlots) < 2) {
      #     dimensions_obj$xpx = shinybrowser::get_width()
      #   } else {
      #     dimensions_obj$xpx = shinybrowser::get_width()/2
      #   }
      #
      #   # y (height) in pixels
      #   if (length(input$showPlots) < 3) {
      #     dimensions_obj$ypx = shinybrowser::get_height()
      #   } else {
      #     dimensions_obj$ypx = shinybrowser::get_height()/2.1
      #   }
      # })
      #
      #

      # # Reset everything
      # shiny::observeEvent(input$reset_all, {
      #   print_time("MOFA: resetting experiment.")
      #   r6$reset_all()
      # })
      #
      # # Load example data
      # shiny::observeEvent(input$use_artificial_data, {
      #   print_time("MOFA: Loading example data...")
      #   r6$get_example_data()
      # })
      #
      #
      #
      # # Plotting events
      # explained_variance_events(r6, dimensions_obj, r6_settings, input, output, session)
      # factor_plot_events(r6, dimensions_obj, r6_settings, input, output, session)
      # feature_weights_events(r6, dimensions_obj, r6_settings, input, output, session)
      # feature_top_weights_events(r6, dimensions_obj, r6_settings, input, output, session)
      # mofa_heatmap_events(r6, dimensions_obj, r6_settings, input, output, session)
      # scatterplot_events(r6, dimensions_obj, r6_settings, input, output, session)
      #
      #
      #
      #
      #
      #
      #
      # #----------------------------------------------------------- Plotting ----
      # # Plot selection
      # shiny::observeEvent(input$showPlots, {
      #
      #   # Plots selected: 1 to 4
      #   print_time(paste0("Plot selection: ", paste(input$showPlots, collapse = ", ")))
      #   if (length(input$showPlots) == 1) {
      #     mofa_plot_one(r6 = r6,
      #                   dimensions_obj = dimensions_obj,
      #                   selection_list = input$showPlots,
      #                   input = input,
      #                   output = output,
      #                   session = session)
      #
      #   } else if (length(input$showPlots) == 2) {
      #     mofa_plot_two(r6 = r6,
      #                   dimensions_obj = dimensions_obj,
      #                   selection_list = input$showPlots,
      #                   input = input,
      #                   output = output,
      #                   session = session)
      #
      #   } else if (length(input$showPlots) == 3) {
      #     mofa_plot_three(r6 = r6,
      #                     dimensions_obj = dimensions_obj,
      #                     selection_list = input$showPlots,
      #                     input = input,
      #                     output = output,
      #                     session = session)
      #
      #   } else if (length(input$showPlots) >= 4) {
      #     mofa_plot_four(r6 = r6,
      #                    dimensions_obj = dimensions_obj,
      #                    selection_list = input$showPlots,
      #                    input = input,
      #                    output = output,
      #                    session = session)
      #
      #     shinyWidgets::updateCheckboxGroupButtons(
      #       session = session,
      #       inputId = "showPlots",
      #       disabledChoices = setdiff(unname(get_mofa_plot_list()), input$showPlots)
      #     )
      #
      #   }
      #   if (between(length(input$showPlots), 2, 3)) {
      #     shinyWidgets::updateCheckboxGroupButtons(
      #       session = session,
      #       inputId = "showPlots",
      #       disabledChoices = NULL
      #     )
      #   } else if (length(input$showPlots) == 1) {
      #     shinyWidgets::updateCheckboxGroupButtons(
      #       session = session,
      #       inputId = "showPlots",
      #       disabledChoices = input$showPlots
      #     )
      #   }
      # })
      #
      # shiny::observeEvent(input$clear_plots, {
      #   print_time("Clearing plots")
      #   shinyWidgets::updateCheckboxGroupButtons(
      #     session = session,
      #     inputId = "showPlots",
      #     disabled = FALSE,
      #     selected = character(0))
      #   output$plotbox_field = shiny::renderUI(
      #     NULL
      #   )
      # })

    }
  )
}


