#----------------------------------------------------------- MOFA utilities ----

mofa_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_explained_variance" = explained_variance_ui,
                                          "select_factor_plot" = factor_plot_ui,
                                          "select_combined_factors" = combined_factors_ui,
                                          "select_feature_weights" = feature_weights_ui,
                                          "select_feature_top_weights" = feature_top_weights_ui,
                                          "select_mofa_heatmap" = mofa_heatmap_ui,
                                          "select_scatterplot" = scatterplot_ui)
    )
  }
  return(ui_functions)
}

mofa_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_explained_variance" = explained_variance_server,
                                                  "select_factor_plot" = factor_plot_server,
                                                  "select_combined_factors" = combined_factors_server,
                                                  "select_feature_weights" = feature_weights_server,
                                                  "select_feature_top_weights" = feature_top_weights_server,
                                                  "select_mofa_heatmap" = mofa_heatmap_server,
                                                  "select_scatterplot" = scatterplot_server)
    )
  }
  return(server_functions)
}

mofa_plot_one = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)

  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })

  plot_servers = mofa_plotbox_switch_server(selection_list = input$show_plots_mofa)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


mofa_plot_two = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })

  plot_servers = mofa_plotbox_switch_server(selection_list = input$show_plots_mofa)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

mofa_plot_three = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })

  plot_servers = mofa_plotbox_switch_server(selection_list = input$show_plots_mofa)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

mofa_plot_four = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = mofa_plotbox_switch_ui(selection_list = selection_list)
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

  plot_servers = mofa_plotbox_switch_server(selection_list = input$show_plots_mofa)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}










#------------------------------------------------------------------ MOFA UI ----
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
          shinyWidgets::checkboxGroupButtons(inputId = ns("show_plots_mofa"),
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


#-------------------------------------------------------------- MOFA server ----
mofa_server = function(id, r6, module_controler) {
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

      # Start MOFA
      session$userData[[id]]$run_mofa = shiny::observeEvent(input$run_mofa, {

        # Disable button while running
        shinyjs::disable("run_mofa")

        r6$tables$metadata = NULL
        r6$tables$omics_tables = list()

        if (!is.null(module_controler$exp_r6$exp_1)) {
          if (input$select_exp_1) {
            print('selected exp 1')
            r6$tables$omics_tables[[module_controler$exp_r6$exp_1$name]] = t(module_controler$exp_r6$exp_1$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_1$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_2)) {
          if (input$select_exp_2) {
            print('selected exp 2')
            r6$tables$omics_tables[[module_controler$exp_r6$exp_2$name]] = t(module_controler$exp_r6$exp_2$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_2$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_3)) {
          if (input$select_exp_3) {
            print('selected exp 3')
            r6$tables$omics_tables[[module_controler$exp_r6$exp_3$name]] = t(module_controler$exp_r6$exp_3$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_3$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_4)) {
          if (input$select_exp_4) {
            print('selected exp 4')
            r6$tables$omics_tables[[module_controler$exp_r6$exp_4$name]] = t(module_controler$exp_r6$exp_4$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_4$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_5)) {
          if (input$select_exp_5) {
            print('selected exp 5')
            r6$tables$omics_tables[[module_controler$exp_r6$exp_5$name]] = t(module_controler$exp_r6$exp_5$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_5$tables$raw_meta)
            }
          }
        }

        if (!is.null(module_controler$exp_r6$exp_6)) {
          if (input$select_exp_6) {
            print('selected exp 6')
            r6$tables$omics_tables[[module_controler$exp_r6$exp_6$name]] = t(module_controler$exp_r6$exp_6$tables$raw_data)
            if (is.null(r6$tables$metadata)) {
              r6$tables$metadata = t(module_controler$exp_r6$exp_6$tables$raw_meta)
            }
          }
        }

        print(names(r6$tables$omics_tables))

        r6$create_mofa_object()

        print_t("MOFA: training model...")
        r6$prepare_mofa(scale_views = input$data_scale_views,
                        scale_groups = input$data_scale_groups,
                        center_groups = input$data_center_groups,
                        likelihoods = input$model_likelihoods,
                        num_factors = as.numeric(input$model_num_factors),
                        spikeslab_factors = input$model_spikeslab_factors,
                        spikeslab_weights = input$model_spikeslab_weights,
                        ard_factors = input$model_ard_factors,
                        ard_weights = input$model_ard_weights,
                        maxiter = as.numeric(input$training_iterations),
                        convergence_mode = input$training_convergence_mode,
                        startELBO = as.numeric(input$training_start_elbo),
                        freqELBO = as.numeric(input$training_freq_elbo),
                        stochastic = input$training_stochastic,
                        weight_views = input$training_weight_views)
        r6$train_model(mofa_object = r6$mofa_objects$pretrained,
                       outfile = base::file.path("./models", timestamped_name("model.hdf5")),
                       save_data = T)
        r6$add_metadata_to_mofa()




        print_t("MOFA: model ready.")

        # Enable button
        shinyjs::enable("run_mofa")
      })

      # #----------------------------------------------------------- Plotting ----

      # Initialise dimensions object
      dimensions_obj = shiny::reactiveValues()
      shiny::observe({
        dimensions_obj$x_box = module_controler$dims$x_box
        dimensions_obj$y_box = module_controler$dims$y_box
        dimensions_obj$x_plot = module_controler$dims$x_plot
        dimensions_obj$y_plot = module_controler$dims$y_plot
        dimensions_obj$x_plot_full = module_controler$dims$x_plot_full
        dimensions_obj$y_plot_full = module_controler$dims$y_plot_full
        dimensions_obj$xpx_total = shinybrowser::get_width()
        dimensions_obj$ypx_total = shinybrowser::get_height()
        dimensions_obj$xbs = 12
        dimensions_obj$xpx = shinybrowser::get_width()
        dimensions_obj$ypx = shinybrowser::get_height()
      })

      color_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = 'Spectral'))(40)
      # # Plotting events
      explained_variance_events(r6, dimensions_obj, color_palette, input, output, session)
      factor_plot_events(r6, dimensions_obj, color_palette, input, output, session)
      combined_factors_events(r6, dimensions_obj, color_palette, input, output, session)
      feature_weights_events(r6, dimensions_obj, color_palette, input, output, session)
      feature_top_weights_events(r6, dimensions_obj, color_palette, input, output, session)
      mofa_heatmap_events(r6, dimensions_obj, color_palette, input, output, session)
      scatterplot_events(r6, dimensions_obj, r6_settings, input, output, session)


      # Plot selection
      session$userData[[id]]$show_plots_mofa = shiny::observeEvent(input$show_plots_mofa, {

        # Update x dimensions in px and bs, and y in px
        if (length(input$show_plots_mofa) < 2) {
          dimensions_obj$xbs = 12
          dimensions_obj$xpx = shinybrowser::get_width()
          dimensions_obj$ypx = shinybrowser::get_height()
        } else if (length(input$show_plots_mofa) == 2) {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()
        } else {
          dimensions_obj$xbs  = 6
          dimensions_obj$xpx = shinybrowser::get_width()/2
          dimensions_obj$ypx = shinybrowser::get_height()/2.2
        }

        # Plots selected: 1 to 4
        print_t(paste0("MOFA plot selection: ", paste(input$show_plots_mofa, collapse = ", ")))
        if (length(input$show_plots_mofa) == 1) {
          mofa_plot_one(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$show_plots_mofa,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$show_plots_mofa) == 2) {
          mofa_plot_two(r6 = r6,
                        dimensions_obj = dimensions_obj,
                        selection_list = input$show_plots_mofa,
                        input = input,
                        output = output,
                        session = session)

        } else if (length(input$show_plots_mofa) == 3) {
          mofa_plot_three(r6 = r6,
                          dimensions_obj = dimensions_obj,
                          selection_list = input$show_plots_mofa,
                          input = input,
                          output = output,
                          session = session)

        } else if (length(input$show_plots_mofa) >= 4) {
          mofa_plot_four(r6 = r6,
                         dimensions_obj = dimensions_obj,
                         selection_list = input$show_plots_mofa,
                         input = input,
                         output = output,
                         session = session)

          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_mofa",
            disabledChoices = setdiff(unname(get_mofa_plot_list()), input$show_plots_mofa)
          )

        }



        if ((length(input$show_plots_mofa) > 1) & (length(input$show_plots_mofa) < 4)) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_mofa",
            disabledChoices = NULL
          )
        } else if (length(input$show_plots_mofa) == 1) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "show_plots_mofa",
            disabledChoices = input$show_plots_mofa
          )
        }
      })

      session$userData[[id]]$clear_plots = shiny::observeEvent(input$clear_plots, {
        print_t("Clearing plots")
        shinyWidgets::updateCheckboxGroupButtons(
          session = session,
          inputId = "show_plots_mofa",
          disabled = FALSE,
          selected = character(0))
        output$plotbox_field = shiny::renderUI(
          NULL
        )
      })

    }
  )
}


