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

      shiny::observe({
        if (!is.null(module_controler$exp_r6$exp_1)) {
          output$exp_1_data = shiny::renderUI({
            shiny::fluidRow(
              shiny::checkboxInput(
                inputId = ns('select_exp_1'),
                label = NULL
              ),
              shiny::p(module_controler$exp_r6$exp_1$name),
              shiny::p(module_controler$exp_r6$exp_1$type)
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
              shiny::p(module_controler$exp_r6$exp_2$name),
              shiny::p(module_controler$exp_r6$exp_2$type)
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
              shiny::p(module_controler$exp_r6$exp_3$name),
              shiny::p(module_controler$exp_r6$exp_3$type)
            )
          })
        }

      })

      #
      # ## Create a dimensions object to store browser dimensions
      # dimensions_obj = shiny::reactiveValues()
      #
      # # Constant values
      # dimensions_obj$x_box = 0.9
      # dimensions_obj$y_box = 0.8
      # dimensions_obj$x_plot = 0.8
      # dimensions_obj$y_plot = 0.75
      # dimensions_obj$x_plot_full = 0.95
      # dimensions_obj$y_plot_full = 0.91
      #
      # shiny::observe({
      #   dimensions_obj$xpx_total = shinybrowser::get_width()
      #   dimensions_obj$ypx_total = shinybrowser::get_height()
      # })
      #
      #
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
      # # Data upload
      # meta_table_input = reactive({
      #   validate(need(input$file_meta, message = FALSE))
      #   sep = find_delim(path = input$file_meta$datapath)
      #   read.csv(input$file_meta$datapath,
      #            header = T,
      #            sep = sep,
      #            check.names = FALSE)
      # })
      #
      # data_table_input = reactive({
      #   validate(need(input$file_data, message = FALSE))
      #   sep = find_delim(path = input$file_data$datapath)
      #   read.csv(input$file_data$datapath,
      #            header = T,
      #            sep = sep,
      #            check.names = FALSE)
      # })
      #
      # # Update fields and display tables
      # shiny::observe({
      #   shiny::req(meta_table_input())
      #   shiny::updateSelectInput(
      #     session = session,
      #     inputId = "select_meta_id",
      #     choices = colnames(meta_table_input()),
      #     selected = colnames(meta_table_input())[1]
      #   )
      #   output$meta_table = renderDataTable({
      #     DT::datatable(meta_table_input()[1:min(nrow(meta_table_input()), max_rows), 1:min(ncol(meta_table_input()), max_cols)], options = list(paging = FALSE))
      #   })
      # })
      #
      # shiny::observe({
      #   shiny::req(data_table_input())
      #   shiny::updateSelectInput(
      #     session = session,
      #     inputId = "select_data_id",
      #     choices = colnames(data_table_input()),
      #     selected = colnames(data_table_input())[1]
      #   )
      #   output$data_table = renderDataTable({
      #     DT::datatable(data_table_input()[1:min(nrow(data_table_input()), max_rows), 1:min(ncol(data_table_input()), max_cols)], options = list(paging = FALSE))
      #   })
      # })
      #
      # # Add metadata table
      # shiny::observeEvent(input$add_metatable, {
      #   shiny::req(meta_table_input())
      #   r6$add_metadata_table(metadata = meta_table_input(),
      #                         id_col = input$select_meta_id)
      #   print_time('MOFA: Added metadata.')
      # })
      #
      # # Add omics tables
      # shiny::observeEvent(input$add_datatable, {
      #   shiny::req(c(data_table_input(), input$omics_name))
      #   if (!is.null(r6$tables$metadata)) {
      #     r6$add_omics(name = input$omics_name,
      #                  omics_table = data_table_input(),
      #                  id_col = input$select_data_id)
      #
      #     shiny::updateTextInput(
      #       session = session,
      #       inputId = 'select_data_id',
      #       value = character(0)
      #     )
      #
      #     shiny::updateTextInput(
      #       session = session,
      #       inputId = 'omics_name',
      #       value = character(0)
      #     )
      #     output$data_table = renderDataTable({
      #       NULL
      #     })
      #     print_time(paste0('MOFA: Added omics data: ', input$omics_name))
      #   }
      #
      # })
      #
      # # Create the mofa object
      # shiny::observeEvent(input$combine_data, {
      #   print_time("MOFA: experiment loaded.")
      #   r6$create_mofa_object()
      # })
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
      # # Start MOFA+
      # shiny::observeEvent(input$run_mofa, {
      #
      #   # Disable button while running
      #   shinyjs::disable("run_mofa")
      #
      #   print_time("MOFA: training model...")
      #   r6$prepare_mofa(scale_views = input$data_scale_views,
      #                   scale_groups = input$data_scale_groups,
      #                   center_groups = input$data_center_groups,
      #                   likelihoods = input$model_likelihoods,
      #                   num_factors = as.numeric(input$model_num_factors),
      #                   spikeslab_factors = input$model_spikeslab_factors,
      #                   spikeslab_weights = input$model_spikeslab_weights,
      #                   ard_factors = input$model_ard_factors,
      #                   ard_weights = input$model_ard_weights,
      #                   maxiter = as.numeric(input$training_iterations),
      #                   convergence_mode = input$training_convergence_mode,
      #                   startELBO = as.numeric(input$training_start_elbo),
      #                   freqELBO = as.numeric(input$training_freq_elbo),
      #                   stochastic = input$training_stochastic,
      #                   weight_views = input$training_weight_views)
      #   r6$train_model(mofa_object = r6$mofa_objects$pretrained,
      #                  outfile = base::file.path("./models", timestamped_name("model.hdf5")),
      #                  save_data = T)
      #   r6$add_metadata_to_mofa()
      #
      #   print_time("MOFA: model ready.")
      #
      #   # Enable button
      #   shinyjs::enable("run_mofa")
      # })
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


