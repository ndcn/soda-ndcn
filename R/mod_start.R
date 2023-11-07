
#----------------------------------------------------------------- Start UI ----
start_ui = function(id){
  ns = shiny::NS(id)
  shiny::tagList(
    shiny::fluidRow(
      shiny::h1('SODA - Simple Omics Data Analysis')
    ),
    shiny::fluidRow(
      shiny::column(
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        width = 6,
        shiny::h3('Start by creating some experiments:'),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shiny::textInput(
              inputId = ns('exp_name'),
              label = 'Exp. name',
              placeholder = 'lips_1',
              width = '100%'
            ),
            shiny::selectInput(
              inputId = ns('exp_type'),
              label = 'Exp. type',
              choices = c('Lipidomics', 'Proteomics', 'Transcriptomics'),
              width = '100%'
            )
          ),
          shiny::column(
            width = 6,
            shiny::selectInput(
              inputId = ns('del_exp'),
              label = 'Delete exp.',
              choices = NULL,
              selected = NULL,
              multiple = TRUE,
              width = '100%'
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 6,
            shinyWidgets::actionBttn(
              inputId = ns('add_exp'),
              label = "Add exp.",
              style = "material-flat",
              color = 'success',
              block = T,
              icon = icon("check")
            )
          ),
          shiny::column(
            width = 6,
            shinyWidgets::actionBttn(
              inputId = ns('remove_exp'),
              label = 'Remove exp.',
              style = "material-flat",
              color = 'danger',
              block = T,
              icon = icon("x")
            )
          )
        ),
        shiny::fluidRow(
          shiny::column(
            width = 12,
            shiny::br(),
            shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
            shiny::h3('... or try out SODA with our example datasets!'),
            shiny::br(),
            shiny::fluidRow(
              shiny::column(
                width = 4,
                shinyWidgets::actionBttn(
                  inputId = ns('add_lipidomics_ex'),
                  label = "Lipidomics",
                  style = "pill",
                  color = 'primary',
                  block = T,
                  icon = icon("upload")
                )
              ),
              shiny::column(
                width = 4,
                shinyWidgets::actionBttn(
                  inputId = ns('add_proteomics_ex'),
                  label = "Proteomics",
                  style = "pill",
                  color = 'primary',
                  block = T,
                  icon = icon("upload")
                )
              ),
              shiny::column(
                width = 4,
                shinyWidgets::actionBttn(
                  inputId = ns('add_transcriptomics_ex'),
                  label = "Transcriptomics",
                  style = "pill",
                  color = 'primary',
                  block = T,
                  icon = icon("upload")
                )
              )
            )
          )
        )
      ),
      shiny::column(
        width = 1
      ),
      shiny::column(
        width = 5,
        shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
        shiny::h3('What is the SODA app?'),
        shiny::p('SODA is designed to analyze and visualize preprocessed omics data.
                 Currently available omic experiments are lipidomics, proteomics
                 and transcriptomics.'),
        shiny::p("Start using SODA by creating an experiment with the options on
                 the left, by defining an experiment name (for you), a it's type
                 and pressing ADD EXP. This will create a new module on the sidebar
                 for you to start importing and analysing your data. Made a mistake
                 creating an experiment? You can select one or multiple experiments
                 in Delete exp. and remove them by pressing REMOVE EXP. Bear in
                 mind, SODA is currently limited to a maximum of 6 simultaneous
                 experiments."),
        shiny::p('The general workflow consists in uploading a metadata and a data
                  table: samples and rows, descriptors/features as columns. The
                  metadata table contains all types of data describing each sample
                  and the data table contains only numeric data.'),
        shiny::p('Samples and features are then be filtered and imputed according
                 to user set parameters.'),
        shiny::p('Finally, data can be analyzed and visualized via interactive
                 plots, and even further analysed with geneset enrichment, over-representation
                 analysis and multiomics integration if the data allows it.')
      )
    )
  )
}

#------------------------------------------------------------- Start server ----

start_server = function(id, main_input, main_output, main_session, module_controler) {
  shiny::moduleServer(
    id,
    function(input, output, session) {

      # Create experiments
      shiny::observeEvent(input$add_exp,{
        exp_name = input$exp_name

        if (exp_name %in% unname(unlist(module_controler$exp_names))) {
          print_t('ERROR: experiment already exists.')
          return()
        }

        if (exp_name == '') {
          exp_name = experiment_switch(input$exp_type)
          counter = 1
          while (paste0(exp_name, '_', counter) %in% unname(unlist(module_controler$exp_names))) {
            counter = counter + 1
          }
          exp_name = paste0(exp_name, '_', counter)
        }

        if (!grepl("^[a-zA-Z0-9_]+$", exp_name)) {
          print_t('ERROR: only alphanumeric and underscores accepted')
          return()
        }

        slot  = names(module_controler$slot_taken)[!sapply(module_controler$slot_taken, base::isTRUE)][1]
        exp_type = input$exp_type
        main_output[[slot]] = bs4Dash::renderMenu({
          bs4Dash::sidebarMenu(
            bs4Dash::menuItem(text = exp_name,
                              tabName = slot,
                              icon = icon(tolower(substr(exp_type, 1, 1))))
          )
        })

        module_controler$slot_taken[[slot]] = TRUE
        module_controler$exp_names[[slot]] = exp_name
        module_controler$exp_types[[slot]] = exp_type
        module_controler$exp_r6[[slot]] = r6_switch(exp_type = exp_type, name = exp_name, id = paste0('mod_', slot) ,slot = slot)

        if (sum(sapply(module_controler$slot_taken, base::isTRUE)) >= 6) {
          shinyjs::disable("add_exp")
        }

        print_t(paste0('Added ', input$exp_name, ' (', exp_type, ')'))

        created_modules = unname(unlist(module_controler$exp_names))
        created_modules = created_modules[!is.na(created_modules)]
        shiny::updateSelectInput(
          inputId = 'del_exp',
          choices = created_modules
        )

        shiny::updateTextInput(
          inputId = 'exp_name',
          value = character(0)
        )

      })

      # shiny::observeEvent(input$remove_exp, {
      #   shiny::req(input$del_exp)
      #
      #   for (mod in input$del_exp) {
      #     print_t(paste0('Removing ', mod))
      #     exp_id = names(which(module_controler$exp_names == mod))[1]
      #     purge_module_inputs(id = exp_id, input_object = main_input)
      #     events = names(session$userData[[paste0('mod_', exp_id)]])
      #     for (e in events) {
      #       session$userData[[paste0('mod_', exp_id)]][[e]]$destroy()
      #     }
      #     main_output[[exp_id]] = NULL
      #     module_controler$slot_taken[[exp_id]] = FALSE
      #     module_controler$module_loaded[[exp_id]] = FALSE
      #     module_controler$exp_types[[exp_id]] = NA
      #     module_controler$exp_names[[exp_id]] = NA
      #     module_controler$exp_r6[[exp_id]] = NA
      #   }
      #
      #   shiny::updateSelectInput(
      #     inputId = 'del_exp',
      #     selected = character(0),
      #     choices = unname(unlist(module_controler$exp_names))
      #   )
      #
      #   shinyjs::enable('add_exp')
      # })

      # Switch experiment

      shiny::observeEvent(input$exp_type,{
        if (input$exp_type == 'Proteomics') {
          shiny::updateTextInput(
            inputId = 'exp_name',
            value = character(0),
            placeholder = 'prot_1'
          )
        } else if (input$exp_type == 'Transcriptomics') {
          shiny::updateTextInput(
            inputId = 'exp_name',
            value = character(0),
            placeholder = 'trns_1'
          )
        } else if (input$exp_type == 'Lipidomics') {
          shiny::updateTextInput(
            inputId = 'exp_name',
            value = character(0),
            placeholder = 'lips_1'
          )
        }

      })

    }
  )
}

