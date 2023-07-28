
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

      exp_slots = shiny::reactiveValues(
        exp_6 = FALSE,
        exp_5 = FALSE,
        exp_4 = FALSE,
        exp_3 = FALSE,
        exp_2 = FALSE,
        exp_1 = FALSE
      )

      exp_data = shiny::reactiveValues(
        exp_names_list = NULL,
        exp_names = NULL

      )

      # Create experiments
      shiny::observeEvent(input$add_exp,{
        exp_name = input$exp_name
        exp_type = input$exp_type

        if (exp_name == '') {
          print('ERROR: please enter a name for the experiment')
          return()
        }
        if (exp_name %in% names(exp_data$exp_names)) {
          print('ERROR: experiment already exists.')
          return()
        }


        for (slot in names(exp_slots)) {
          if (!exp_slots[[slot]]) {
            main_output[[slot]] = bs4Dash::renderMenu({
              bs4Dash::sidebarMenu(
                bs4Dash::menuItem(text = exp_name,
                                  tabName = slot,
                                  icon = icon(tolower(substr(exp_type, 1, 1))))
              )
            })
            exp_slots[[slot]] = TRUE
            exp_data$exp_names[[exp_name]] = slot
            module_controler[[slot]] = exp_type
            break
          }
        }

        if (length(exp_data$exp_names) >= 6) {
          shinyjs::disable("add_exp")
        }

        print(paste0('Added ', input$exp_name, ' (', exp_type, ')'))

        exp_data$exp_names_list = c(exp_data$exp_names_list, exp_name)
        shiny::updateSelectInput(
          inputId = 'del_exp',
          choices = names(exp_data$exp_names)
        )
        shiny::updateTextInput(
          inputId = 'exp_name',
          value = character(0)
        )

      })

      shiny::observeEvent(input$remove_exp, {
        shiny::req(input$del_exp)

        for (mod in input$del_exp) {
          print(paste0('Removing ', mod))
          exp_id = exp_data$exp_names[[mod]]
          main_output[[exp_id]] = NULL
          exp_slots[[exp_id]] = FALSE
          module_controler[[exp_id]] = NULL
          purge_module_inputs(id = exp_id, input_object = main_input)
          session$userData[[paste0('mod_', exp_id)]]$test$destroy()
          exp_data$exp_names = exp_data$exp_names[names(exp_data$exp_names) != mod]
        }

        shiny::updateSelectInput(
          inputId = 'del_exp',
          selected = character(0),
          choices = names(exp_data$exp_names)
        )
        shinyjs::enable('add_exp')
      })

    }
  )
}

