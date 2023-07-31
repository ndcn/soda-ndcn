library(shiny)
library(shinyjs)
library(bs4Dash)
library(shinyWidgets)


#------------------------------------------------------------- Setup header ----
header_ui = function() {

  # Get data from the description file
  desc = read.delim("DESCRIPTION", header = FALSE)

  # Extract and capitalise name
  name = stringr::str_split(desc[1,1], ":")[[1]][2]
  name = toupper(trimws(name))

  # Extract version
  version = gsub("[^0-9.-]", "", desc[3,1])
  header = paste(name, "|", version, sep = " ")
  bs4Dash::dashboardHeader(title = header)
}

#------------------------------------------------------------ Setup sidebar ----

sidebar_ui = function() {
  bs4Dash::dashboardSidebar(
    bs4Dash::sidebarMenu(

      # Start menu
      bs4Dash::menuItem(
        text = "Start",
        tabName = "start",
        icon = shiny::icon("list")),

      bs4Dash::sidebarMenuOutput("exp_1"),
      bs4Dash::sidebarMenuOutput("exp_2"),
      bs4Dash::sidebarMenuOutput("exp_3"),
      bs4Dash::sidebarMenuOutput("exp_4"),
      bs4Dash::sidebarMenuOutput("exp_5"),
      bs4Dash::sidebarMenuOutput("exp_6"),

      bs4Dash::menuItem(
        text = "About",
        tabName = "about",
        icon = shiny::icon("question")
      )
    )
  )
}


#--------------------------------------------------------------- Setup body ----
body_ui = function() {
  bs4Dash::dashboardBody(
    shinyjs::useShinyjs(),
    bs4Dash::tabItems(

      # Start page
      bs4Dash::tabItem(
        tabName = "start",
        start_ui(id = 'mod_start')
      ),

      bs4Dash::tabItem(
        tabName = "exp_1",
        experiment_ui(id = 'mod_exp_1')
      ),

      bs4Dash::tabItem(
        tabName = "exp_2",
        experiment_ui(id = 'mod_exp_2')
      ),

      bs4Dash::tabItem(
        tabName = "exp_3",
        experiment_ui(id = 'mod_exp_3')
      ),

      bs4Dash::tabItem(
        tabName = "exp_4",
        experiment_ui(id = 'mod_exp_4')
      ),

      bs4Dash::tabItem(
        tabName = "exp_5",
        experiment_ui(id = 'mod_exp_5')
      ),

      bs4Dash::tabItem(
        tabName = "exp_6",
        experiment_ui(id = 'mod_exp_6')
      ),

      bs4Dash::tabItem(
        tabName = "about",
        about_ui(id = 'mod_about')
      )

    )
  )
}

#----------------------------------------------------------------------- UI ----
header = header_ui()
sidebar = sidebar_ui()
body = body_ui()
ui = bs4Dash::dashboardPage(header, sidebar, body)
#------------------------------------------------------------------- Server ----

server = function(input, output, session) {
  module_controler = shiny::reactiveValues(

    slot_taken = list(
      'exp_1' = FALSE,
      'exp_2' = FALSE,
      'exp_3' = FALSE,
      'exp_4' = FALSE,
      'exp_5' = FALSE,
      'exp_6' = FALSE
    ),

    module_loaded = list(
      'exp_1' = FALSE,
      'exp_2' = FALSE,
      'exp_3' = FALSE,
      'exp_4' = FALSE,
      'exp_5' = FALSE,
      'exp_6' = FALSE
    ),

    exp_types = list(
      'exp_1' = NA,
      'exp_2' = NA,
      'exp_3' = NA,
      'exp_4' = NA,
      'exp_5' = NA,
      'exp_6' = NA
    ),

    exp_names = list(
      'exp_1' = NA,
      'exp_2' = NA,
      'exp_3' = NA,
      'exp_4' = NA,
      'exp_5' = NA,
      'exp_6' = NA
    ),

    exp_r6 = list(
      'exp_1' = NA,
      'exp_2' = NA,
      'exp_3' = NA,
      'exp_4' = NA,
      'exp_5' = NA,
      'exp_6' = NA
    )
  )
  start_server(id = 'mod_start', main_input = input, main_output = output, main_session = session, module_controler = module_controler)
  about_server(id = 'mod_about', main_output = output)

  # shiny::observeEvent(input[['mod_start-add_exp']],{

  shiny::observe({
    set_1 = names(which(module_controler$slot_taken == TRUE))
    set_2 = names(which(module_controler$module_loaded == TRUE))
    slot = base::setdiff(set_1, set_2)
    if (length(slot) > 0) {
      slot = slot[1]
      exp_type = module_controler$exp_types[[slot]]
      experiment_server(id = paste0(c('mod', slot), collapse = '_'), type = exp_type)
      module_controler$module_loaded[[slot]] = TRUE
    }
  })
}

#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
