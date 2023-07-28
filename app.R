library(shiny)
library(shinyjs)
library(bs4Dash)
library(shinyWidgets)

# New
library(shinythemes) # https://bootswatch.com/quartz/


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
    'exp_1' = NULL,
    'exp_2' = NULL,
    'exp_3' = NULL,
    'exp_4' = NULL,
    'exp_5' = NULL,
    'exp_6' = NULL,
  )
  start_server(id = 'mod_start', main_input = input, main_output = output, main_session = session, module_controler = module_controler)
  about_server(id = 'mod_about', main_output = output)

  # shiny::observeEvent(input[['mod_start-add_exp']],{
  shiny::observe({
    shiny::req(module_controler$exp_1)
    exp_type = module_controler$exp_1
    experiment_server(id = 'mod_exp_1', type = exp_type)
  })

  shiny::observe({
    shiny::req(module_controler$exp_2)
    exp_type = module_controler$exp_2
    experiment_server(id = 'mod_exp_2', type = exp_type)
  })

  shiny::observe({
    shiny::req(module_controler$exp_3)
    exp_type = module_controler$exp_3
    experiment_server(id = 'mod_exp_3', type = exp_type)
  })

  shiny::observe({
    shiny::req(module_controler$exp_4)
    exp_type = module_controler$exp_4
    experiment_server(id = 'mod_exp_4', type = exp_type)
  })

  shiny::observe({
    shiny::req(module_controler$exp_5)
    exp_type = module_controler$exp_5
    experiment_server(id = 'mod_exp_5', type = exp_type)
  })

  shiny::observe({
    shiny::req(module_controler$exp_6)
    exp_type = module_controler$exp_6
    experiment_server(id = 'mod_exp_6', type = exp_type)
  })




}

#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
