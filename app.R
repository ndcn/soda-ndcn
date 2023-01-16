# UI
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(bs4Dash)

# OOP
library(R6)

# Data
library(DT)
library(markdown)

# Plotting
library(ggplot2)
library(plotly)
library(gridExtra)

# colouring
library(grDevices)
library(RColorBrewer)

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
      
      # Welcome menu
      bs4Dash::menuItem(
        text = "Welcome",
        tabName = "welcome",
        icon = shiny::icon("home")),
      
      # Data upload and submenus
      bs4Dash::menuItem(
        text = "Data upload",
        tabName = "data_upload",
        icon = shiny::icon("upload"),
        
        bs4Dash::menuSubItem(
          text = "Metadata",
          tabName = "meta_upload"),
        
        bs4Dash::menuSubItem(
          text = "Lipidomics",
          tabName = "lips_upload")),
      
      # Data visualisation and submenus
      bs4Dash::menuItem(
        text = "Visualisation",
        tabName = "global_visual",
        icon = shiny::icon("chart-simple"),
        
        bs4Dash::menuSubItem(
          text = "Lipidomics",
          tabName = "lips_visual")),      
      
      # Help menu and submenus
      bs4Dash::menuItem(
        text = "Help",
        tabName = "help_global",
        icon = shiny::icon("circle-info"),
        
        bs4Dash::menuSubItem(
          text = "Table formats",
          tabName = "help_format"),
      
        bs4Dash::menuSubItem(
          text = "Data upload",
          tabName = "help_upload"))
    )
  )
}


#--------------------------------------------------------------- Setup body ----
body_ui = function() {
  bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      bs4Dash::tabItem(
        tabName = "welcome",
        soda_welcome()
      ),
      bs4Dash::tabItem(
        tabName = "help_format",
        soda_help("data_format")
      ),
      bs4Dash::tabItem(
        tabName = "help_upload",
        soda_help("data_upload")
      ),
      bs4Dash::tabItem(
        tabName = "meta_upload",
        soda_upload_meta_ui(id = "upload_metadata", head = F)
      ),
      bs4Dash::tabItem(
        tabName = "lips_upload",
        soda_upload_lips_ui(id = "upload_lipidomics", head = T)
      ),
      bs4Dash::tabItem(
        tabName = "lips_visual",
        soda_visualise_lips_ui(id = "visualise_lipidomics")
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
  
  lipidomics_data = Omics_data$new(
    name = "lips_1",
    type = "lipidomics"
  )
  

  soda_upload_meta_server("upload_metadata", r6 = lipidomics_data)
  soda_upload_lips_server("upload_lipidomics", r6 = lipidomics_data)
  soda_visualise_lips_server("visualise_lipidomics")

  
}



#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
