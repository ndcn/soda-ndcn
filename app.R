library(shiny)
library(markdown)
library(shinydashboard)
library(R6)
library(DT)
library(markdown)
library(plotly)
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
  shinydashboard::dashboardHeader(title = header)
}

#------------------------------------------------------------ Setup sidebar ----

sidebar_ui = function() {
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      
      # Welcome menu
      shinydashboard::menuItem(
        text = "Welcome",
        tabName = "welcome",
        icon = shiny::icon("home")),
      
      # Data upload and submenus
      shinydashboard::menuItem(
        text = "Data upload",
        tabName = "data_upload",
        icon = shiny::icon("upload"),
        
        shinydashboard::menuSubItem(
          text = "Metadata",
          tabName = "meta_upload"),
        
        shinydashboard::menuSubItem(
          text = "Lipidomics",
          tabName = "lips_upload")),
      
      # Data visualisation and submenus
      shinydashboard::menuItem(
        text = "Visualisation",
        tabName = "global_visual",
        icon = shiny::icon("chart-simple"),
        
        shinydashboard::menuSubItem(
          text = "Lipidomics",
          tabName = "lips_visual")),      
      
      # Help menu and submenus
      shinydashboard::menuItem(
        text = "Help",
        tabName = "help_global",
        icon = shiny::icon("circle-info"),
        
        shinydashboard::menuSubItem(
          text = "Table formats",
          tabName = "help_format"),
      
        shinydashboard::menuSubItem(
          text = "Data upload",
          tabName = "help_upload"))
    )
  )
}


#--------------------------------------------------------------- Setup body ----
body_ui = function() {
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      shinydashboard::tabItem(
        tabName = "welcome",
        soda_welcome()
      ),
      shinydashboard::tabItem(
        tabName = "help_format",
        soda_help("data_format")
      ),
      shinydashboard::tabItem(
        tabName = "help_upload",
        soda_help("data_upload")
      ),
      shinydashboard::tabItem(
        tabName = "meta_upload",
        soda_upload_meta_ui(id = "upload_metadata", head = F)
      ),
      shinydashboard::tabItem(
        tabName = "lips_upload",
        soda_upload_lips_ui(id = "upload_lipidomics", head = T)
      )
    )
  )
}





#----------------------------------------------------------------------- UI ----
header = header_ui()
sidebar = sidebar_ui()
body = body_ui()
ui = shinydashboard::dashboardPage(header, sidebar, body)


#------------------------------------------------------------------- Server ----

server = function(input, output, session) {
  
  lipidomics_data = Omics_data$new(
    name = "lips_1",
    type = "lipidomics"
  )
  

  soda_upload_meta_server("upload_metadata", r6 = lipidomics_data)
  soda_upload_lips_server("upload_lipidomics", r6 = lipidomics_data)

  
}



#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
