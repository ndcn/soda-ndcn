# UI
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinybrowser)
library(bs4Dash)

# Authentification
library(shinymanager)

# OOP
library(R6)

# Data
library(DT)
library(markdown)
library(dplyr)

# Plotting
library(ggplot2)
library(plotly)
library(gridExtra)
library(heatmaply)

# colouring
library(grDevices)
library(RColorBrewer)

# Statistics
library(pcaMethods)

# Text
library(stringr)

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
      
      # Table merge menu
      bs4Dash::menuItem(
        text = "Merge & download",
        tabName = "merge_tables",
        icon = shiny::icon("download")
      ),
      
      # Help menu and submenus
      bs4Dash::menuItem(
        text = "Help",
        tabName = "help_global",
        icon = shiny::icon("circle-info"),
        
        bs4Dash::menuSubItem(
          text = "Data upload",
          tabName = "help_data_upload"),
        
        bs4Dash::menuSubItem(
          text = "Data visualisation",
          tabName = "help_visualisation"),
        
        bs4Dash::menuSubItem(
          text = "Tables",
          tabName = "help_data_tables")
      )
    )
  )
}


#--------------------------------------------------------------- Setup body ----
body_ui = function() {
  bs4Dash::dashboardBody(
    bs4Dash::tabItems(
      
      # Welcome page
      bs4Dash::tabItem(
        tabName = "welcome",
        soda_welcome()
      ),
      
      # Data upload pages
      bs4Dash::tabItem(
        tabName = "meta_upload",
        soda_upload_meta_ui(id = "upload_metadata", head = F)
      ),
      bs4Dash::tabItem(
        tabName = "lips_upload",
        soda_upload_lips_ui(id = "upload_lipidomics", head = T)
      ),
      
      # Data visualisation pages
      bs4Dash::tabItem(
        tabName = "lips_visual",
        soda_visualise_lips_ui(id = "visualise_lipidomics")
      ),
      
      # Table merge page
      bs4Dash::tabItem(
        tabName = "merge_tables",
        soda_merge_tables_ui(id = "merge_tables_page")
      ),
      
      # Help pages
      bs4Dash::tabItem(
        tabName = "help_data_upload",
        soda_help_data_upload()
      ),
      bs4Dash::tabItem(
        tabName = "help_visualisation",
        soda_help_data_visualisation()
      ),
      bs4Dash::tabItem(
        tabName = "help_data_tables",
        soda_help_tables()
      )

    )
  )
}





#----------------------------------------------------------------------- UI ----
header = header_ui()
sidebar = sidebar_ui()
body = body_ui()
# ui = bs4Dash::dashboardPage(header, sidebar, body)
ui = shinymanager::secure_app(bs4Dash::dashboardPage(header, sidebar, body))
#------------------------------------------------------------------- Server ----

server = function(input, output, session) {
  
  # Basic authentification
  res_auth <- shinymanager::secure_server(
    check_credentials = shinymanager::check_credentials(db = data.frame(
      user = c("user1", "user2"), # mandatory
      password = c("1234", "monkey"), # mandatory
      admin = c(FALSE, FALSE),
      comment = "Secure authentification mechanism for SODA",
      stringsAsFactors = FALSE
    ))
  )

  # Initiate some variables
  options(shiny.maxRequestSize=30*1024^2)
  
  lipidomics_data = Omics_data$new(
    name = "lips_1",
    type = "lipidomics"
  )
  
  colour_list= RColorBrewer::brewer.pal(n = 11, name = 'Spectral')
  colour_list = colour_list[-6]
  colour_list = grDevices::colorRampPalette(colour_list)(60)

  # Load modules
  soda_upload_meta_server("upload_metadata", r6 = lipidomics_data)
  soda_upload_lips_server("upload_lipidomics", r6 = lipidomics_data)
  soda_visualise_lips_server("visualise_lipidomics", r6 = lipidomics_data, colour_list = colour_list)
  soda_merge_tables_server("merge_tables_page", r6 = lipidomics_data)
  
  
}



#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
