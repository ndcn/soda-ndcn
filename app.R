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
library(readxl)

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
library(glmnet)

# Text
library(stringr)

# Requests
library(httr)
library(xml2)
library(rjson)

# Omics
library(org.Hs.eg.db)


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
      
      # General settings
      bs4Dash::menuItem(
        text = "General settings",
        tabName = "gen_set",
        icon = shiny::icon("gears")),
      
      # Data upload and submenus
      bs4Dash::menuItem(
        text = "Data upload",
        tabName = "data_upload",
        icon = shiny::icon("upload"),
        
        bs4Dash::menuSubItem(
          text = "Lipidomics",
          tabName = "lips_upload"),
        
        bs4Dash::menuSubItem(
          text = "Proteomics",
          tabName = "prot_upload")
        
        ),
      
      # Data visualisation and submenus
      bs4Dash::menuItem(
        text = "Visualisation",
        tabName = "global_visual",
        icon = shiny::icon("chart-simple"),
        
        bs4Dash::menuSubItem(
          text = "Lipidomics",
          tabName = "lips_visual"),
        bs4Dash::menuSubItem(
          text = "Proteomics",
          tabName = "prot_visual")
        ), 
      
      # Table merge menu
      bs4Dash::menuItem(
        text = "Utilities",
        tabName = "utilities",
        icon = shiny::icon("wrench"),

        bs4Dash::menuSubItem(
          text = "Merge & download",
          tabName = "merge_tables"),
        
        bs4Dash::menuSubItem(
          text = "Convert table",
          tabName = "table_convert")
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
      
      # General settings page
      bs4Dash::tabItem(
        tabName = "gen_set",
        soda_genset_ui(id = "general_settings")
      ),
      
      # Data upload pages
      bs4Dash::tabItem(
        tabName = "lips_upload",
        soda_upload_lips_ui(id = "upload_lipidomics", head_meta = F, head_data = T)
      ),
      
      # Data upload pages
      bs4Dash::tabItem(
        tabName = "prot_upload",
        soda_upload_prot_ui(id = "upload_proteomics", head_meta = F, head_data = T)
      ),
      
      # Data visualisation pages
      bs4Dash::tabItem(
        tabName = "lips_visual",
        soda_visualise_lips_ui(id = "visualise_lipidomics")
      ),
      
      bs4Dash::tabItem(
        tabName = "prot_visual",
        soda_visualise_prot_ui(id = "visualise_proteomics")
      ),
      
      # Table merge page
      bs4Dash::tabItem(
        tabName = "merge_tables",
        utils_merge_tables_ui(id = "merge_tables_page")
      ),
      
      # Table convert page
      bs4Dash::tabItem(
        tabName = "table_convert",
        utils_convert_table_ui(id = "convert_tables_page")
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
  options(shiny.maxRequestSize=300*1024^2)
  
  lipidomics_data = Lips_data$new(
    name = "lips_1",
    type = "lipidomics"
  )
  
  proteomics_data = Prot_data$new(
    name = "prot_1",
    type = "proteomics"
  )

  
  
  
  general_settings = General_settings_class$new()
  soda_genset_server("general_settings", r6 = general_settings)


  # Load modules
  soda_upload_lips_server("upload_lipidomics", r6 = lipidomics_data)
  soda_upload_prot_server("upload_proteomics", r6 = proteomics_data)
  soda_visualise_lips_server("visualise_lipidomics", r6 = lipidomics_data, r6_settings = general_settings)
  soda_visualise_prot_server("visualise_proteomics", r6 = proteomics_data, r6_settings = general_settings)
  utils_merge_tables_server("merge_tables_page")
  utils_convert_table_server("convert_tables_page")
  
  
}



#---------------------------------------------------------------------- End ----
shinyApp(ui, server)
