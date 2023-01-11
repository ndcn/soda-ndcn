# library(shiny)
# 
# soda_get_col_ui = function(id, label = "Column selection", desc = "Description"){
#   ns = NS(id)
#   shiny::tagList(
#     shiny::strong(label),
#     shiny::br(),
#     shiny::helpText(desc),
#     shiny::selectInput(inputId = ns("select_col"), choices = NULL, label = NULL, multiple = F)
#   )
# }
# 
# soda_get_col_server = function(id, col_list) {
#   shiny::moduleServer(
#     id,
#     function(input, output, session) {
#       observe({
        # shiny::updateSelectInput(
        #   session = session,
        #   inputId = "select_col",
        #   choices = col_list
        # )
#       })
#     }
#   )
# }