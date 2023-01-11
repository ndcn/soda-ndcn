# library(shiny)
# 
# soda_txt_input_ui = function(id, label = "Free text", default = "text", desc = "Description"){
#   ns = NS(id)
#   shiny::tagList(
#     shiny::strong(label),
#     shiny::br(),
#     shiny::helpText(desc),
#     shiny::textInput(inputId = ns("select_col"), label = NULL, value = default)
#   )
# }
