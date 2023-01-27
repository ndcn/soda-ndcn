# Global UI functions
soda_get_col_ui = function(label = "Column selection", desc = "Description"){
  shiny::tagList(
    shiny::strong(label),
    shiny::br(),
    shiny::helpText(desc)
  )
}