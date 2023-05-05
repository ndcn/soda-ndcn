#------------------------------------------------------ General settings UI ----
soda_genset_ui = function(id) {
  
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    shiny::tabPanel(
      ##################################################### Upload metadata ####
      title = "Color palette",
      shiny::fluidRow(
        
        # First column with the table input and preview of the raw data
        shiny::column(
          width = 9,
          shiny::h2("Preview"),
          shiny::plotOutput(ns("palette_preview"),
                            height = "800px")
        ),
        shiny::column(
          width = 3,
          shiny::h2("Palette selection"),
          shiny::selectizeInput(inputId = ns("palette_name"),
                                label = "Name",
                                choices = NULL,
                                selected = NULL,
                                multiple = F),
          shiny::sliderInput(inputId = ns("palette_ramp"),
                             label = "Color ramp",
                             min = 20,
                             max = 60,
                             step = 1,
                             value = 40,
                             width = "100%"),
          shiny::checkboxInput(inputId = ns("reverse_order"),
                               label = "Reverse order",
                               value = FALSE,
                               width = "100%")
        )
      )
    )
  )
}

#-------------------------------------------------- General settings server ----
soda_genset_server = function(id, r6) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      # Update available palettes using the ones in the R6 object
      shiny::updateSelectizeInput(
        session = session,
        inputId = "palette_name",
        choices = names(r6$color_list),
        selected = r6$color_settings$name
      )
      
      # Preview the palette
      shiny::observeEvent(c(input$palette_name, input$palette_ramp, input$reverse_order), {
        req(input$palette_name)
        r6$set_color_palette(name = input$palette_name,
                             n = r6$color_list[[input$palette_name]],
                             ramp = as.numeric(input$palette_ramp),
                             reverse_order = input$reverse_order)
        output$palette_preview = shiny::renderPlot({
          r6$color_settings$color_preview
        }, bg="transparent")  
      })
    }
  )
}