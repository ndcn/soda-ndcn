library(shiny)
library(bs4Dash)
library(shinyWidgets)
#----------------------------------------- Lipidomics data visualisation UI ----
soda_visualise_lips_ui = function(id) {
  ns = NS(id)
  shiny::tagList(
    shiny::fluidRow(
      # Select plots to display
      shinyWidgets::checkboxGroupButtons(
        inputId = ns("show_plots"),
        label = "Show plots:",
        status = "primary",
        choices = c("Class distribution" = "class_distribution",
                    "Class comparison" = "class_comparison"),
        selected = NULL
      )
    ),
    shiny::fluidRow(
      shiny::span("truffles"),
      shiny::plotOutput(
        outputId = ns("plot_id_1")
      ),
      shiny::br(),
      bs4Dash::box(
        id = ns("class_comparison_box"),
        title = "Class comparison",
        solidHeader = TRUE,
        maximizable = TRUE,
        collapsible = FALSE,
        status = "primary"
      )
      # shiny::uiOutput(outputId = ns("uiPlots"))
    )
  )
}

#------------------------------------- Lipidomics data visualisation server ----
soda_visualise_lips_server = function(id, r6 = NULL) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      
      output$plot_id_1 = shiny::renderPlot({
        plot(1:10,
             1:10)
      })
      
      # Render box
      
      # output$uiPlots = renderUI({
      #   shiny::tagList(
      #     shiny::column(
      #       width = 12,
      #       shiny::fluidRow(
      #         if ("class_distribution" %in% input$show_plots) {
      #           bs4Dash::box(
      #             id = "class_distribution_box",
      #             title = "Class distribution",
      #             width = 6,
      #             solidHeader = TRUE,
      #             maximizable = TRUE,
      #             collapsible = FALSE,
      #             status = "primary",
      #             # plotlyOutput(outputId = "plotlyClassDistribution",
      #             #              height = boxDimension$height),
      #             # sidebar = bs4Dash::boxSidebar(
      #             #   id = "classDistributionSidebar",
      #             #   width = 40,
      #             #   uiOutput(outputId = "uiComparisonClassDistribution")
      #             # )
      #           )
      #         }else{
      #           NULL
      #         },
      #         if ("class_comparison" %in% input$show_plots) {
      #           bs4Dash::box(
      #             id = "class_comparison_box",
      #             title = "Class comparison",
      #             width = 6,
      #             solidHeader = TRUE,
      #             maximizable = TRUE,
      #             collapsible = FALSE,
      #             status = "primary",
      #             # plotlyOutput(outputId = "plotlyClassDistribution",
      #             #              height = boxDimension$height),
      #             # sidebar = bs4Dash::boxSidebar(
      #             #   id = "classDistributionSidebar",
      #             #   width = 40,
      #             #   uiOutput(outputId = "uiComparisonClassDistribution")
      #             # )
      #           )
      #         }else{
      #           NULL
      #         }
      #       )
      #     )
      #   )
      # })
    }
  )
}
