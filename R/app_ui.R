#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import datamods
#' @import shinyWidgets
#' @import shinyjqui
#' @import bs4Dash
#' @importFrom utils packageVersion
#'
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    dashboardPage(
      #### header ####
      dashboardHeader(title = paste0("SODA | v", packageVersion(pkg = "soda"))),

      #### sidebar menu ####
      bs4Dash::dashboardSidebar(
        skin = "light",
        sidebarMenu(
          menuItem(text = "Welcome",
                   tabName = "intro",
                   icon = icon("home")),
          menuItem(text = "File",
                   tabName = "file",
                   icon = icon("file-excel")),
          menuItem(text = "Visualisation",
                   tabName = "visualisation",
                   icon = icon("bar-chart-o"))
        )
      ),

      #### body ####
      dashboardBody(
        # determine the size of the browser window
        tags$head(tags$script('
            var dimension = [0, 0];
            $(document).on("shiny:connected", function(e) {
              dimension[0] = window.innerWidth;
              dimension[1] = window.innerHeight;
              Shiny.onInputChange("dimension", dimension);
            });
            $(window).resize(function(e) {
              dimension[0] = window.innerWidth;
              dimension[1] = window.innerHeight;
              Shiny.onInputChange("dimension", dimension);
            });
            ')),

        tabItems(
          tabItem(
            tabName = "intro",
            includeMarkdown(app_sys("app/www/welcome.Rmd"))
          ), # end tabItem introduction
          tabItem(
            tabName = "file",
            fluidRow(
              column(
                width = 8,
                bs4Dash::tabsetPanel(
                  id = "tabset_data",
                  # type = "pills",
                  tabPanel(
                    title = "Load data",
                    import_file_ui(
                      id = "lipid_data_import",
                      title = TRUE,
                      preview_data = TRUE,
                      file_extensions = ".xlsx"
                    )
                  ),
                  tabPanel(
                    title = "Update data",
                    uiOutput(outputId = "uiUpdateLipidData")
                  )
                )
              ),
              column(
                width = 4,
                wellPanel(
                  checkboxInput(inputId = "scale",
                                label = "Scale and center",
                                value = TRUE,
                  ),
                  textInput(
                    inputId = "blank_thr",
                    label = "Nr of blanks threshold",
                    value = "2",
                    width = NULL,
                    placeholder = NULL
                  ),
                  textInput(
                    inputId = "blank_thr_nr_of_samples",
                    label = "Proportion of samples above blank threshold",
                    value = "0.8",
                    width = NULL,
                    placeholder = NULL
                  ),
                  textInput(
                    inputId = "group_thr_nr_of_samples",
                    label = "Proportion of samples from one group above blank threshold",
                    value = "0.7",
                    width = NULL,
                    placeholder = NULL
                  ),
                  # checkboxInput("rem_zeros", "Remove zero dominated measurements"),
                  # checkboxInput("cluster", "Apply clustering"),
                  # uiOutput("comp1"),
                ), # end wellPanel
                wellPanel(
                  selectInput(inputId = "norm",
                              label = "Normalization",
                              choices = c("Normalized to total class" = "class_norm",
                                          "Normalized to total lipid" = "lipid_norm"),
                              selected = "lipid_norm",
                              multiple = FALSE)
                ),
                wellPanel(
                  uiOutput(outputId = "info"),
                  uiOutput(outputId = "comp2"),
                  checkboxInput(inputId = "analysis1",
                                label = "Apply disc. and red. analysis"),
                  sliderInput(inputId = "alpha",
                              label = "Alpha:",
                              ticks = TRUE,
                              min = 0.5,
                              max = 0.95,
                              value = 0.8,
                              step = 0.01),
                )
              ),
              verbatimTextOutput("dimension_display") # only for debugging
            ) # end fluidRow
          ), # end tabItem file
          tabItem(
            tabName = "visualisation",
            fluidRow(
              column(
                width = 4,
                checkboxGroupButtons(inputId = "showPlots",
                                     label = "Show plots:",
                                     status = "primary",
                                     choices = c("Class distribution" = "plotlyClassDistribution",
                                                 "Class comparison" = "plotlyClassComparison",
                                                 "Volcano plot" = "volcano",
                                                 "Heatmap" = "heatmap"),
                                     selected = c("plotlyClassDistribution",
                                                  "plotlyClassComparison"))
              )
            ), # end fluidrow
            uiOutput(outputId = "uiPlots")
          )
        ) # end tabItems
      ) # end body
    ) # end page

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#'
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "soda"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
