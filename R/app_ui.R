#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import datamods
#' @import shinyWidgets
#' @import shinyjqui
#' @import shinymanager
#' @import bs4Dash
#' @importFrom reactable reactableOutput
#' @importFrom utils packageVersion
#'
#' @noRd

app_ui <- function(request) {
  ui <- dashboardPage(

    #### header ####
    dashboardHeader(title = paste0("SODA | v", packageVersion(pkg = "soda"))),

    #### sidebar menu ####
    bs4Dash::dashboardSidebar(
      skin = "dark",
      bs4Dash::sidebarMenu(
        menuItem(text = "Welcome",
                 tabName = "intro",
                 icon = shiny::icon("home")),
        menuItem(text = "Data upload",
                 tabName = "data_upload",
                 icon = shiny::icon("file-excel"),
                 menuSubItem(
                   text = "Metadata",
                   tabName = "sample_metadata_upload"),
                 menuSubItem(
                   text = "Lipidomics",
                   tabName = "lipids_upload"),
                 menuSubItem(
                   text = "Proteomics",
                   tabName = "proteo_upload"),
                 menuSubItem(
                   text = "Transcriptomics",
                   tabName = "transcripto_upload")),

        menuItem(text = "Visualisation",
                 tabName = "visualisation",
                 icon = shiny::icon("table"),
                 menuSubItem(
                   text = "Lipidomics",
                   tabName = "vis_lipids"),
                 menuSubItem(
                   text = "Proteomics",
                   tabName = "vis_prot"),
                 menuSubItem(
                   text = "Transcriptomics",
                   tabName = "vis_trans")))),

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
      ###################################################### Introduction tab
      tabItems(tabItem(tabName = "intro",
                       includeMarkdown(app_sys("app/www/welcome.Rmd"))),
               ############################################## Sample metadata upload
               tabItem(
                 tabName = "sample_metadata_upload",
                 fluidRow(
                   column(
                     width = 8,
                     bs4Dash::tabsetPanel(
                       id = "tabset_samples",
                       shiny::tabPanel(
                         title = "Load data",
                         datamods::import_file_ui(
                           id = "import_data_samples",
                           title = shiny::tags$h3("Sample metadata"),
                           preview_data = TRUE,
                           file_extensions = ".csv")),
                       shiny::tabPanel(
                         title = "Update",
                         uiOutput(outputId = "uiUpdateSampleData")))),
                   column(
                     shiny::tags$h3("Select columns"),
                     width = 4,
                     strong("Sample IDs"),
                     br(),
                     helpText("Column containing the sample IDs."),
                     selectInput(inputId = "samp_ID",
                                 label = NULL,
                                 choices = NULL,
                                 multiple = FALSE),
                     strong("Sample type"),
                     br(),
                     helpText("Column containing the sample types."),
                     selectInput(inputId = "samp_typecol",
                                 label = NULL,
                                 choices = NULL,
                                 multiple = FALSE),
                     strong("Blank pattern"),
                     br(),
                     helpText('Text pattern to autodect blanks samples from the above metioned "Sample type" column'),
                     textInput(inputId = "pattern_blank",
                               label = NULL,
                               value = "blank",
                               placeholder = "Type pattern"),
                     strong("QC pattern"),
                     br(),
                     helpText('Text pattern to autodect QC samples from the above metioned "Sample type" column'),
                     textInput(inputId = "pattern_qc",
                               label = NULL,
                               value = "qc",
                               placeholder = "Type pattern")
                   ))),
               ################################################### Lipidomics upload
               tabItem(
                 tabName = "lipids_upload",
                 fluidRow(
                   column(12,
                          bs4Dash::tabsetPanel(
                            id = "tabset_data",
                            shiny::tabPanel( # Load lipidomics data
                              title = "Load data",
                              fluidRow(
                                column(8,
                                       datamods::import_file_ui(
                                         id = "lipid_data_import",
                                         title = shiny::tags$h4("Lipidomics data"),
                                         preview_data = TRUE,
                                         file_extensions = ".csv")),
                                column(4,
                                       shiny::tags$h4("Select columns"),
                                       strong("Sample IDs"),
                                       br(),
                                       helpText("Column containing the sample IDs."),
                                       selectInput(inputId = "lips_ID",
                                                   label = NULL,
                                                   choices = NULL,
                                                   multiple = FALSE),
                                       strong("Group filtering"),
                                       br(),
                                       helpText("Column containing the groups for each sample."),
                                       selectInput(inputId = "lips_groupcol",
                                                   label = NULL,
                                                   choices = NULL,
                                                   multiple = FALSE)
                                ))),



                            shiny::tabPanel( # Update lipidomics data
                              title = "Update",
                              fluidRow(
                                column(12,
                                       uiOutput(outputId = "uiUpdateLipidData")))),
                            shiny::tabPanel( # Filter lipidomics data
                              title = "Filter",
                              fluidRow(
                                column(8,
                                       shiny::tags$h3("Lipidomics data :"),
                                       actionButton("reset_lips", "Reset"),
                                       actionButton("save_lips", "Save"),
                                       actionButton("display_lips", "Display"),
                                       progressBar(
                                         id = "pbar",
                                         value = 100,
                                         total = 100,
                                         display_pct = TRUE),
                                       reactable::reactableOutput(outputId = "table"),
                                ),
                                column(4,
                                       shiny::tags$h3("Thresholds"),
                                       helpText("Number of blanks."),
                                       textInput(
                                         inputId = "blank_thr",
                                         label = NULL,
                                         value = "2",
                                         width = NULL,
                                         placeholder = NULL),
                                       helpText("Proportion of samples above blank"),
                                       textInput(
                                         inputId = "blank_thr_nr_of_samples",
                                         label = NULL,
                                         value = "0.8",
                                         width = NULL,
                                         placeholder = NULL),
                                       helpText("Proportion of samples from one group above blank"),
                                       textInput(
                                         inputId = "group_thr_nr_of_samples",
                                         label = NULL,
                                         value = "0.7",
                                         width = NULL,
                                         placeholder = NULL),
                                       shiny::tags$hr(),
                                       shiny::tags$h3("Normalization"),
                                       helpText("Normalization"),
                                       selectInput(
                                         inputId = "norm",
                                         label = NULL,
                                         choices = c("Normalized to total class" = "class_norm",
                                                     "Normalized to total lipid" = "lipid_norm"),
                                         selected = "lipid_norm",
                                         multiple = FALSE),
                                       checkboxInput(
                                         inputId = "scale",
                                         label = "Scale and center",
                                         value = TRUE),
                                       uiOutput(outputId = "info"),
                                       uiOutput(outputId = "comp2"),
                                       shiny::tags$hr(),
                                       shiny::tags$h3("Discriminant analysis"),
                                       checkboxInput(
                                         inputId = "analysis1",
                                         label = "Apply disc. and red. analysis"),
                                       sliderInput(
                                         inputId = "alpha",
                                         label = "Alpha:",
                                         ticks = TRUE,
                                         min = 0.5,
                                         max = 0.95,
                                         value = 0.8,
                                         step = 0.01)
                                )))),
                   ), # end column
                 ) # end fluidRow
               ), # end tabItem file
               ############################################ Lipidomics visualisation
               tabItem(
                 tabName = "vis_lipids",
                 fluidRow(
                   column(
                     width = 4,
                     checkboxGroupButtons(inputId = "showPlots",
                                          label = "Show plots:",
                                          status = "primary",
                                          choices = c("Class distribution" = "plotlyClassDistribution",
                                                      "Class comparison" = "plotlyClassComparison",
                                                      "Volcano plot" = "volcano",
                                                      "Heatmap" = "heatmap",
                                                      "Unsaturation plot" = "plot_unsat",
                                                      "PCA" = "lips_pca"),
                                          selected = c("plotlyClassDistribution",
                                                       "plotlyClassComparison"))
                   )
                 ), # end fluidrow
                 uiOutput(outputId = "uiPlots")
               )
      ) # end tabItems
    ) # end body
  ) # end page

  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),

    # Your application UI logic
    ui
  )
}

# app_ui <- shinymanager::secure_app(ui = app_ui)

# app_ui <- shinymanager::secure_app(ui = ui)

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
