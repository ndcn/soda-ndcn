#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import datamods
#' @import stringr
#' @import shinymanager
#' @importFrom glmnet cv.glmnet
#' @importFrom plotly plotlyOutput ggplotly renderPlotly config layout
#' @importFrom ComplexHeatmap Heatmap draw
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapWidget
#' @importFrom grid grid.newpage grid.text
#'
#'
#' @noRd
app_server <- function(input, output, session) {

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )

  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })

  boxDimension <- reactiveValues(width = NULL,
                                 height = NULL)

  # Initialise the plots
  volcanoComparisonSelected <- reactiveVal(NULL)
  heatmapLabelingSelected <- reactiveVal(NULL)
  heatmapDatasetSelected <- reactiveVal(NULL)
  isHeatmap <- reactiveVal(NULL)

  # Set up the color vector
  #colour_list= brewer.pal(n = 9, name = 'Set1')
  colour_list= brewer.pal(n = 11, name = 'Spectral')
  colour_list = colorRampPalette(colour_list)(25)

  ################################################################# Data import
  # Sample metadata
  imported_samps <- datamods::import_file_server(
    id = "import_data_samples",
    btn_show_data = FALSE,
    trigger_return = "change"
  )

  # Lipidomics data
  imported_lips <- datamods::import_file_server(
    id = "lipid_data_import",
    btn_show_data = FALSE,
    trigger_return = "change"
  )

  #Sample metadata : column selection
  observe({
    updateSelectInput(session = session,
                      inputId = "samp_ID",
                      choices = colnames(imported_samps$data()))
    updateSelectInput(session = session,
                      inputId = "lips_groupcol",
                      choices = colnames(imported_samps$data()),
                      selected = colnames(imported_samps$data())[2])
    updateSelectInput(session = session,
                      inputId = "samp_typecol",
                      choices = colnames(imported_samps$data()),
                      selected = colnames(imported_samps$data())[3])
  })

  # Lipids column selection
  observe({
    updateSelectInput(session = session,
                      inputId = "lips_ID",
                      choices = colnames(imported_lips$data()))
  })

  ########################################################## Original data safe

  # Save the original lipids data
  lips_unsaved = TRUE
  lips_safe = NULL
  observe({
    if(!is.null(imported_lips$data()) && lips_unsaved){
      lips_save = imported_lips$data()
      lips_unsaved = FALSE
    }
  })


  ################################################################# Update data

  # Sample metadata
  output$uiUpdateSampleData <- renderUI({
    req(imported_samps$data)
    if(!is.null(imported_samps$data())) {
      tagList(update_variables_ui(id = "update_samps"))}})
  updated_meta_samples <- update_variables_server(
    id = "update_samps",
    data = imported_samps$data()
  )

  # Lipidomics data
  output$uiUpdateLipidData <- renderUI({
    req(imported_lips$data)
    if(!is.null(imported_lips$data())) {
      tagList(update_variables_ui(id = "update_lips"))}})
  updated_lips <- update_variables_server(
    id = "update_lips",
    data = imported_lips$data()
  )

  observe({
    if(!is.null(updated_lips())) {
      print("nrows updated:")
      print(nrow(updated_lips()))
      print("ncols updated:")
      print(ncol(updated_lips()))
    }
  })

  ################################################################# Filter data

  observeEvent(input$reset_lips, {
    print("RESET")
  })
  observeEvent(input$save_lips, {
    print("SAVED")
  })
  observeEvent(input$display_lips, {
    print("nrows :")
    print(nrow(imported_lips$data()))
    print("ncols :")
    print(ncol(imported_lips$data()))
    print(imported_lips$name())
    print(imported_lips$status())
  })


  ######################################## Process lipids for unsaturation plot
  unsatplot_data = reactive({
    req(imported_lips$data,
        imported_samps$data,
        input$unsatplot_groupcol_selection,
        input$unsatplot_groups_selection)

    if (!is.null(imported_lips$data())){
      lips_table = imported_lips$data()
      samp_table = imported_samps$data()

      # Set index cols (move the samp_data blocks to a separate function in fct_files)
      samp_table = set_index(samp_table, input$samp_ID)
      lips_table = set_index(lips_table, input$lips_ID)


      samp_table = get_samp_table(samp_table,
                                  input$pattern_blank,
                                  input$pattern_qc,
                                  input$samp_typecol)


      # Filter out non-sample rows from the lipids table
      lips_table = lips_table[rownames(samp_table),]

      # Do wilxocon test on the lipids table
      wilcoxon_table = get_wilcoxon_table(lips_table = lips_table,
                                          samp_table = samp_table,
                                          selected_group = input$unsatplot_groupcol_selection,
                                          groups = input$unsatplot_groups_selection)

      # Get the unsaturation data from the lipids
      unsaturation_data = get_unsaturation_data(lips_table)

      # Combine wilcoxon test values with unsaturation data and return
      unsatplot_data = get_unsatplot_data(unsaturation_data, wilcoxon_table)

      return(unsatplot_data)
    } else {
      return(NULL)
    }
  })


  ######################################## Process lipids for PCA plot
  lipspca_plot = reactive({
    req(input$lipspca_groupcol_selection)

    if (!is.null(imported_lips$data())){
      lips_table = imported_lips$data()
      samp_table = imported_samps$data()

      print("LOLIGO")

      # Set index cols (move the samp_data blocks to a separate function in fct_files)
      samp_table = set_index(samp_table, input$samp_ID)
      lips_table = set_index(lips_table, input$lips_ID)


      # samp_table = get_samp_table(samp_table,
      #                             input$pattern_blank,
      #                             input$pattern_qc,
      #                             input$samp_typecol)


      # Filter out non-sample rows from the lipids table
      # lips_table = lips_table[rownames(samp_table),]


      fig = get_pca_plot(data_table = lips_table,
                         meta_table = samp_table,
                         group_col = input$lipspca_groupcol_selection,
                         colour_list = colour_list,
                         nPcs = 2,
                         scale = "none",
                         cv = "none")

      return(fig)
    } else {
      return(NULL)
    }
  })


  ####################################### Process lipids data for visualisation
  vistables_lips <- reactive({
    req(imported_lips$data,
        imported_samps$data,
        input$blank_thr,
        input$blank_thr_nr_of_samples,
        input$group_thr_nr_of_samples)

    # Get lipid and sample data, from source or updated
    # if(is.null(updated_lips())){
    #   lipid_data <- imported_lips$data()
    # } else {
    #   lipid_data <- updated_lips()
    # }
    lipid_data <- imported_lips$data()
    if(is.null(updated_meta_samples())){
      samp_data <- imported_samps$data()
    } else {
      samp_data <- updated_meta_samples()
    }

    # Set index cols (move the samp_data blocks to a separate function in fct_files)
    samp_data = set_index(samp_data, input$samp_ID)
    lipid_data = set_index(lipid_data, input$lips_ID)

    # Format to numeric and remove NAs
    lipid_data = table_to_numeric(lipid_data)

    # Get tables formated to visualise lipids data
    if(!is.null(lipid_data)) {
      vistables_lips = get_vistables_lips(samp_table = samp_data,
                                          lips_table = lipid_data,
                                          blank_thr = as.numeric(input$blank_thr),
                                          blank_thr_nr_of_samples = as.numeric(input$blank_thr_nr_of_samples),
                                          group_thr_nr_of_samples = as.numeric(input$group_thr_nr_of_samples),
                                          type_col = input$samp_typecol,
                                          group_col = input$lips_groupcol,
                                          pattern_blank = input$pattern_blank,
                                          pattern_qc = input$pattern_qc)
      print("TETRA")
      return(vistables_lips)
    } else {
      return(NULL)
    }
  })

  mat <- reactive({
    req(vistables_lips,
        input$dataset,
        input$norm,
        input$labeling)


    if(is.null(vistables_lips()))
      return(NULL)

    if(as.numeric(input$dataset) == 1 & input$norm == "class_norm") {
      m3_scl1 <- as.matrix(vistables_lips()$data_classNorm)
    } else if(as.numeric(input$dataset) == 1 & input$norm == "lipid_norm") {
      m3_scl1 <- as.matrix(vistables_lips()$data_totLipidNorm)
    } else if(as.numeric(input$dataset) == 2 & input$norm == "class_norm") {
      m3_scl1 <- as.matrix(vistables_lips()$classData_totLipidNorm)
    } else if(as.numeric(input$dataset) == 2 & input$norm == "lipid_norm") {
      m3_scl1 <- as.matrix(vistables_lips()$classData_totLipidNorm)
    }


    tmp_rn <- rownames(m3_scl1) # this gets row names for some reason

    m3_scl1 <- if(input$scale) {
      m3_scl1 = scale(m3_scl1)
    } else {
      m3_scl1
    }

    # rownames(m3_scl1) <- tmp_rn
    if(!is.null(input$labeling)) {
      lab_ind <- which(colnames(vistables_lips()$samp_table) %in% input$labeling)
      if(length(input$labeling) == 1) {
        rownames(m3_scl1) <- vistables_lips()$samp_table[, input$labeling]
      } else {
        rownames(m3_scl1) <- apply(vistables_lips()$samp_table[, input$labeling],
                                   1,
                                   paste,
                                   collapse = "_")
      }
    }

    cat("\ninput$dataset=",input$dataset,"\n")
    cat("\nlength(vistables_lips())=",length(vistables_lips()),"\n")
    cat("\ndim(m3_scl1)=",dim(m3_scl1),"\n")

    cat("\ninput$comparision=",input$comparisionClassDistribution,"\n")

    comp_ind=which(colnames(vistables_lips()$samp_table)%in%input$comparisionClassDistribution)

    # group_m3=group_m3_=sapply(rownames(m3_scl1),FUN = function(x) strsplit(x,split = "__",fixed = T)[[1]][3]) #[input$comparision])

    ind_coef=c(1:length(m3_scl1[1,]))

    # input$analysis1=T
    if (input$analysis1)
    {
      group_m3=group_m3_=sapply(tmp_rn,
                                FUN = function(x)
                                {
                                  s=strsplit(x,split = "__",fixed = T)[[1]][comp_ind]
                                  paste(s,collapse = "_")
                                }
      )

      g=unique(group_m3)
      #g
      for(i in c(1:length(g)))
        group_m3[which(group_m3==g[i])]=i



      ind_rem_group=which(as.numeric(group_m3)%in%which(table(group_m3)<3))
      if(length(ind_rem_group)>0)
      {
        group_m3=group_m3[-ind_rem_group]
      }

      if(length(unique(group_m3))>1)
      {
        if(length(ind_rem_group)>0)
        {
          m3_scl1=m3_scl1[-ind_rem_group,]
        }
        # input$alpha=0.8
        m3_scl1[which(is.na(m3_scl1),arr.ind = T)]=0

        set.seed(100)
        cvfit=cv.glmnet(m3_scl1,(group_m3),nlambda=100,alpha=as.numeric(input$alpha),family="multinomial",type.multinomial="grouped")
        coef=coef(cvfit,s="lambda.min")
        tmp=as.matrix(coef$'1')

        tmp1=tmp[which(tmp!=0)]
        coef_names=rownames(tmp)[which(tmp!=0)][-1]
        ind_coef=which(colnames(m3_scl1)%in%coef_names)
        m3_scl1[,ind_coef]
        # input$analysis1=T
      }
    }
    # tmp=if (input$analysis1){m3_scl1[,ind_coef]}else{m3_scl1[,]}

    tmp=m3_scl1[,ind_coef]

    cat("\ndim(m3_scl1)=",dim(m3_scl1),"\n")
    cat("\ndim(tmp)=",dim(tmp),"\n")

    # rownames(tmp)=group_m3_
    mat=tmp

    return(mat)
  })





  ### calculate everything for the volcano plot
  volcano_db <- reactive({
    ###
    # this is a bit double, same as cls_db
    # needs a nicer solution
    ###

    req(vistables_lips,
        input$norm)

    ind_for_comp1_ <- which(colnames(vistables_lips()$samp_table) == input$siVolcanoCategory)
    comp1_i <- unique(as.factor(vistables_lips()$samp_table[, ind_for_comp1_]))
    comp1_ <- comp1_i[which(comp1_i %in% input$siVolcanoComparison)]

    # only start calculations if 2 groups are choosen.
    if (length(input$siVolcanoComparison) == 2) {

      # which data to select
      inputCol <- switch(input$norm,
                         "lipid_norm" = "data_totLipidNorm",
                         "class_norm" = "data_classNorm")

      res <- generat_all_cls(l3 = vistables_lips()[[inputCol]],
                             meta_s = vistables_lips()$samp_table,
                             ind_for_comp1 = ind_for_comp1_,
                             comp1 = comp1_
      )

      # make one big data.frame
      res <- do.call("rbind", lapply(res, function(x) {
        tmp <- x[, c("logFC", "logP")]
        tmp$lipid_name <- rownames(x)
        tmp$lipid_class <- sub(x = tmp$lipid_name,
                               pattern = "^([a-zA-Z]*).*",
                               replacement = "\\1")

        return(tmp)
      }))

    } else {
      res <- NULL
    }
    return(res)
  })

  # set the dimensions of the boxes
  observe({
    boxWidth <- switch(
      as.character(length(input$showPlots)),
      "1" = 12,
      "2" = 6,
      "3" = 6,
      "4" = 6
    )

    boxHeight <- switch(
      # newHeight = windowHeight - header - buttons - margin
      as.character(length(input$showPlots)),
      "1" = paste0(input$dimension[2] - 75 - 125 - 75, "px"),
      "2" = paste0(input$dimension[2] - 75 - 125 - 75, "px"),
      "3" = paste0((input$dimension[2] - 75 - 225 - 75) / 2, "px"),
      "4" = paste0((input$dimension[2] - 75 - 225 - 75) / 2, "px")
    )

    boxDimension$width <- boxWidth
    boxDimension$height <- boxHeight
  })

  # render all plots
  output$uiPlots <- renderUI({
    req(boxDimension)

    tagList(
      column(
        width = 12,
        fluidRow(
          # div(id = "divPlots",
          if("plotlyClassDistribution" %in% input$showPlots) {
            # !!instead of setting the box height, set the plot height!!
            bs4Dash::box(
              id = "classDistributionBox",
              title = "Class distribution",
              width = boxDimension$width,
              solidHeader = TRUE,
              maximizable = TRUE,
              collapsible = FALSE,
              status = "primary",
              plotlyOutput(outputId = "plotlyClassDistribution",
                           height = boxDimension$height),
              sidebar = bs4Dash::boxSidebar(
                id = "classDistributionSidebar",
                width = 40,
                uiOutput(outputId = "uiComparisonClassDistribution")
              )
            )
          } else {
            NULL
          },
          if("plotlyClassComparison" %in% input$showPlots) {
            # !!instead of setting the box height, set the plot height!!
            bs4Dash::box(
              id = "classComparisonBox",
              title = "Class comparison",
              width = boxDimension$width,
              solidHeader = TRUE,
              maximizable = TRUE,
              collapsible = FALSE,
              status = "primary",
              plotlyOutput(outputId = "plotClassComparison",
                         height = boxDimension$height),
              sidebar = bs4Dash::boxSidebar(
                id = "classComparisonSidebar",
                width = 40,
                uiOutput(outputId = "uiComparisonClassComparison")
              )
            )
          } else {
            NULL
          },
          if("volcano" %in% input$showPlots) {
            # !!instead of setting the box height, set the plot height!!
            bs4Dash::box(
              id = "volcanoPlotBox",
              title = "Volcano plot",
              width = boxDimension$width,
              solidHeader = TRUE,
              maximizable = TRUE,
              collapsible = FALSE,
              status = "primary",
              plotlyOutput(outputId = "plotVolcano",
                           height = boxDimension$height),
              sidebar = bs4Dash::boxSidebar(
                id = "volcanoPlotSidebar",
                # this causes the plot to be recalculated with the initial values
                # startOpen = length(volcanoComparisonSelected()) < 2,
                width = 40,
                uiOutput(outputId = "uiVolcanoCategory"),
                uiOutput(outputId = "uiVolcanoComparison")
              )
            )
          } else {
            NULL
          },
          if("heatmap" %in% input$showPlots) {
            # !!instead of setting the box height, set the plot height!!
            bs4Dash::box(
              id = "heatmapPlotBox",
              title = "Heatmap",
              width = boxDimension$width,
              solidHeader = TRUE,
              maximizable = TRUE,
              collapsible = FALSE,
              status = "primary",
              div(style = paste0("height:", boxDimension$height),
                  htmlOutput(outputId = "plotHeatmap"),
              ),
              sidebar = bs4Dash::boxSidebar(
                id = "heatmapPlotSidebar",
                startOpen = is.null(isHeatmap()),
                width = 40,
                uiOutput(outputId = "uiHeatmapLabeling"),
                uiOutput(outputId = "uiHeatmapDataset"),
                uiOutput(outputId = "uiHeatmapButton")
              )
            )
          } else {
            NULL
          },
          if("plot_unsat" %in% input$showPlots) {
            # !!instead of setting the box height, set the plot height!!
            bs4Dash::box(
              id = "plot_unsat_box",
              title = "Unsaturation plot",
              width = boxDimension$width,
              solidHeader = TRUE,
              maximizable = TRUE,
              collapsible = FALSE,
              status = "primary",
              plotlyOutput(outputId = "plot_unsat",
                           height = boxDimension$height),
              sidebar = bs4Dash::boxSidebar(
                id = "unsatplot_sidebar",
                width = 40,
                uiOutput(outputId = "ui_unsatplot_groupcol"),
                uiOutput(outputId = "ui_unsatplot_groups"),
                uiOutput(outputId = "ui_unsatplot_classes")
              )
            )
          } else {
            NULL
          },
          if("lips_pca" %in% input$showPlots) {
            # !!instead of setting the box height, set the plot height!!
            bs4Dash::box(
              id = "plot_lipspca_box",
              title = "Principal Component Analysis (PCA)",
              width = boxDimension$width,
              solidHeader = TRUE,
              maximizable = TRUE,
              collapsible = FALSE,
              status = "primary",
              plotlyOutput(outputId = "plot_lipspca",
                           height = boxDimension$height),
              sidebar = bs4Dash::boxSidebar(
                id = "lipspca_sidebar",
                width = 40,
                uiOutput(outputId = "ui_lipspca_groupcol")
              )
            )
          } else {
            NULL
          }
        ) # end fluidRow 2
      ) # end column
    ) # end tagList
  }) # end renderUI

  ### big bar plot showing class distribution
  output$plotlyClassDistribution <- renderPlotly({
    req(vistables_lips,
        input$norm,
        input$comparisionClassDistribution,
        input$showPlots)

    inputCol <- switch(input$norm,
                       "lipid_norm" = "classData_totLipidNorm",
                       "class_norm" = "data_classNorm_melt")


    fig = plot_class_distribution(table = vistables_lips()[[inputCol]],
                                  samp_table = vistables_lips()$samp_table,
                                  group_col = input$comparisionClassDistribution,
                                  colour_list = colour_list)
  })

  output$uiComparisonClassDistribution <- renderUI({
    if (is.null(imported_lips$data()))
      return()

    selectInput(inputId = "comparisionClassDistribution",
                label = "Select category comparision",
                choices = colnames(vistables_lips()$samp_table),
                selected = input$lips_groupcol,
                multiple = FALSE)
  })


  # Unsaturation plot
  output$plot_unsat <- renderPlotly({
    req(unsatplot_data,
        input$unsatplot_classes)
    fig = plot_lipid_unsaturation(lips_class = input$unsatplot_classes,
                                  unsatplot_data = unsatplot_data())
  })

  output$ui_unsatplot_groupcol <- renderUI({
    selectInput(inputId = "unsatplot_groupcol_selection",
                label = "Select group column",
                choices = colnames(vistables_lips()$samp_table),
                selected = input$lips_groupcol,
                multiple = FALSE)

  })
  output$ui_unsatplot_groups <- renderUI({
    req(input$unsatplot_groupcol_selection)
    selectizeInput(inputId = "unsatplot_groups_selection",
                   label = "Select groups",
                   choices = unique(vistables_lips()$samp_table[, input$unsatplot_groupcol_selection]),
                   selected = unique(vistables_lips()$samp_table[, input$unsatplot_groupcol_selection])[c(1,2)],
                   multiple = TRUE,
                   options = list(maxItems = 2))
  })
  output$ui_unsatplot_classes <- renderUI({
    req(unsatplot_data)
    selectInput(inputId = "unsatplot_classes",
                label = "Select lipid class",
                choices = names(unsatplot_data()),
                selected = names(unsatplot_data())[1],
                multiple = FALSE)
  })

  # Lipids PCA plot

  output$plot_lipspca <- renderPlotly({
    req(lipspca_plot,
        input$lipspca_groupcol_selection)

    return(lipspca_plot())

  })

  output$ui_lipspca_groupcol <- renderUI({
    selectInput(inputId = "lipspca_groupcol_selection",
                label = "Select group column",
                choices = colnames(vistables_lips()$samp_table),
                selected = input$lips_groupcol,
                multiple = FALSE)
  })


  ### facetted bar plot
  output$plotClassComparison <- renderPlotly({
    req(vistables_lips,
        input$norm,
        input$comparisionClassComparison,
        input$showPlots)

    inputCol <- switch(input$norm,
                       "lipid_norm" = "classData_totLipidNorm",
                       "class_norm" = "data_classNorm_melt")

    class_comparison = plot_class_comparison(table = vistables_lips()[[inputCol]],
                                             samp_table = vistables_lips()$samp_table,
                                             group_col = input$comparisionClassComparison,
                                             colour_list = colour_list)
    return(class_comparison)
  })

  output$uiComparisonClassComparison <- renderUI({
    if (is.null(imported_lips$data()))
      return()

    selectInput(inputId = "comparisionClassComparison",
                label = "Select category comparision",
                choices = colnames(vistables_lips()$samp_table),
                selected = input$lips_groupcol,
                multiple = FALSE)
  })

  ### volcano plot
  output$plotVolcano <- renderPlotly({
    req(volcano_db,
        input$siVolcanoComparison)

    if(length(input$siVolcanoComparison) == 2) {
      fig <- plot_class_volcano(all_cls = volcano_db())

      return(fig |>
               config(displaylogo = FALSE))
    } else {
      return(NULL)
    }
  })

  observeEvent(input$siVolcanoComparison, {
    if(length(input$siVolcanoComparison) == 2) {
      volcanoComparisonSelected(input$siVolcanoComparison)
    }
  })

  output$uiVolcanoCategory <- renderUI({
    req(vistables_lips)

    selectInput(inputId = "siVolcanoCategory",
                label = "Select category",
                colnames(vistables_lips()$samp_table),
                selected = input$lips_groupcol,
                multiple = FALSE)
  })

  output$uiVolcanoComparison <- renderUI({
    req(vistables_lips,
        input$siVolcanoCategory)

    selectizeInput(inputId = "siVolcanoComparison",
                label = "Select two to compare",
                choices = unique(vistables_lips()$samp_table[, input$siVolcanoCategory]),
                selected = unique(vistables_lips()$samp_table[, input$siVolcanoCategory])[1:2],
                multiple = TRUE,
                options = list(maxItems = 2))
  })


  ### Heatmap stuff
  observeEvent(input$show_heatmap, {
    req(mat)
      mat = mat()

      if(!is.null(mat)) {
        print("CHECKPOINT 1")
        ht <- ComplexHeatmap::Heatmap(t(mat))
        print("CHECKPOINT 2")
        ht <- ComplexHeatmap::draw(ht)
        print("CHECKPOINT 3")
        InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input = input,
                                                                   output = output,
                                                                   session = session,
                                                                   ht_list = ht,
                                                                   height1 = boxDimension$height,
                                                                   output_id =  "plotHeatmap",
                                                                   layout = "1-(2|3)",
                                                                   close_button = FALSE)
        print("CHECKPOINT 4")
      }else{
        output$plotHeatmap <- renderPlot({
          grid::grid.newpage()
          grid::grid.text("No row exists after filtering.")
        })
      }
  })

  output$uiHeatmapButton <- renderUI({
    req(vistables_lips)

    if (is.null(imported_lips$data()))
      return()

    actionButton(inputId = "show_heatmap",
                 label = "Generate heatmap")
  })

  observeEvent(input$labeling, {
    heatmapLabelingSelected(input$labeling)
  })

  observeEvent(input$dataset, {
    heatmapDatasetSelected(input$dataset)
  })

  output$uiHeatmapLabeling <- renderUI({
    req(vistables_lips)

    if (is.null(imported_lips$data()))
      return()

    selectInput(inputId = "labeling",
                label = "Select labeling",
                choices = colnames(vistables_lips()$samp_table),
                selected = heatmapLabelingSelected(),
                multiple = TRUE)
  })

  output$uiHeatmapDataset <- renderUI({
    req(vistables_lips)

    if (is.null(imported_lips$data()))
      return()

    selectInput(inputId = "dataset",
                label = "Data set",
                choices = c("Species" = 1,
                            "Classes" = 2),
                selected = heatmapDatasetSelected(),
                multiple = FALSE)
  })
}