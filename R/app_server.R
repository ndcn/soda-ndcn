#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import datamods
#' @importFrom glmnet cv.glmnet
#' @importFrom plotly plotlyOutput ggplotly renderPlotly config layout
#' @importFrom ComplexHeatmap Heatmap draw
#' @importFrom InteractiveComplexHeatmap InteractiveComplexHeatmapWidget
#' @importFrom grid grid.newpage grid.text
#'
#' @noRd
app_server <- function(input, output, session) {
  boxDimension <- reactiveValues(width = NULL,
                                 height = NULL)

  # make sure that plots colouring doesn't change after adding/removing plots
  classDistributionSelected <- reactiveVal(NULL)
  classComparisonSelected <- reactiveVal(NULL)
  volcanoCategorySelected <- reactiveVal(NULL)
  volcanoComparisonSelected <- reactiveVal(NULL)
  heatmapLabelingSelected <- reactiveVal(NULL)
  heatmapDatasetSelected <- reactiveVal(NULL)
  isHeatmap <- reactiveVal(NULL)

  # for debugging output the dimensions of the window and the calculated dimensions
  # of the boxes (plots)
  output$dimension_display <- renderText({
    # paste("width:", input$dimension[1], "| height:", input$dimension[2], "\n",
    #       boxDimension$width, boxDimension$height)
  })

  # import data module
  imported <- import_file_server(
    id = "lipid_data_import",
    btn_show_data = FALSE,
    trigger_return = "change"
    # show_data_in = "popup" # not in this version of datamods
  )

  output$uiUpdateLipidData <- renderUI({
    req(imported$data)

    if(!is.null(imported$data())) {
      tagList(
        update_variables_ui(id = "update_lipid_data")
      )
    }
  })

  updated_data <- update_variables_server(
    id = "update_lipid_data",
    data = imported$data()
  )

  observe({
    if(!is.null(imported$data())) {
      print(sprintf("number of rows imported$data: %d", nrow(imported$data())))
    }
    if(!is.null(updated_data())) {
      print(sprintf("number of rows updated_data: %d", nrow(updated_data())))
    }
  })

  # read the data
  m3_scl <- reactive({
    req(imported$data,
        input$blank_thr,
        input$blank_thr_nr_of_samples,
        input$group_thr_nr_of_samples)

    # this does not work correctly when a new file is uploaded!
    if(is.null(updated_data())){
      lipid_data <- imported$data()
    } else {
      lipid_data <- updated_data()
    }

    if(!is.null(lipid_data)) {
      m_org <- import_lipid_data(data = lipid_data,
                                 blank_thr = as.numeric(input$blank_thr),
                                 blank_thr_nr_of_samples = as.numeric(input$blank_thr_nr_of_samples),
                                 group_thr_nr_of_samples = as.numeric(input$group_thr_nr_of_samples))
      cat("\n m3_scl calculated \n")

      return(m_org)
    } else {
      return(NULL)
    }
  })

  mat <- reactive({
    req(m3_scl,
        input$dataset,
        input$norm,
        input$scale,
        input$labeling)

    if(is.null(m3_scl()))
      return(NULL)

    meta_scl=m3_scl()$meta

    if(as.numeric(input$dataset) == 1 & input$norm == "class_norm") {
      m3_scl1 <- m3_scl()$data_classNorm
    } else if(as.numeric(input$dataset) == 1 & input$norm == "lipid_norm") {
      m3_scl1 <- m3_scl()$data_totLipidNorm
    } else if(as.numeric(input$dataset) == 2 & input$norm == "class_norm") {
      m3_scl1 <- m3_scl()$classData_totLipidNorm
    } else if(as.numeric(input$dataset) == 2 & input$norm == "lipid_norm") {
      m3_scl1 <- m3_scl()$classData_totLipidNorm
    }

    # quick fix to remove meta data from the data, this needs fixing!!!!
    meta_names <- c("sample_name", colnames(meta_scl))
    m3_scl1 <- m3_scl1[, !(colnames(m3_scl1) %in% meta_names)]

    tmp_rn <- rownames(m3_scl1)

    m3_scl1 <- if(input$scale) {
      scale(m3_scl1)
    } else {
      m3_scl1
    }

    rownames(m3_scl1) <- tmp_rn
    if(!is.null(input$labeling)) {
      lab_ind <- which(colnames(meta_scl) %in% input$labeling)
      if(length(input$labeling) == 1) {
        rownames(m3_scl1) <- meta_scl[, input$labeling]
      } else {
        rownames(m3_scl1) <- apply(meta_scl[, input$labeling],
                                   1,
                                   paste,
                                   collapse = "_")
      }
    }

    cat("\ninput$dataset=",input$dataset,"\n")
    cat("\nlength(m3_scl())=",length(m3_scl()),"\n")
    cat("\ndim(m3_scl1)=",dim(m3_scl1),"\n")

    cat("\ninput$comparision=",input$comparisionClassDistribution,"\n")

    comp_ind=which(colnames(meta_scl)%in%input$comparisionClassDistribution)

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

  mat_classes <- reactive({
    req(m3_scl,
        input$scale)

    if(is.null(m3_scl()))
      return(
        NULL
      )
    #names(m_org)
    # m3_scl1=m3_scl1_=m_org[[4]]
    # meta_scl=m_org[[6]]
    meta_scl=m3_scl()[[6]]
    m3_scl1=m3_scl()[[4]]
    tmp_rn=rownames(m3_scl1)

    # input=list()
    # input$comparision=colnames(meta_scl)[c(7)]
    # input$labeling=colnames(meta_scl)[c(3,4,8)]
    # input$scale=T

    m3_scl1=if(input$scale){scale(m3_scl1)}else{m3_scl1}

    rownames(m3_scl1)=tmp_rn
    if(!is.null(input$labeling))
    {

      lab_ind=which(colnames(meta_scl)%in%input$labeling)
      rownames(m3_scl1)=sapply(tmp_rn,
                               FUN = function(x)
                               {
                                 s=strsplit(x,split = "__",fixed = T)[[1]][lab_ind]
                                 paste(s,collapse = "_")
                               }
      )
    }
    mat_classes=m3_scl1
    mat_classes
  })


  ### calculate everything for the volcano plot
  volcano_db <- reactive({
    ###
    # this is a bit double, same as cls_db
    # needs a nicer solution
    ###

    req(m3_scl,
        input$norm)

    ind_for_comp1_ <- which(colnames(m3_scl()$meta) == input$siVolcanoCategory)
    comp1_i <- unique(m3_scl()$meta[, ind_for_comp1_])
    comp1_ <- comp1_i[which(comp1_i %in% input$siVolcanoComparison)]

    # only start calculations if 2 groups are choosen.
    if (length(input$siVolcanoComparison) == 2) {
      cat("\n#########################################################\n")
      cat("#########################################################\n")
      cat("Calculate volcano plot.\n\n")

      # which data to select
      inputCol <- switch(input$norm,
                         "lipid_norm" = "data_totLipidNorm",
                         "class_norm" = "data_classNorm")

      res <- generat_all_cls(l3 = m3_scl()[[inputCol]],
                             meta_s = m3_scl()$meta,
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
              plotOutput(outputId = "plotClassComparison",
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
                startOpen = length(volcanoComparisonSelected()) < 2,
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
          }
        ) # end fluidRow 2
      ) # end column
    ) # end tagList
  }) # end renderUI

  ### big bar plot showing class distribution
  output$plotlyClassDistribution <- renderPlotly({
    req(m3_scl,
        input$norm,
        input$comparisionClassDistribution,
        input$showPlots)

    inputCol <- switch(input$norm,
                       "lipid_norm" = "classData_totLipidNorm_melt",
                       "class_norm" = "classData_classNorm_melt")

    ggly <- ggplotly(plot_classes(l3 = m3_scl()[[inputCol]],
                                  meta_s = m3_scl()$meta,
                                  fill1 = input$comparisionClassDistribution))
    ggly |>
      plotly::config(displaylogo = FALSE) |>
      plotly::layout(xaxis = list(fixedrange = input$moveZoomPlots == "move"),
                     yaxis = list(fixedrange = input$moveZoomPlots == "move"))
  })

  observeEvent(input$comparisionClassDistribution, {
    classDistributionSelected(input$comparisionClassDistribution)
  })

  output$uiComparisonClassDistribution <- renderUI({
    if (is.null(imported$data()))
      return()

    selectInput(inputId = "comparisionClassDistribution",
                label = "Select category comparision",
                choices = colnames(m3_scl()$meta),
                selected = classDistributionSelected(),
                multiple = FALSE)
  })


  ### facetted bar plot
  output$plotClassComparison <- renderPlot({
    # for now this is a static plot
    # this renders quite slow as a plotly plot
    req(m3_scl,
        input$norm,
        input$comparisionClassComparison,
        input$showPlots)

    inputCol <- switch(input$norm,
                       "lipid_norm" = "classData_totLipidNorm_melt",
                       "class_norm" = "classData_classNorm_melt")

    plot_compare_classes(l4_2 = m3_scl()[[inputCol]],
                         meta_s = m3_scl()$meta,
                         compare1 = input$comparisionClassComparison)
  })

  observeEvent(input$comparisionClassComparison, {
    classComparisonSelected(input$comparisionClassComparison)
  })

  output$uiComparisonClassComparison <- renderUI({
    if (is.null(imported$data()))
      return()

    selectInput(inputId = "comparisionClassComparison",
                label = "Select category comparision",
                choices = colnames(m3_scl()$meta),
                selected = classComparisonSelected(),
                multiple = FALSE)
  })

  ### volcano plot
  output$plotVolcano <- renderPlotly({
    req(volcano_db,
        input$siVolcanoComparison)

    if(length(input$siVolcanoComparison) == 2) {
      ggly <- plot_class_volcano(all_cls = volcano_db())

      return(ggly |>
               config(displaylogo = FALSE))
    } else {
      return(NULL)
    }
  })

  observeEvent(input$siVolcanoCategory, {
    volcanoCategorySelected(input$siVolcanoCategory)
  })

  observeEvent(input$siVolcanoComparison, {
    if(length(input$siVolcanoComparison) == 2) {
      volcanoComparisonSelected(input$siVolcanoComparison)
    }
  })

  output$uiVolcanoCategory <- renderUI({
    req(m3_scl)

    selectInput(inputId = "siVolcanoCategory",
                label = "Select category",
                colnames(m3_scl()$meta),
                selected = volcanoCategorySelected(),
                multiple = FALSE)
  })

  output$uiVolcanoComparison <- renderUI({
    req(m3_scl,
        input$siVolcanoCategory)

    selectInput(inputId = "siVolcanoComparison",
                label = "Select two to compare",
                unique(m3_scl()$meta[, input$siVolcanoCategory]),
                selected = volcanoComparisonSelected(),
                multiple = TRUE)#,options = list(maxItems = 2))
  })


  ### Heatmap stuff
  observeEvent(input$show_heatmap, {
    req(mat)

    if("heatmap" %in% input$showPlots) {
      mat <- mat()
      # mat_classes <- mat_classes()

      ht <- ht2 <- NULL
      if(!is.null(mat)) {
        ht <- ComplexHeatmap::Heatmap(t(mat))
        ht <- ComplexHeatmap::draw(ht)
      }

      isHeatmap(ht)

      if(!is.null(ht)) {
        InteractiveComplexHeatmap::InteractiveComplexHeatmapWidget(input = input,
                                                                   output = output,
                                                                   session = session,
                                                                   ht_list = ht,
                                                                   height1 = boxDimension$height,
                                                                   output_id =  "plotHeatmap",
                                                                   layout = "1-(2|3)",
                                                                   close_button = FALSE)
      } else {
        output$plotHeatmap <- renderPlot({
          grid::grid.newpage()
          grid::grid.text("No row exists after filtering.")
        })
      }
    }
  })

  output$uiHeatmapButton <- renderUI({
    req(m3_scl)

    if (is.null(imported$data()))
      return()

    actionButton(inputId = "show_heatmap",
                 label = "Generate_heatmap")
  })

  observeEvent(input$labeling, {
    heatmapLabelingSelected(input$labeling)
  })

  observeEvent(input$dataset, {
    heatmapDatasetSelected(input$dataset)
  })

  output$uiHeatmapLabeling <- renderUI({
    req(m3_scl)

    if (is.null(imported$data()))
      return()

    selectInput(inputId = "labeling",
                label = "Select labeling",
                choices = colnames(m3_scl()$meta),
                selected = heatmapLabelingSelected(),
                multiple = TRUE)
  })

  output$uiHeatmapDataset <- renderUI({
    req(m3_scl)

    if (is.null(imported$data()))
      return()

    selectInput(inputId = "dataset",
                label = "Data set",
                choices = c("Species" = 1,
                            "Classes" = 2),
                selected = heatmapDatasetSelected(),
                multiple = FALSE)
  })
}
