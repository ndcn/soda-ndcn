#------------------------------------------------------- Class distribution ----

class_distribution_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Class distribution: generating plot.")

  if (input$class_distribution_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_class_distribution(table = table_switch(input$class_distribution_dataset, r6),
                             group_col = input$class_distribution_metacol,
                             colour_list = colour_list,
                             width = width,
                             height = height)
}


class_distribution_spawn = function(r6, format, output) {
  print_tm(r6$name, "Class distribution: spawning plot.")
  output$class_distribution_plot = plotly::renderPlotly({
    r6$plots$class_distribution
    plotly::config(r6$plots$class_distribution, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_name('class_distribution'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


class_distribution_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "class_distribution",
                 label = "Class distribution",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


class_distribution_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Class distribution : START.")
  # Generate UI
  output$class_distribution_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("class_distribution_dataset"),
        label = "Select table",
        choices = c('Class table', 'Class table total normalized'),
        selected = r6$params$class_distribution$dataset
      ),
      shiny::selectInput(
        inputId = ns("class_distribution_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$class_distribution$group_col
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("class_distribution_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$class_distribution$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_class_distribution_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })


  # class_distribution_events(r6, dimensions_obj, color_palette, input, output, session)

}

class_distribution_events = function(r6, dimensions_obj, color_palette, input, output, session) {



  # Generate the plot
  session$userData[[r6$id]]$class_distribution = shiny::observeEvent(c(input$class_distribution_dataset, input$class_distribution_metacol, input$class_distribution_img_format), {
    print_tm(r6$name, "Class distribution: Updating params...")

    r6$param_class_distribution(dataset = input$class_distribution_dataset,
                                group_col = input$class_distribution_metacol,
                                img_format = input$class_distribution_img_format)

    base::tryCatch({
      class_distribution_generate(r6, color_palette, dimensions_obj, input)
      class_distribution_spawn(r6, input$class_distribution_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Class distribution: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$download_class_distribution_table = shiny::downloadHandler(
    filename = function(){timestamped_name("class_distribution_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$class_distribution_table, file_name)
    }
  )

  # Expanded boxes
  class_distribution_proxy = plotly::plotlyProxy(outputId = "class_distribution_plot",
                                                 session = session)

  session$userData[[r6$id]]$class_distribution_proxy = shiny::observeEvent(input$class_distribution_plotbox,{
    if (input$class_distribution_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = class_distribution_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = class_distribution_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#--------------------------------------------------------- Class comparison ----

class_comparison_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Class comparison: generating plot.")

  if (input$class_comparison_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }



  r6$plot_class_comparison(data_table = table_switch(input$class_comparison_dataset, r6),
                           group_col = input$class_comparison_metacol,
                           colour_list = colour_list,
                           width = width,
                           height = height)
}

class_comparison_spawn = function(r6, format, output) {
  print_tm(r6$name, "Class comparison: spawning plot.")
  output$class_comparison_plot = plotly::renderPlotly({
    r6$plots$class_comparison
    plotly::config(r6$plots$class_comparison, toImageButtonOptions = list(format= format,
                                                                          filename= timestamped_name('class_comparison'),
                                                                          height= NULL,
                                                                          width= NULL,
                                                                          scale= 1))
  })
}




class_comparison_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "class_comparison",
                 label = "Class comparison",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


class_comparison_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "Class comparison: START.")

  output$class_comparison_sidebar_ui = shiny::renderUI({
    shiny::tagList(

      shiny::selectInput(
        inputId = ns("class_comparison_dataset"),
        label = "Select table",
        choices = c('Class table', 'Class table total normalized'),
        selected = r6$params$class_comparison$dataset
      ),
      shiny::selectInput(
        inputId = ns("class_comparison_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$class_comparison$group_col
      ),
      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("class_comparison_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$class_comparison$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_class_comparison_table"),
        label = "Download unavailable for now",
        style = "width:100%;"
      )
    )
  })
}
class_comparison_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$class_comparison_dataset, input$class_comparison_metacol, input$class_comparison_img_format), {
    print_tm(r6$name, "Class comparison: Updating params...")

    r6$param_class_comparison(dataset = input$class_comparison_dataset,
                              group_col = input$class_comparison_metacol,
                              img_format = input$class_comparison_img_format)

    base::tryCatch({
      class_comparison_generate(r6, color_palette, dimensions_obj, input)
      class_comparison_spawn(r6, input$class_comparison_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Class comparison: error, missing data.')
    },finally={}
    )


  })


  # # Download associated table
  # output$download_class_comparison_table = shiny::downloadHandler(
  #   filename = function(){"class_comparison_table.csv"},
  #   content = function(file_name){
  #     write.csv(r6$tables$class_distribution_table, file_name)
  #   }
  # )


  # Expanded boxes
  class_comparison_proxy = plotly::plotlyProxy(outputId = "class_comparison_plot",
                                               session = session)

  shiny::observeEvent(input$class_comparison_plotbox,{
    if (input$class_comparison_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = class_comparison_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = class_comparison_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#------------------------------------------------------------- Volcano plot ----

volcano_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Volcano plot: generating plot.")

  if (input$volcano_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$get_volcano_table(data_table = table_switch(input$volcano_plot_tables, r6),
                       group_col = input$volcano_plot_metacol,
                       used_function =  input$volcano_plot_function,
                       test = input$volcano_plot_test,
                       group_1 = input$volcano_plot_metagroup[1],
                       group_2 = input$volcano_plot_metagroup[2])

  r6$plot_volcano(data_table = r6$tables$volcano_table,
                  adjustment = input$volcano_plot_adjustment,
                  colour_list = colour_list,
                  group_1 = input$volcano_plot_metagroup[1],
                  group_2 = input$volcano_plot_metagroup[2],
                  displayed_classes = input$volcano_plot_lipclass,
                  colouring = input$volcano_plot_colouring,
                  width = width,
                  height = height)

}


volcano_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Volcano plot: spawning plot.")
  output$volcano_plot_plot = plotly::renderPlotly({
    r6$plots$volcano_plot
    plotly::config(r6$plots$volcano_plot, toImageButtonOptions = list(format= format,
                                                                      filename= timestamped_name('volcano_plot'),
                                                                      height= NULL,
                                                                      width= NULL,
                                                                      scale= 1))
  })
}

volcano_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "volcano_plot",
                 label = "Volcano plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


volcano_plot_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "Volcano plot: START.")

  # Set UI
  output$volcano_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("volcano_plot_tables"),
        label = "Select data table",
        choices = c("Raw data table", "Class normalized table", "Total normalized table"),
        selected = r6$params$volcano_plot$data_table
      ),
      shiny::selectInput(
        inputId = ns("volcano_plot_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$volcano_plot$group_col
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_metagroup"),
        label = "Select two groups to compare",
        choices = unique(r6$tables$raw_meta[,r6$params$volcano_plot$group_col]),
        selected = r6$params$volcano_plot$groups,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_lipclass"),
        label = "Classes to display",
        choices = unique(r6$tables$feature_table[,"lipid_class"]),
        selected = r6$params$volcano_plot$classes,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_function"),
        label = "Select function",
        choices = c("median", "mean"),
        selected = r6$params$volcano_plot$selected_function,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_test"),
        label = "Select test",
        choices = c("Wilcoxon", "t-Test"),
        selected = r6$params$volcano_plot$selected_test,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_adjustment"),
        label = "Select adjustment",
        choices = c("None", "Benjamini-Hochberg"),
        selected = r6$params$volcano_plot$adjustment,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("volcano_plot_colouring"),
        label = "Select colouring",
        choices = c("Lipid class", "Double bonds (chain 1)", "Double bonds (chain 2)", "Double bonds (sum)", "Carbon count (chain 1)", "Carbon count (chain 2)", "Carbon count (sum)"),
        selected = r6$params$volcano_plot$colouring,
        multiple = FALSE
      ),
      shiny::actionButton(
        inputId = ns("volcano_feature_select"),
        label = "Save selection",
        width = "100%"
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("volcano_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$volcano_plot$img_format,
        width = "100%"),

      shiny::fluidRow(
        shiny::downloadButton(
          outputId = ns("download_volcano_table"),
          label = "Download associated table",
          style = "width:50%;"
        ),
        shiny::downloadButton(
          outputId = ns("download_volcano_subtable"),
          label = "Download selection",
          style = "width:50%;"
        )
      )
    )
  })
}

volcano_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # auto-update selected groups
  shiny::observeEvent(input$volcano_plot_metacol,{
    # r6$params$volcano_plot$group_column = input$volcano_plot_metacol
    shiny::updateSelectizeInput(
      inputId = "volcano_plot_metagroup",
      session = session,
      choices = unique(r6$tables$raw_meta[,input$volcano_plot_metacol]),
      selected = unique(r6$tables$raw_meta[,input$volcano_plot_metacol])[c(1,2)]
    )
  })

  shiny::observeEvent(c(input$volcano_plot_metagroup, input$volcano_plot_tables, input$volcano_plot_function, input$volcano_plot_colouring, input$volcano_plot_lipclass, input$volcano_plot_adjustment, input$volcano_plot_test, input$volcano_plot_img_format), {
    shiny::req(length(input$volcano_plot_metagroup) == 2)

    print_tm(r6$name, "Volcano plot: Updating params...")
    r6$param_volcano_plot(data_table = input$volcano_plot_tables,
                          adjustment = input$volcano_plot_adjustment,
                          group_col = input$volcano_plot_metacol,
                          groups = input$volcano_plot_metagroup,
                          classes = input$volcano_plot_lipclass,
                          selected_function = input$volcano_plot_function,
                          selected_test = input$volcano_plot_test,
                          colouring = input$volcano_plot_colouring,
                          img_format = input$volcano_plot_img_format)

    volcano_plot_generate(r6, color_palette, dimensions_obj, input)
    volcano_plot_spawn(r6, input$volcano_plot_img_format, output)
  })



  # Save selection
  shiny::observeEvent(input$volcano_feature_select, {
    print_tm(r6$name, "Volcano plot: saving selection.")
    volcano_selection = plotly::event_data(
      "plotly_selected"
    )
    if (is.null(volcano_selection)){
      print_tm(r6$name, "Brushed points appear here (double-click to clear)")
    }else {
      r6$slice_volcano_table(
        x = volcano_selection[3][[1]],
        y = volcano_selection[4][[1]],
        x_col = "log2_fold_change",
        y_col = adjustment_switch(input$volcano_plot_adjustment)
      )
    }
  })

  # Export volcano table
  output$download_volcano_table = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_table, file_name)
    }
  )

  output$download_volcano_subtable = shiny::downloadHandler(
    filename = function(){timestamped_name("volcano_table_selection.csv")},
    content = function(file_name){
      write.csv(r6$tables$volcano_table_slice, file_name)
    }
  )

  # Expanded boxes
  volcano_plot_proxy = plotly::plotlyProxy(outputId = "volcano_plot_plot",
                                           session = session)

  shiny::observeEvent(input$volcano_plot_plotbox,{
    if (input$volcano_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = volcano_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = volcano_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}

#----------------------------------------------------------------- Heat map ----

heatmap_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Heatmap: generating plot.")

  if (input$heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_heatmap(data_table = table_switch(input$heatmap_dataset, r6),
                  impute = input$heatmap_impute,
                  meta_table = r6$tables$raw_meta,
                  meta_table_features = r6$tables$feature_table,
                  cluster_rows = input$heatmap_cluster_samples,
                  cluster_cols = input$heatmap_cluster_features,
                  row_annotations = input$heatmap_map_rows,
                  col_annotations = input$heatmap_map_cols,

                  apply_da = input$heatmap_apply_da,
                  group_column_da = input$heatmap_group_col_da,
                  alpha_da = input$heatmap_alpha_da,

                  width = dimensions_obj$xpx * dimensions_obj$x_plot,
                  height = dimensions_obj$ypx * dimensions_obj$y_plot)
}

heatmap_spawn = function(r6, format, output) {
  print_tm(r6$name, "Heatmap: spawning plot.")
  output$heatmap_plot = plotly::renderPlotly({
    r6$plots$heatmap
    plotly::config(r6$plots$heatmap, toImageButtonOptions = list(format= format,
                                                                 filename= timestamped_name('heatmap'),
                                                                 height= NULL,
                                                                 width= NULL,
                                                                 scale= 1))
  })
}


heatmap_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "heatmap",
                 label = "Heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


heatmap_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "Heatmap: START.")

  output$heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("heatmap_dataset"),
        label = "Select dataset",
        choices = c('Z-scored table', 'Class table z-scored'),
        selected = r6$params$heatmap$dataset
      ),

      shiny::fluidRow(
        shiny::column(
          width = 4,
          shinyWidgets::switchInput(inputId = ns("heatmap_impute"),
                                    label = "Impute missing",
                                    value = r6$params$heatmap$imputation,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
          )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::switchInput(inputId = ns("heatmap_cluster_samples"),
                                    label = "Cluster samples",
                                    value = r6$params$heatmap$cluster_samples,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
                                    )
        ),
        shiny::column(
          width = 4,
          shinyWidgets::switchInput(inputId = ns("heatmap_cluster_features"),
                                    label = "Cluster features",
                                    value = r6$params$heatmap$cluster_features,
                                    onLabel = 'YES',
                                    offLabel = 'NO',
                                    labelWidth = '150px'
          )
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectizeInput(
            inputId = ns("heatmap_map_rows"),
            label = "Map sample data",
            multiple = TRUE,
            choices = colnames(r6$tables$raw_meta),
            selected = r6$params$heatmap$map_sample_data
          )
        ),
        shiny::column(
          width = 6,
          shiny::selectizeInput(
            inputId = ns("heatmap_map_cols"),
            label = "Map feature data",
            multiple = TRUE,
            choices = c('Lipid class', 'Double bonds (chain 1)', 'Carbon count (chain 1)', 'Double bonds (chain 2)',
                        'Carbon count (chain 2)', 'Double bonds (sum)', 'Carbon count (sum)'),
            selected = r6$params$heatmap$map_feature_data
          )
        )
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h3("Discriminant analysis")
        ),
        shiny::column(
          width = 6,
          shinyWidgets::switchInput(inputId = ns("heatmap_apply_da"),
                                    label = "Apply",
                                    value = r6$params$heatmap$apply_da)
        )
      ),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::selectizeInput(inputId = ns("heatmap_group_col_da"),
                                label = "Group column",
                                choices = colnames(r6$tables$raw_meta),
                                selected = r6$params$heatmap$group_column_da,
                                multiple = FALSE,
                                width = "100%")
        ),
        shiny::column(
          width = 6,
          shiny::sliderInput(inputId = ns("heatmap_alpha_da"),
                             label = "Alpha",
                             min = 0,
                             max = 0.99,
                             value = r6$params$heatmap$alpha_da,
                             step = 0.01,
                             width = "100%")
        )
      ),

      shiny::actionButton(
        inputId = ns("heatmap_run"),
        label = "Generate heatmap",
        width = "100%"
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("heatmap_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$heatmap$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_heatmap_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(input$heatmap_run,{
    shinyjs::disable("heatmap_run")
    print_tm(r6$name, "Heatmap: Updating params...")

    r6$param_heatmap(dataset = input$heatmap_dataset,
                     impute = input$heatmap_impute,
                     cluster_samples = input$heatmap_cluster_samples,
                     cluster_features = input$heatmap_cluster_features,
                     map_sample_data = input$heatmap_map_rows,
                     map_feature_data = input$heatmap_map_cols,
                     group_column_da = input$heatmap_group_col_da,
                     apply_da = input$heatmap_apply_da,
                     alpha_da = input$heatmap_alpha_da,
                     img_format = input$heatmap_img_format)

    base::tryCatch({
      heatmap_generate(r6, color_palette, dimensions_obj, input)
      heatmap_spawn(r6, input$heatmap_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Heatmap: ERROR.')
      shinyjs::enable("heatmap_run")
    },finally={}
    )

    shinyjs::enable("heatmap_run")
  })


  # Download associated table
  output$download_heatmap_table = shiny::downloadHandler(
    filename = function(){timestamped_name("heatmap_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$heatmap_table, file_name)
    }
  )

  # Expanded boxes
  heatmap_proxy = plotly::plotlyProxy(outputId = "heatmap_plot",
                                      session = session)

  shiny::observeEvent(input$heatmap_plotbox,{
    if (input$heatmap_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}



#---------------------------------------------------------------------- PCA ----

pca_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "PCA: generating plot.")

  if (input$pca_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }


  r6$plot_pca(data_table = table_switch(input$pca_dataset, r6),
              group_column = input$pca_metacol,
              apply_da = input$pca_apply_da,
              alpha_da = input$pca_alpha_da,
              width = width,
              height = height,
              colour_list = colour_list)
}

pca_spawn = function(r6, format, output) {
  print_tm(r6$name, "PCA: spawning plot.")
  output$pca_plot = plotly::renderPlotly({
    r6$plots$pca_plot
    plotly::config(r6$plots$pca_plot, toImageButtonOptions = list(format= format,
                                                                  filename= timestamped_name('pca_plot'),
                                                                  height= NULL,
                                                                  width= NULL,
                                                                  scale= 1))
  })
}

pca_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "pca",
                 label = "PCA",
                 dimensions_obj = dimensions_obj,
                 session = session)
}


pca_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "PCA: START.")

  output$pca_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("pca_dataset"),
        label = "Select dataset",
        choices = c('Z-scored table', 'Class table z-scored'),
        selected = r6$params$pca$dataset
      ),
      shiny::selectInput(
        inputId = ns("pca_metacol"),
        label = "Select metadata column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$pca$group_column
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::column(
          width = 6,
          shiny::h4("Discriminant analysis")
        ),
        shiny::column(
          width = 6,
          shinyWidgets::switchInput(inputId = ns("pca_apply_da"),
                                    label = "Apply",
                                    value = r6$params$pca$apply_da,
                                    width = "100%")
        )
      ),

      shiny::sliderInput(inputId = ns("pca_alpha_da"),
                         label = "Alpha",
                         min = 0,
                         max = 0.99,
                         value = r6$params$pca$alpha_da,
                         step = 0.01,
                         width = "100%"),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::fluidRow(
        shiny::selectInput(
          inputId = ns("pca_img_format"),
          label = "Image format",
          choices = c("png", "svg", "jpeg", "webp"),
          selected = r6$params$pca$img_format,
          width = "100%"),
        shiny::downloadButton(
          outputId = ns("download_pca_scores_table"),
          label = "Download scores table",
          style = "width:50%;"
        ),
        shiny::downloadButton(
          outputId = ns("download_pca_loadings_table"),
          label = "Download loadings table",
          style = "width:50%;"
        )
      )

    )
  })
}

pca_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  shiny::observeEvent(c(input$pca_dataset, input$pca_metacol, input$pca_apply_da, input$pca_alpha_da, input$pca_img_format),{
    print_tm(r6$name, "PCA: Updating params...")

    r6$param_pca(dataset = input$pca_dataset,
                 group_column = input$pca_metacol,
                 apply_da = input$pca_apply_da,
                 alpha_da = input$pca_alpha_da,
                 img_format = input$pca_img_format)

    base::tryCatch({
      pca_generate(r6, color_palette, dimensions_obj, input)
      pca_spawn(r6, input$pca_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'PCA: ERROR.')
    },finally={}
    )

  })


  # Download associated tables
  output$download_pca_scores_table = shiny::downloadHandler(
    filename = function(){timestamped_name("pca_scores_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$pca_scores_table, file_name)
    }
  )
  output$download_pca_loadings_table = shiny::downloadHandler(
    filename = function(){timestamped_name("pca_loadings_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$pca_loadings_table, file_name)
    }
  )

  # Expanded boxes
  pca_proxy = plotly::plotlyProxy(outputId = "pca_plot",
                                  session = session)

  shiny::observeEvent(input$pca_plotbox,{
    if (input$pca_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = pca_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = pca_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })

}


#-------------------------------------------------------- Double bonds plot ----
double_bonds_generate_single = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Double bonds plot: generating plot.")

  if (input$double_bonds_plotbox$maximized) {
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$get_dbplot_table_single(data_table = table_switch(input$double_bonds_dataset, r6),
                             dbplot_table = r6$tables$feature_table,
                             col_group = input$double_bonds_metacol,
                             used_function = input$double_bonds_function,
                             group_1 = input$double_bonds_metagroup[1])

  r6$plot_doublebonds_single(lipid_class = input$double_bonds_class,
                             carbon_selection = input$double_bonds_carbon_select,
                             unsat_selection = input$double_bonds_unsat_select,
                             group_1 = input$double_bonds_metagroup[1],
                             width = width,
                             height = height)
}

double_bonds_generate_double = function(r6, colour_list, dimensions_obj, input, session) {
  print_tm(r6$name, "Double bonds plot: generating plot.")
  if (input$double_bonds_plotbox$maximized) {
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$get_dbplot_table_double(data_table = table_switch(input$double_bonds_dataset, r6),
                             dbplot_table = r6$tables$feature_table,
                             col_group = input$double_bonds_metacol,
                             used_function =  input$double_bonds_function,
                             test = input$double_bonds_test,
                             group_1 = input$double_bonds_metagroup[1],
                             group_2 = input$double_bonds_metagroup[2])

  selected_rows = rownames(r6$tables$dbplot_table)[r6$tables$dbplot_table["lipid_class"] == input$double_bonds_class]

  fc_limits = round(max(abs(r6$tables$dbplot_table[selected_rows, "log2_fold_change"])), 1) + 1
  r6$params$db_plot$fc_range = c(-fc_limits, fc_limits)
  if (fc_limits > 1) {
    r6$params$db_plot$fc_values = c(-1, 1)
  } else {
    r6$params$db_plot$fc_values = c(0, 0)
  }

  r6$params$db_plot$pval_range = c(0, round(max(r6$tables$dbplot_table[selected_rows, adjustment_switch(input$double_bonds_plot_adjustment)]), 1) + 1)
  r6$params$db_plot$pval_values = c(0, round(max(r6$tables$dbplot_table[selected_rows, adjustment_switch(input$double_bonds_plot_adjustment)]), 1) + 1)

  shiny::updateSliderInput(
    session = session,
    inputId = "log2_fc_slider",
    min = r6$params$db_plot$fc_range[1],
    max = r6$params$db_plot$fc_range[2],
    value = r6$params$db_plot$fc_values,
  )

  shiny::updateSliderInput(
    session = session,
    inputId = "min_log10_bh_pval_slider",
    max = r6$params$db_plot$pval_range[2],
    value = r6$params$db_plot$pval_values
  )

  r6$plot_doublebonds_double(lipid_class = input$double_bonds_class,
                             carbon_selection = input$double_bonds_carbon_select,
                             unsat_selection = input$double_bonds_unsat_select,
                             adjustment = adjustment_switch(input$double_bonds_plot_adjustment),
                             fc_limits = input$log2_fc_slider,
                             pval_limits = input$min_log10_bh_pval_slider,
                             group_1 = input$double_bonds_metagroup[1],
                             group_2 = input$double_bonds_metagroup[2],
                             width = width,
                             height = height)
}


double_bonds_generate_double_sliders = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Double bonds plot: generating plot.")
  if (input$double_bonds_plotbox$maximized) {
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_doublebonds_double(lipid_class = input$double_bonds_class,
                             carbon_selection = input$double_bonds_carbon_select,
                             unsat_selection = input$double_bonds_unsat_select,
                             adjustment = adjustment_switch(input$double_bonds_plot_adjustment),
                             fc_limits = input$log2_fc_slider,
                             pval_limits = input$min_log10_bh_pval_slider,
                             group_1 = input$double_bonds_metagroup[1],
                             group_2 = input$double_bonds_metagroup[2],
                             width = width,
                             height = height)
}



double_bonds_spawn = function(r6, format, output) {
  print_tm(r6$name, "Double bonds plot: spawning plot.")
  output$double_bonds_plot = plotly::renderPlotly({
    r6$plots$double_bond_plot
    plotly::config(r6$plots$double_bond_plot, toImageButtonOptions = list(format= format,
                                                                          filename= timestamped_name('double_bond_plot'),
                                                                          height= NULL,
                                                                          width= NULL,
                                                                          scale= 1))
  })
}



double_bonds_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "double_bonds",
                 label = "Double bonds plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


double_bonds_server = function(r6, output, session) {

  ns = session$ns
  print_tm(r6$name, "Double bonds plot: START.")

  output$double_bonds_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("double_bonds_dataset"),
        label = "Select data table",
        choices = c("Raw data table", "Class normalized table", "Total normalized table"),
        selected = r6$params$db_plot$dataset
      ),
      shiny::selectInput(
        inputId = ns("double_bonds_metacol"),
        label = "Select group column",
        choices = colnames(r6$tables$raw_meta),
        selected = r6$params$db_plot$group_column
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_metagroup"),
        label = "Select group(s)",
        choices = unique(r6$tables$raw_meta[,r6$params$db_plot$group_column]),
        selected = r6$params$db_plot$selected_groups,
        multiple = TRUE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_class"),
        label = "Select lipid class",
        choices = unique(r6$tables$feature_table$lipid_class),
        selected = r6$params$db_plot$selected_lipid_class,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_carbon_select"),
        label = "Carbon count",
        choices = c('Carbon count (chain 1)', 'Carbon count (chain 2)', 'Carbon count (sum)'),
        selected = r6$params$db_plot$selected_carbon_chain,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_unsat_select"),
        label = "Unsaturation count",
        choices = c('Double bonds (chain 1)', 'Double bonds (chain 2)', 'Double bonds (sum)'),
        selected = r6$params$db_plot$selected_unsat,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_function"),
        label = "Select function",
        choices = c("median", "mean"),
        selected = r6$params$db_plot$selected_function,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_test"),
        label = "Select test",
        choices = c("Wilcoxon", "t-Test"),
        selected = r6$params$db_plot$selected_test,
        multiple = FALSE
      ),
      shiny::selectizeInput(
        inputId = ns("double_bonds_plot_adjustment"),
        label = "Select adjustment",
        choices = c("None", "Benjamini-Hochberg"),
        selected = r6$params$db_plot$adjustment,
        multiple = FALSE
      ),
      shiny::sliderInput(
        inputId = ns("log2_fc_slider"),
        label = "Coloring : Log2(Fold change) (exlude)",
        min = r6$params$db_plot$fc_range[1],
        max = r6$params$db_plot$fc_range[2],
        value = r6$params$db_plot$fc_values,
        step = 0.1
      ),
      shiny::sliderInput(
        inputId = ns("min_log10_bh_pval_slider"),
        label = "Size : -Log10(p-value)",
        min = r6$params$db_plot$pval_range[1],
        max = r6$params$db_plot$pval_range[2],
        value = r6$params$db_plot$pval_values,
        step = 0.1
      ),

      shiny::hr(style = "border-top: 1px solid #7d7d7d;"),
      shiny::selectInput(
        inputId = ns("double_bonds_plot_img_format"),
        label = "Image format",
        choices = c("png", "svg", "jpeg", "webp"),
        selected = r6$params$db_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("download_double_bond_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })
}

db_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Group col selection
  shiny::observeEvent(input$double_bonds_metacol,{
    shiny::updateSelectizeInput(
      inputId = "double_bonds_metagroup",
      session = session,
      choices = unique(r6$tables$raw_meta[,input$double_bonds_metacol]),
      selected = unique(r6$tables$raw_meta[,input$double_bonds_metacol])[c(1,2)]
    )
  })

  # Double bonds plot SINGLE
  shiny::observeEvent(c(shiny::req(length(input$double_bonds_metagroup) == 1), input$double_bonds_class, input$double_bonds_dataset, input$double_bonds_function, input$double_bonds_plot_img_format, input$double_bonds_carbon_select, input$double_bonds_unsat_select), {

    print_tm(r6$name, "Double bonds plot single: Updating params...")

    r6$params$db_plot$dataset = input$double_bonds_dataset
    r6$params$db_plot$group_column = input$double_bonds_metacol
    r6$params$db_plot$selected_groups = input$double_bonds_metagroup
    r6$params$db_plot$selected_lipid_class = input$double_bonds_class
    r6$params$db_plot$selected_carbon_chain = input$double_bonds_carbon_select
    r6$params$db_plot$selected_unsat = input$double_bonds_unsat_select
    r6$params$db_plot$selected_function = input$double_bonds_function
    r6$params$db_plot$img_format = input$double_bonds_plot_img_format


    double_bonds_generate_single(r6, color_palette, dimensions_obj, input)
    double_bonds_spawn(r6, input$double_bonds_plot_img_format, output)

    # base::tryCatch({
    #   double_bonds_generate_single(r6, color_palette, dimensions_obj, input)
    #   double_bonds_spawn(r6, input$double_bonds_plot_img_format, output)
    # },error=function(e){
    #   print_tm(r6$name, 'Double bonds plot: error, missing data.')
    # },finally={}
    # )

  })

  # Double bonds plot DOUBLE : Non-slider events
  shiny::observeEvent(c(input$double_bonds_dataset, input$double_bonds_metagroup, input$double_bonds_class, input$double_bonds_function, input$double_bonds_plot_adjustment, input$double_bonds_test, input$double_bonds_plot_img_format, input$double_bonds_carbon_select, input$double_bonds_unsat_select),{
    shiny::req(length(input$double_bonds_metagroup) == 2)

    print_tm(r6$name, "Double bonds plot non-sliders: Updating params...")
    r6$params$db_plot$dataset = input$double_bonds_dataset
    r6$params$db_plot$group_column = input$double_bonds_metacol
    r6$params$db_plot$selected_groups = input$double_bonds_metagroup
    r6$params$db_plot$selected_lipid_class = input$double_bonds_class
    r6$params$db_plot$selected_carbon_chain = input$double_bonds_carbon_select
    r6$params$db_plot$selected_unsat = input$double_bonds_unsat_select
    r6$params$db_plot$selected_function = input$double_bonds_function
    r6$params$db_plot$adjustment = input$double_bonds_plot_adjustment
    r6$params$db_plot$selected_test = input$double_bonds_test
    r6$params$db_plot$img_format = input$double_bonds_plot_img_format


    double_bonds_generate_double(r6, colour_list, dimensions_obj, input, session)
    double_bonds_spawn(r6, input$double_bonds_plot_img_format, output)

  })

  # Double bonds plot DOUBLE : Slider events
  shiny::observeEvent(c(input$log2_fc_slider, input$min_log10_bh_pval_slider),{
    shiny::req(length(input$double_bonds_metagroup) == 2)

    print_tm(r6$name, "Double bonds plot sliders: Updating params...")
    r6$params$db_plot$fc_values = input$log2_fc_slider
    r6$params$db_plot$pval_values = input$min_log10_bh_pval_slider

    double_bonds_generate_double_sliders(r6, colour_list, dimensions_obj, input)
    double_bonds_spawn(r6, input$double_bonds_plot_img_format, output)
  })

  # Download associated tables
  output$download_double_bond_table = shiny::downloadHandler(
    filename = function(){timestamped_name("double_bond_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$dbplot_table, file_name)
    }
  )

  # Expanded boxes
  double_bonds_proxy = plotly::plotlyProxy(outputId = "double_bonds_plot",
                                           session = session)

  shiny::observeEvent(input$double_bonds_plotbox,{
    if (input$double_bonds_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = double_bonds_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = double_bonds_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}