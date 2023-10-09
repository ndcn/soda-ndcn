#------------------------------------------------------- Explained variance ----
explained_variance_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Explained variance: generating plot.")

  if (input$explained_variance_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_explained_variance(selected_plot = r6$params$explained_variance$selected_plot,
                             width = width,
                             height = height)
}
explained_variance_spawn = function(r6, format, output) {
  print_tm(r6$name, "Explained variance: spawning plot.")
  output$explained_variance_plot = plotly::renderPlotly({
    r6$plots$explained_variance
    plotly::config(r6$plots$explained_variance, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_name('explained_variance'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


explained_variance_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "explained_variance",
                 label = "Explained variance",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


explained_variance_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Explained variance: START.")
  # Generate UI
  output$explained_variance_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("explained_variance_selected_plot"),
        label = 'Selected plot',
        choices = c('1', '2'),
        selected = r6$params$explained_variance$selected_plot,
        width = "100%"),
      shiny::selectInput(
        inputId = ns("explained_variance_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$explained_variance$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("explained_variance_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


explained_variance_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$explained_variance_selected_plot, input$explained_variance_img_format), {
    print_tm(r6$name, "Explained variance: Updating params...")

    r6$param_explained_variance(selected_plot = input$explained_variance_selected_plot,
                                img_format = input$explained_variance_img_format)

    base::tryCatch({
      explained_variance_generate(r6, color_palette, dimensions_obj, input)
      explained_variance_spawn(r6, input$explained_variance_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Explained variance: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$explained_variance_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("explained_variance_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$explained_variance, file_name)
    }
  )

  # Expanded boxes
  explained_variance_proxy = plotly::plotlyProxy(outputId = "explained_variance_plot",
                                                 session = session)

  shiny::observeEvent(input$explained_variance_plotbox,{
    if (input$explained_variance_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = explained_variance_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = explained_variance_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



#-------------------------------------------------------------- Factor plot ----
factor_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Factor plot: generating plot.")

  if (input$factor_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_factor_plot(factors = r6$params$factor_plot$factors,
                      color_by = r6$params$factor_plot$color_by,
                      scale = r6$params$factor_plot$scale,
                      dodge = r6$params$factor_plot$dodge,
                      add_violin = r6$params$factor_plot$add_violin,
                      width = width,
                      height = height)
}
factor_plot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Factor plot: spawning plot.")
  output$factor_plot_plot = plotly::renderPlotly({
    r6$plots$factor_plot
    plotly::config(r6$plots$factor_plot, toImageButtonOptions = list(format= format,
                                                                     filename= timestamped_name('factor_plot'),
                                                                     height= NULL,
                                                                     width= NULL,
                                                                     scale= 1))
  })
}


factor_plot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "factor_plot",
                 label = "Factor plot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


factor_plot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Factor plot: START.")
  # Generate UI
  output$factor_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('factor_plot_factors'),
        label = 'Factors',
        choices = r6$params$factor_list,
        selected = r6$params$factor_plot$factors,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('factor_plot_color_by'),
        label = 'Color by',
        choices = rownames(r6$tables$metadata),
        selected = r6$params$factor_plot$color_by,
        multiple = F,
        width = '100%'
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("factor_plot_scale"),
        label = 'Scale',
        value = r6$params$factor_plot$scale,
        fill = TRUE,
        status = "primary"
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("factor_plot_dodge"),
        label = 'Dodge',
        value = r6$params$factor_plot$dodge,
        fill = TRUE,
        status = "primary"
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("factor_plot_add_violin"),
        label = 'Add violin',
        value = r6$params$factor_plot$add_violin,
        fill = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("factor_plot_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$factor_plot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("factor_plot_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


factor_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$factor_plot_factors, input$factor_plot_color_by, input$factor_plot_scale, input$factor_plot_dodge, input$factor_plot_add_violin, input$factor_plot_img_format), {
    print_tm(r6$name, "Factor plot: Updating params...")

    r6$param_factor_plot(factors = input$factor_plot_factors,
                         color_by = input$factor_plot_color_by,
                         scale = input$factor_plot_scale,
                         dodge = input$factor_plot_dodge,
                         add_violin = input$factor_plot_add_violin,
                         img_format = input$factor_plot_img_format)

    base::tryCatch({
      factor_plot_generate(r6, color_palette, dimensions_obj, input)
      factor_plot_spawn(r6, input$factor_plot_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Factor plot: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$factor_plot_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("factor_plot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$factor_plot, file_name)
    }
  )

  # Expanded boxes
  factor_plot_proxy = plotly::plotlyProxy(outputId = "factor_plot_plot",
                                          session = session)

  shiny::observeEvent(input$factor_plot_plotbox,{
    if (input$factor_plot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = factor_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = factor_plot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



#--------------------------------------------------------- Combined factors ----
combined_factors_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Combined factors: generating plot.")

  if (input$combined_factors_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_combined_factors(factors = r6$params$combined_factors$factors,
                           scale = r6$params$combined_factors$scale,
                           color_by = r6$params$combined_factors$color_by,
                           width = width,
                           height = height)
}
combined_factors_spawn = function(r6, format, output) {
  print_tm(r6$name, "Combined factors: spawning plot.")
  output$combined_factors_plot = plotly::renderPlotly({
    r6$plots$combined_factors
    plotly::config(r6$plots$combined_factors, toImageButtonOptions = list(format= format,
                                                                          filename= timestamped_name('combined_factors'),
                                                                          height= NULL,
                                                                          width= NULL,
                                                                          scale= 1))
  })
}


combined_factors_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "combined_factors",
                 label = "Combined factors",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


combined_factors_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Combined factors: START.")
  # Generate UI
  output$combined_factors_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('combined_factors_factors'),
        label = 'Factors',
        choices = r6$params$factor_list,
        selected = r6$params$combined_factors$factors,
        multiple = T,
        width = '100%'
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("combined_factors_scale"),
        label = 'Scale',
        value = r6$params$combined_factors$scale,
        fill = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('combined_factors_color_by'),
        label = 'Color by',
        choices = rownames(r6$tables$metadata),
        selected = r6$params$combined_factors$color_by,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("combined_factors_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$combined_factors$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("combined_factors_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


combined_factors_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$combined_factors_factors, input$combined_factors_scale, input$combined_factors_color_by, input$combined_factors_img_format), {
    print_tm(r6$name, "Combined factors: Updating params...")

    r6$param_combined_factors(factors = input$combined_factors_factors,
                              scale = input$combined_factors_scale,
                              color_by = input$combined_factors_color_by,
                              img_format = input$combined_factors_img_format)

    base::tryCatch({
      combined_factors_generate(r6, color_palette, dimensions_obj, input)
      combined_factors_spawn(r6, input$combined_factors_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Combined factors: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$combined_factors_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("combined_factors_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$combined_factors, file_name)
    }
  )

  # Expanded boxes
  combined_factors_proxy = plotly::plotlyProxy(outputId = "combined_factors_plot",
                                               session = session)

  shiny::observeEvent(input$combined_factors_plotbox,{
    if (input$combined_factors_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = combined_factors_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = combined_factors_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}

#---------------------------------------------------------- Feature weights ----
feature_weights_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Feature weights: generating plot.")

  if (input$feature_weights_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_feature_weights(views = r6$params$feature_weights$views,
                          factors = r6$params$feature_weights$factors,
                          scale = r6$params$feature_weights$scale,
                          abs = r6$params$feature_weights$abs,
                          width = width,
                          height = height)
}
feature_weights_spawn = function(r6, format, output) {
  print_tm(r6$name, "Feature weights: spawning plot.")
  output$feature_weights_plot = plotly::renderPlotly({
    r6$plots$feature_weights
    plotly::config(r6$plots$feature_weights, toImageButtonOptions = list(format= format,
                                                                         filename= timestamped_name('feature_weights'),
                                                                         height= NULL,
                                                                         width= NULL,
                                                                         scale= 1))
  })
}


feature_weights_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "feature_weights",
                 label = "Feature weights",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


feature_weights_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Feature weights: START.")
  # Generate UI
  output$feature_weights_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('feature_weights_views'),
        label = 'Views',
        choices = r6$params$views,
        selected = r6$params$feature_weights$views,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('feature_weights_factors'),
        label = 'Factors',
        choices = r6$params$factor_list,
        selected = r6$params$feature_weights$factors,
        multiple = F,
        width = '100%'
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("feature_weights_scale"),
        label = 'Scale',
        value = r6$params$feature_weights$scale,
        fill = TRUE,
        status = "primary"
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("feature_weights_abs"),
        label = 'Abs',
        value = r6$params$feature_weights$abs,
        fill = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns("feature_weights_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$feature_weights$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("feature_weights_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


feature_weights_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$feature_weights_views, input$feature_weights_factors, input$feature_weights_scale, input$feature_weights_abs, input$feature_weights_img_format), {
    print_tm(r6$name, "Feature weights: Updating params...")

    r6$param_feature_weights(views = input$feature_weights_views,
                             factors = input$feature_weights_factors,
                             scale = input$feature_weights_scale,
                             abs = input$feature_weights_abs,
                             img_format = input$feature_weights_img_format)


    base::tryCatch({
      feature_weights_generate(r6, color_palette, dimensions_obj, input)
      base::suppressMessages(feature_weights_spawn(r6, input$feature_weights_img_format, output))

    },error=function(e){
      print_tm(r6$name, 'Feature weights: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$feature_weights_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("feature_weights_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$feature_weights, file_name)
    }
  )

  # Expanded boxes
  feature_weights_proxy = plotly::plotlyProxy(outputId = "feature_weights_plot",
                                              session = session)

  shiny::observeEvent(input$feature_weights_plotbox,{
    if (input$feature_weights_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = feature_weights_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = feature_weights_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



#------------------------------------------------------ Feature top weights ----

feature_top_weights_generate = function(r6, colour_list, dimensions_obj, input) {
  print_t("MOFA Feature top weights: generating plot.")

  if (input$feature_top_weights_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_feature_top_weights(view = as.numeric(input$feature_top_weights_views),
                          factor = as.numeric(input$feature_top_weights_factor),
                          nfeatures = as.numeric(input$feature_top_weights_feature_count))

}


feature_top_weights_spawn = function(r6, format, output) {
  print_t("MOFA Feature top weights: spawning plot.")
  output$feature_top_weights_plot = shiny::renderPlot({
    r6$plots$feature_top_weights
  })
}


feature_top_weights_ui = function(dimensions_obj, session) {

  get_plot_box(id = "feature_top_weights",
               label = "Feature top weights",
               dimensions_obj = dimensions_obj,
               session = session)

}


feature_top_weights_server = function(r6, output, session) {

  ns = session$ns

  print_t("MOFA Feature top weights : START.")
  # Generate UI
  output$feature_top_weights_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("feature_top_weights_views"),
        label = "Select view",
        choices = r6$params$views,
        selected = r6$params$feature_top_weights$views,
        multiple = F
      ),

      shiny::selectInput(
        inputId = ns("feature_top_weights_factor"),
        label = "Select factor",
        choices = r6$params$factor_list,
        selected = r6$params$feature_top_weights$factor
      ),

      shiny::textInput(
        inputId = ns("feature_top_weights_feature_count"),
        label = "Number of features",
        value = r6$params$feature_top_weights$nfeatures
      )
    )
  })
}

feature_top_weights_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$feature_top_weights_views, input$feature_top_weights_factor, input$feature_top_weights_feature_count), {
    print_t("MOFA Feature top weights: Updating params...")
    r6$params$feature_top_weights$views = input$feature_top_weights_views
    r6$params$feature_top_weights$factor = input$feature_top_weights_factor
    r6$params$feature_top_weights$nfeatures = input$feature_top_weights_feature_count

    base::tryCatch({
      feature_top_weights_generate(r6, color_palette, dimensions_obj, input)
      feature_top_weights_spawn(r6, input$feature_top_weights_img_format, output)
    },error=function(e){
      print_t('MOFA Feature top weights: error, missing data.')
    },finally={}
    )
  })

}


#------------------------------------------------------------- Mofa heatmap ----
mofa_heatmap_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Mofa heatmap: generating plot.")

  if (input$mofa_heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_mofa_heatmap(factor = r6$params$mofa_heatmap$factor,
                       view = r6$params$mofa_heatmap$view,
                       features = r6$params$mofa_heatmap$features,
                       imputed = r6$params$mofa_heatmap$imputed,
                       denoise = r6$params$mofa_heatmap$denoise,
                       cluster_cols = r6$params$mofa_heatmap$cluster_cols,
                       cluster_rows = r6$params$mofa_heatmap$cluster_rows,
                       row_annotations = r6$params$mofa_heatmap$row_annotations,
                       max_value = r6$params$mofa_heatmap$max_value,
                       min_value = r6$params$mofa_heatmap$min_value,
                       width = width,
                       height = height)
}
mofa_heatmap_spawn = function(r6, format, output) {
  print_tm(r6$name, "Mofa heatmap: spawning plot.")
  output$mofa_heatmap_plot = plotly::renderPlotly({
    r6$plots$mofa_heatmap
    plotly::config(r6$plots$mofa_heatmap, toImageButtonOptions = list(format= format,
                                                                      filename= timestamped_name('mofa_heatmap'),
                                                                      height= NULL,
                                                                      width= NULL,
                                                                      scale= 1))
  })
}


mofa_heatmap_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "mofa_heatmap",
                 label = "Mofa heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


mofa_heatmap_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Mofa heatmap: START.")
  # Generate UI
  output$mofa_heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('mofa_heatmap_factor'),
        label = 'Factor',
        choices = r6$params$factor_list,
        selected = r6$params$mofa_heatmap$factor,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('mofa_heatmap_view'),
        label = 'View',
        choices = r6$params$views,
        selected = r6$params$mofa_heatmap$view,
        multiple = F,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('mofa_heatmap_features'),
        label = 'Features',
        value = r6$params$mofa_heatmap$features,
        width = '100%'
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("mofa_heatmap_imputed"),
        label = 'Imputed',
        value = r6$params$mofa_heatmap$imputed,
        fill = TRUE,
        status = "primary"
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("mofa_heatmap_denoise"),
        label = 'Denoise',
        value = r6$params$mofa_heatmap$denoise,
        fill = TRUE,
        status = "primary"
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("mofa_heatmap_cluster_cols"),
        label = 'Cluster cols',
        value = r6$params$mofa_heatmap$cluster_cols,
        fill = TRUE,
        status = "primary"
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("mofa_heatmap_cluster_rows"),
        label = 'Cluster rows',
        value = r6$params$mofa_heatmap$cluster_rows,
        fill = TRUE,
        status = "primary"
      ),
      shiny::selectizeInput(
        inputId = ns("mofa_heatmap_map_rows"),
        label = "Map sample data",
        multiple = TRUE,
        choices = colnames(t(r6$tables$metadata)),
        selected = r6$params$mofa_heatmap$row_annotations
      ),
      shiny::textInput(
        inputId = ns('mofa_heatmap_max_value'),
        label = 'Max value',
        value = r6$params$mofa_heatmap$max_value,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('mofa_heatmap_min_value'),
        label = 'Min value',
        value = r6$params$mofa_heatmap$min_value,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("mofa_heatmap_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$mofa_heatmap$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("mofa_heatmap_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


mofa_heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$mofa_heatmap_factor, input$mofa_heatmap_view, input$mofa_heatmap_features, input$mofa_heatmap_imputed, input$mofa_heatmap_denoise, input$mofa_heatmap_cluster_cols, input$mofa_heatmap_cluster_rows, input$mofa_heatmap_map_rows, input$mofa_heatmap_max_value, input$mofa_heatmap_min_value, input$mofa_heatmap_img_format), {
    print_tm(r6$name, "Mofa heatmap: Updating params...")

    r6$param_mofa_heatmap(factor = input$mofa_heatmap_factor,
                          view = input$mofa_heatmap_view,
                          features = input$mofa_heatmap_features,
                          imputed = input$mofa_heatmap_imputed,
                          denoise = input$mofa_heatmap_denoise,
                          cluster_cols = input$mofa_heatmap_cluster_cols,
                          cluster_rows = input$mofa_heatmap_cluster_rows,
                          row_annotations = input$mofa_heatmap_map_rows,
                          max_value = input$mofa_heatmap_max_value,
                          min_value = input$mofa_heatmap_min_value,
                          img_format = input$mofa_heatmap_img_format)

    base::tryCatch({
      mofa_heatmap_generate(r6, color_palette, dimensions_obj, input)
      mofa_heatmap_spawn(r6, input$mofa_heatmap_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Mofa heatmap: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$mofa_heatmap_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("mofa_heatmap_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$mofa_heatmap, file_name)
    }
  )

  # Expanded boxes
  mofa_heatmap_proxy = plotly::plotlyProxy(outputId = "mofa_heatmap_plot",
                                           session = session)

  shiny::observeEvent(input$mofa_heatmap_plotbox,{
    if (input$mofa_heatmap_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = mofa_heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = mofa_heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}


#-------------------------------------------------------------- Scatterplot ----
scatterplot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Scatterplot: generating plot.")

  if (input$scatterplot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_scatterplot(view = r6$params$scatterplot$view,
                      factor = r6$params$scatterplot$factor,
                      features = r6$params$scatterplot$features,
                      add_lm = r6$params$scatterplot$add_lm,
                      color_by = r6$params$scatterplot$color_by,
                      width = width,
                      height = height)
}
scatterplot_spawn = function(r6, format, output) {
  print_tm(r6$name, "Scatterplot: spawning plot.")
  output$scatterplot_plot = plotly::renderPlotly({
    r6$plots$scatterplot
    plotly::config(r6$plots$scatterplot, toImageButtonOptions = list(format= format,
                                                                     filename= timestamped_name('scatterplot'),
                                                                     height= NULL,
                                                                     width= NULL,
                                                                     scale= 1))
  })
}


scatterplot_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "scatterplot",
                 label = "Scatterplot",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


scatterplot_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Scatterplot: START.")
  # Generate UI
  output$scatterplot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('scatterplot_view'),
        label = 'View',
        choices = r6$params$views,
        selected = r6$params$scatterplot$view,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('scatterplot_factor'),
        label = 'Factor',
        choices = r6$params$factor_list,
        selected = r6$params$scatterplot$factor,
        multiple = F,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('scatterplot_features'),
        label = 'Features',
        value = r6$params$scatterplot$features,
        width = '100%'
      ),
      shinyWidgets::prettySwitch(
        inputId = ns("scatterplot_add_lm"),
        label = 'Add lm',
        value = r6$params$scatterplot$add_lm,
        fill = TRUE,
        status = "primary"
      ),
      shiny::selectInput(
        inputId = ns('scatterplot_color_by'),
        label = 'Color by',
        choices = rownames(r6$tables$metadata),
        selected = r6$params$scatterplot$color_by,
        multiple = F,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("scatterplot_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$scatterplot$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("scatterplot_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


scatterplot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$scatterplot_view, input$scatterplot_factor, input$scatterplot_features, input$scatterplot_add_lm, input$scatterplot_color_by, input$scatterplot_img_format), {
    print_tm(r6$name, "Scatterplot: Updating params...")

    r6$param_scatterplot(view = input$scatterplot_view,
                         factor = input$scatterplot_factor,
                         features = input$scatterplot_features,
                         add_lm = input$scatterplot_add_lm,
                         color_by = input$scatterplot_color_by,
                         img_format = input$scatterplot_img_format)

    base::tryCatch({
      scatterplot_generate(r6, color_palette, dimensions_obj, input)
      scatterplot_spawn(r6, input$scatterplot_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Scatterplot: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$scatterplot_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("scatterplot_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$scatterplot, file_name)
    }
  )

  # Expanded boxes
  scatterplot_proxy = plotly::plotlyProxy(outputId = "scatterplot_plot",
                                          session = session)

  shiny::observeEvent(input$scatterplot_plotbox,{
    if (input$scatterplot_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = scatterplot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = scatterplot_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}




