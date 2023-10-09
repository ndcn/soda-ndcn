#------------------------------------------------------- Clusters heatmap 1 ----
clusters_heatmap_1_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Clusters heatmap 1: generating plot.")

  if (input$clusters_heatmap_1_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_clusters_heatmap(data_table = r6$params$clusters_heatmap_1$data_table,
                           K1 = r6$params$clusters_heatmap_1$K1,
                           sigma = r6$params$clusters_heatmap_1$sigma,
                           K2 = r6$params$clusters_heatmap_1$K2,
                           vertical_annotations = r6$params$clusters_heatmap_1$vertical_annotations,
                           horizontal_annotations = r6$params$clusters_heatmap_1$horizontal_annotations,
                           context = 'clusters_heatmap_1')
}
clusters_heatmap_1_spawn = function(r6, format, output) {
  print_tm(r6$name, "Clusters heatmap 1: spawning plot.")
  output$clusters_heatmap_1_plot = plotly::renderPlotly({
    r6$plots$clusters_heatmap_1
    plotly::config(r6$plots$clusters_heatmap_1, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_name('clusters_heatmap_1'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


clusters_heatmap_1_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "clusters_heatmap_1",
                 label = "Clusters heatmap 1",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


clusters_heatmap_1_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Clusters heatmap 1: START.")
  # Generate UI
  output$clusters_heatmap_1_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('clusters_heatmap_1_data_table'),
        label = 'Data table',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$clusters_heatmap_1$data_table,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('clusters_heatmap_1_K1'),
        label = 'K1',
        value = r6$params$clusters_heatmap_1$K1,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('clusters_heatmap_1_sigma'),
        label = 'Sigma',
        value = r6$params$clusters_heatmap_1$sigma,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('clusters_heatmap_1_K2'),
        label = 'K2',
        value = r6$params$clusters_heatmap_1$K2,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('clusters_heatmap_1_vertical_annotations'),
        label = 'Vertical annotations',
        choices = colnames(r6$tables$metadata),
        selected = r6$params$clusters_heatmap_1$vertical_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('clusters_heatmap_1_horizontal_annotations'),
        label = 'Horizontal annotations',
        choices = colnames(r6$tables$metadata),
        selected = r6$params$clusters_heatmap_1$horizontal_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("clusters_heatmap_1_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$clusters_heatmap_1$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("clusters_heatmap_1_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


clusters_heatmap_1_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$clusters_heatmap_1_data_table, input$clusters_heatmap_1_K1, input$clusters_heatmap_1_sigma, input$clusters_heatmap_1_K2, input$clusters_heatmap_1_vertical_annotations, input$clusters_heatmap_1_horizontal_annotations, input$clusters_heatmap_1_img_format), {
    print_tm(r6$name, "Clusters heatmap 1: Updating params...")

    r6$param_cluster_heatmap(data_table = input$clusters_heatmap_1_data_table,
                             K1 = input$clusters_heatmap_1_K1,
                             sigma = input$clusters_heatmap_1_sigma,
                             K2 = input$clusters_heatmap_1_K2,
                             vertical_annotations = input$clusters_heatmap_1_vertical_annotations,
                             horizontal_annotations = input$clusters_heatmap_1_horizontal_annotations,
                             img_format = input$clusters_heatmap_1_img_format,
                             context = 'clusters_heatmap_1')

    base::tryCatch({
      clusters_heatmap_1_generate(r6, color_palette, dimensions_obj, input)
      clusters_heatmap_1_spawn(r6, input$clusters_heatmap_1_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Clusters heatmap 1: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$clusters_heatmap_1_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("clusters_heatmap_1_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$clusters_heatmap_1, file_name)
    }
  )

  # Expanded boxes
  clusters_heatmap_1_proxy = plotly::plotlyProxy(outputId = "clusters_heatmap_1_plot",
                                                 session = session)

  shiny::observeEvent(input$clusters_heatmap_1_plotbox,{
    if (input$clusters_heatmap_1_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = clusters_heatmap_1_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = clusters_heatmap_1_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}



#------------------------------------------------------- Clusters heatmap 2 ----
clusters_heatmap_2_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Clusters heatmap 2: generating plot.")

  if (input$clusters_heatmap_2_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_clusters_heatmap(data_table = r6$params$clusters_heatmap_2$data_table,
                           K1 = r6$params$clusters_heatmap_2$K1,
                           sigma = r6$params$clusters_heatmap_2$sigma,
                           K2 = r6$params$clusters_heatmap_2$K2,
                           vertical_annotations = r6$params$clusters_heatmap_2$vertical_annotations,
                           horizontal_annotations = r6$params$clusters_heatmap_2$horizontal_annotations,
                           context = 'clusters_heatmap_2')
}
clusters_heatmap_2_spawn = function(r6, format, output) {
  print_tm(r6$name, "Clusters heatmap 2: spawning plot.")
  output$clusters_heatmap_2_plot = plotly::renderPlotly({
    r6$plots$clusters_heatmap_2
    plotly::config(r6$plots$clusters_heatmap_2, toImageButtonOptions = list(format= format,
                                                                            filename= timestamped_name('clusters_heatmap_2'),
                                                                            height= NULL,
                                                                            width= NULL,
                                                                            scale= 1))
  })
}


clusters_heatmap_2_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "clusters_heatmap_2",
                 label = "Clusters heatmap 2",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


clusters_heatmap_2_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Clusters heatmap 2: START.")
  # Generate UI
  output$clusters_heatmap_2_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('clusters_heatmap_2_data_table'),
        label = 'Data table',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$clusters_heatmap_2$data_table,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('clusters_heatmap_2_K1'),
        label = 'K1',
        value = r6$params$clusters_heatmap_2$K1,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('clusters_heatmap_2_sigma'),
        label = 'Sigma',
        value = r6$params$clusters_heatmap_2$sigma,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('clusters_heatmap_2_K2'),
        label = 'K2',
        value = r6$params$clusters_heatmap_2$K2,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('clusters_heatmap_2_vertical_annotations'),
        label = 'Vertical annotations',
        choices = colnames(r6$tables$metadata),
        selected = r6$params$clusters_heatmap_2$vertical_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('clusters_heatmap_2_horizontal_annotations'),
        label = 'Horizontal annotations',
        choices = colnames(r6$tables$metadata),
        selected = r6$params$clusters_heatmap_2$horizontal_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("clusters_heatmap_2_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$clusters_heatmap_2$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("clusters_heatmap_2_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


clusters_heatmap_2_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$clusters_heatmap_2_data_table, input$clusters_heatmap_2_K1, input$clusters_heatmap_2_sigma, input$clusters_heatmap_2_K2, input$clusters_heatmap_2_vertical_annotations, input$clusters_heatmap_2_horizontal_annotations, input$clusters_heatmap_2_img_format), {
    print_tm(r6$name, "Clusters heatmap 2: Updating params...")

    r6$param_cluster_heatmap(data_table = input$clusters_heatmap_2_data_table,
                             K1 = input$clusters_heatmap_2_K1,
                             sigma = input$clusters_heatmap_2_sigma,
                             K2 = input$clusters_heatmap_2_K2,
                             vertical_annotations = input$clusters_heatmap_2_vertical_annotations,
                             horizontal_annotations = input$clusters_heatmap_2_horizontal_annotations,
                             img_format = input$clusters_heatmap_2_img_format,
                             context = 'clusters_heatmap_2')

    base::tryCatch({
      clusters_heatmap_2_generate(r6, color_palette, dimensions_obj, input)
      clusters_heatmap_2_spawn(r6, input$clusters_heatmap_2_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Clusters heatmap 2: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$clusters_heatmap_2_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("clusters_heatmap_2_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$clusters_heatmap_2, file_name)
    }
  )

  # Expanded boxes
  clusters_heatmap_2_proxy = plotly::plotlyProxy(outputId = "clusters_heatmap_2_plot",
                                                 session = session)

  shiny::observeEvent(input$clusters_heatmap_2_plotbox,{
    if (input$clusters_heatmap_2_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = clusters_heatmap_2_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = clusters_heatmap_2_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}




#----------------------------------------------------------- Fusion heatmap ----
fusion_heatmap_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Fusion heatmap: generating plot.")

  if (input$fusion_heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_fusion_heatmap(K1 = r6$params$fusion_heatmap$K1,
                                    sigma = r6$params$fusion_heatmap$sigma,
                                    K2 = r6$params$fusion_heatmap$K2,
                                    Wall = r6$params$fusion_heatmap$Wall,
                                    K3 = r6$params$fusion_heatmap$K3,
                                    t = r6$params$fusion_heatmap$t,
                                    vertical_annotations = r6$params$fusion_heatmap$vertical_annotations,
                                    horizontal_annotations = r6$params$fusion_heatmap$horizontal_annotations)
}
fusion_heatmap_spawn = function(r6, format, output) {
  print_tm(r6$name, "Fusion heatmap: spawning plot.")
  output$fusion_heatmap_plot = plotly::renderPlotly({
    r6$plots$fusion_heatmap
    plotly::config(r6$plots$fusion_heatmap, toImageButtonOptions = list(format= format,
                                                                                   filename= timestamped_name('fusion_heatmap'),
                                                                                   height= NULL,
                                                                                   width= NULL,
                                                                                   scale= 1))
  })
}


fusion_heatmap_ui = function(dimensions_obj, session) {

  get_plotly_box(id = "fusion_heatmap",
                 label = "Fusion heatmap",
                 dimensions_obj = dimensions_obj,
                 session = session)

}


fusion_heatmap_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Fusion heatmap: START.")
  # Generate UI
  output$fusion_heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns('fusion_heatmap_K1'),
        label = 'K1',
        value = r6$params$fusion_heatmap$K1,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('fusion_heatmap_sigma'),
        label = 'Sigma',
        value = r6$params$fusion_heatmap$sigma,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('fusion_heatmap_K2'),
        label = 'K2',
        value = r6$params$fusion_heatmap$K2,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('fusion_heatmap_Wall'),
        label = 'Wall',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$fusion_heatmap$Wall,
        multiple = T,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('fusion_heatmap_K3'),
        label = 'K3',
        value = r6$params$fusion_heatmap$K3,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('fusion_heatmap_t'),
        label = 'T',
        value = r6$params$fusion_heatmap$t,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('fusion_heatmap_vertical_annotations'),
        label = 'Vertical annotations',
        choices = colnames(r6$tables$metadata),
        selected = r6$params$fusion_heatmap$vertical_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('fusion_heatmap_horizontal_annotations'),
        label = 'Horizontal annotations',
        choices = colnames(r6$tables$metadata),
        selected = r6$params$fusion_heatmap$horizontal_annotations,
        multiple = T,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns("fusion_heatmap_img_format"),
        label = 'Img format',
        choices = c('png', 'svg', 'jpeg', 'webp'),
        selected = r6$params$fusion_heatmap$img_format,
        width = "100%"),
      shiny::downloadButton(
        outputId = ns("fusion_heatmap_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


fusion_heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$fusion_heatmap_K1, input$fusion_heatmap_sigma, input$fusion_heatmap_K2, input$fusion_heatmap_Wall, input$fusion_heatmap_K3, input$fusion_heatmap_t, input$fusion_heatmap_vertical_annotations, input$fusion_heatmap_horizontal_annotations, input$fusion_heatmap_img_format), {
    print_tm(r6$name, "Fusion heatmap: Updating params...")

    r6$param_fusion_heatmap(K1 = input$fusion_heatmap_K1,
                                       sigma = input$fusion_heatmap_sigma,
                                       K2 = input$fusion_heatmap_K2,
                                       Wall = input$fusion_heatmap_Wall,
                                       K3 = input$fusion_heatmap_K3,
                                       t = input$fusion_heatmap_t,
                                       vertical_annotations = input$fusion_heatmap_vertical_annotations,
                                       horizontal_annotations = input$fusion_heatmap_horizontal_annotations,
                                       img_format = input$fusion_heatmap_img_format)

    base::tryCatch({
      fusion_heatmap_generate(r6, color_palette, dimensions_obj, input)
      fusion_heatmap_spawn(r6, input$fusion_heatmap_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Fusion heatmap: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$fusion_heatmap_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("fusion_heatmap_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$fusion_heatmap, file_name)
    }
  )

  # Expanded boxes
  fusion_heatmap_proxy = plotly::plotlyProxy(outputId = "fusion_heatmap_plot",
                                                        session = session)

  shiny::observeEvent(input$fusion_heatmap_plotbox,{
    if (input$fusion_heatmap_plotbox$maximized) {
      plotly::plotlyProxyInvoke(p = fusion_heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
                                     height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
                                ))
    } else {
      plotly::plotlyProxyInvoke(p = fusion_heatmap_proxy,
                                method = "relayout",
                                list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                     height = dimensions_obj$ypx * dimensions_obj$y_plot
                                ))
    }
  })
}

#----------------------------------------------------- Similarity Network 1 ----
similarity_network_1_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Similarity Network 1: generating plot.")

  if (input$similarity_network_1_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_similarity_network(data_table = r6$params$similarity_network_1$data_table,
                             K1 = r6$params$similarity_network_1$K1,
                             sigma = r6$params$similarity_network_1$sigma,
                             K2 = r6$params$similarity_network_1$K2,
                             context = 'similarity_network_1')
}
similarity_network_1_spawn = function(r6, format, output) {
  print_tm(r6$name, "Similarity Network 1: spawning plot.")
  output$similarity_network_1_plot = networkD3::renderSimpleNetwork({
    r6$plots$similarity_network_1
  })
}


similarity_network_1_ui = function(dimensions_obj, session) {

  get_networkd3_box(id = "similarity_network_1",
                    label = "Similarity Network 1",
                    dimensions_obj = dimensions_obj,
                    session = session)

}


similarity_network_1_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Similarity Network 1: START.")
  # Generate UI
  output$similarity_network_1_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('similarity_network_1_data_table'),
        label = 'Data table',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$similarity_network_1$data_table,
        multiple = F,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_1_K1'),
        label = 'K1',
        value = r6$params$similarity_network_1$K1,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_1_sigma'),
        label = 'Sigma',
        value = r6$params$similarity_network_1$sigma,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_1_K2'),
        label = 'K2',
        value = r6$params$similarity_network_1$K2,
        width = '100%'
      ),
      shiny::downloadButton(
        outputId = ns("similarity_network_1_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


similarity_network_1_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$similarity_network_1_data_table, input$similarity_network_1_K1, input$similarity_network_1_sigma, input$similarity_network_1_K2), {
    print_tm(r6$name, "Similarity Network 1: Updating params...")

    r6$param_similarity_network(data_table = input$similarity_network_1_data_table,
                                K1 = input$similarity_network_1_K1,
                                sigma = input$similarity_network_1_sigma,
                                K2 = input$similarity_network_1_K2,
                                context = 'similarity_network_1')

    base::tryCatch({
      similarity_network_1_generate(r6, color_palette, dimensions_obj, input)
      similarity_network_1_spawn(r6, input$similarity_network_1_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Similarity Network 1: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$similarity_network_1_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("similarity_network_1_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$similarity_network_1, file_name)
    }
  )

  # Expanded boxes
  similarity_network_1_proxy = plotly::plotlyProxy(outputId = "similarity_network_1_plot",
                                                   session = session)

  # shiny::observeEvent(input$similarity_network_1_plotbox,{
  #   if (input$similarity_network_1_plotbox$maximized) {
  #     plotly::plotlyProxyInvoke(p = similarity_network_1_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
  #                                    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  #                               ))
  #   } else {
  #     plotly::plotlyProxyInvoke(p = similarity_network_1_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
  #                                    height = dimensions_obj$ypx * dimensions_obj$y_plot
  #                               ))
  #   }
  # })
}

#----------------------------------------------------- Similarity Network 2 ----
similarity_network_2_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Similarity Network 2: generating plot.")

  if (input$similarity_network_2_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_similarity_network(data_table = r6$params$similarity_network_2$data_table,
                             K1 = r6$params$similarity_network_2$K1,
                             sigma = r6$params$similarity_network_2$sigma,
                             K2 = r6$params$similarity_network_2$K2,
                             context = 'similarity_network_2')
}
similarity_network_2_spawn = function(r6, format, output) {
  print_tm(r6$name, "Similarity Network 2: spawning plot.")
  output$similarity_network_2_plot = networkD3::renderSimpleNetwork({
    r6$plots$similarity_network_2
  })
}


similarity_network_2_ui = function(dimensions_obj, session) {

  get_networkd3_box(id = "similarity_network_2",
                    label = "Similarity Network 2",
                    dimensions_obj = dimensions_obj,
                    session = session)

}


similarity_network_2_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Similarity Network 2: START.")
  # Generate UI
  output$similarity_network_2_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns('similarity_network_2_data_table'),
        label = 'Data table',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$similarity_network_2$data_table,
        multiple = F,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_2_K1'),
        label = 'K1',
        value = r6$params$similarity_network_2$K1,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_2_sigma'),
        label = 'Sigma',
        value = r6$params$similarity_network_2$sigma,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_2_K2'),
        label = 'K2',
        value = r6$params$similarity_network_2$K2,
        width = '100%'
      ),
      shiny::downloadButton(
        outputId = ns("similarity_network_2_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


similarity_network_2_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$similarity_network_2_data_table, input$similarity_network_2_K1, input$similarity_network_2_sigma, input$similarity_network_2_K2), {
    print_tm(r6$name, "Similarity Network 2: Updating params...")

    r6$param_similarity_network(data_table = input$similarity_network_2_data_table,
                                K1 = input$similarity_network_2_K1,
                                sigma = input$similarity_network_2_sigma,
                                K2 = input$similarity_network_2_K2,
                                context = 'similarity_network_2')

    base::tryCatch({
      similarity_network_2_generate(r6, color_palette, dimensions_obj, input)
      similarity_network_2_spawn(r6, input$similarity_network_2_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Similarity Network 2: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$similarity_network_2_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("similarity_network_2_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$similarity_network_2, file_name)
    }
  )

  # Expanded boxes
  similarity_network_2_proxy = plotly::plotlyProxy(outputId = "similarity_network_2_plot",
                                                   session = session)

  # shiny::observeEvent(input$similarity_network_2_plotbox,{
  #   if (input$similarity_network_2_plotbox$maximized) {
  #     plotly::plotlyProxyInvoke(p = similarity_network_2_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
  #                                    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  #                               ))
  #   } else {
  #     plotly::plotlyProxyInvoke(p = similarity_network_2_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
  #                                    height = dimensions_obj$ypx * dimensions_obj$y_plot
  #                               ))
  #   }
  # })
}

#------------------------------------------------ Similarity Network Fusion ----
similarity_network_fusion_generate = function(r6, colour_list, dimensions_obj, input) {
  print_tm(r6$name, "Similarity Network Fusion: generating plot.")

  if (input$similarity_network_fusion_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_similarity_network_fusion(K1 = r6$params$similarity_network_fusion$K1,
                                    sigma = r6$params$similarity_network_fusion$sigma,
                                    K2 = r6$params$similarity_network_fusion$K2,
                                    Wall = r6$params$similarity_network_fusion$Wall,
                                    K3 = r6$params$similarity_network_fusion$K3,
                                    t = r6$params$similarity_network_fusion$t)
}
similarity_network_fusion_spawn = function(r6, format, output) {
  print_tm(r6$name, "Similarity Network Fusion: spawning plot.")
  output$similarity_network_fusion_plot = networkD3::renderSimpleNetwork({
    r6$plots$similarity_network_fusion
  })
}


similarity_network_fusion_ui = function(dimensions_obj, session) {

  get_networkd3_box(id = "similarity_network_fusion",
                    label = "Similarity Network Fusion",
                    dimensions_obj = dimensions_obj,
                    session = session)

}


similarity_network_fusion_server = function(r6, output, session) {

  ns = session$ns

  print_tm(r6$name, "Similarity Network Fusion: START.")
  # Generate UI
  output$similarity_network_fusion_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::textInput(
        inputId = ns('similarity_network_fusion_K1'),
        label = 'K1',
        value = r6$params$similarity_network_fusion$K1,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_fusion_sigma'),
        label = 'Sigma',
        value = r6$params$similarity_network_fusion$sigma,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_fusion_K2'),
        label = 'K2',
        value = r6$params$similarity_network_fusion$K2,
        width = '100%'
      ),
      shiny::selectInput(
        inputId = ns('similarity_network_fusion_data_table'),
        label = 'Data table',
        choices = names(r6$tables$omics_tables),
        selected = r6$params$similarity_network_fusion$data_table,
        multiple = T,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_fusion_K3'),
        label = 'K3',
        value = r6$params$similarity_network_fusion$K3,
        width = '100%'
      ),
      shiny::textInput(
        inputId = ns('similarity_network_fusion_t'),
        label = 'T',
        value = r6$params$similarity_network_fusion$t,
        width = '100%'
      ),
      shiny::downloadButton(
        outputId = ns("similarity_network_fusion_dl_table"),
        label = "Download associated table",
        style = "width:100%;"
      )
    )
  })

}


similarity_network_fusion_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$similarity_network_fusion_K1, input$similarity_network_fusion_sigma, input$similarity_network_fusion_K2, input$similarity_network_fusion_Wall, input$similarity_network_fusion_K3, input$similarity_network_fusion_t), {
    print_tm(r6$name, "Similarity Network Fusion: Updating params...")

    r6$param_similarity_network_fusion(K1 = input$similarity_network_fusion_K1,
                                       sigma = input$similarity_network_fusion_sigma,
                                       K2 = input$similarity_network_fusion_K2,
                                       Wall = input$similarity_network_fusion_Wall,
                                       K3 = input$similarity_network_fusion_K3,
                                       t = input$similarity_network_fusion_t)

    base::tryCatch({
      similarity_network_fusion_generate(r6, color_palette, dimensions_obj, input)
      similarity_network_fusion_spawn(r6, input$similarity_network_fusion_img_format, output)
    },error=function(e){
      print_tm(r6$name, 'Similarity Network Fusion: ERROR.')
    },finally={}
    )
  })

  # Download associated table
  output$similarity_network_fusion_dl_table = shiny::downloadHandler(
    filename = function(){timestamped_name("similarity_network_fusion_table.csv")},
    content = function(file_name){
      write.csv(r6$tables$similarity_network_fusion, file_name)
    }
  )

  # Expanded boxes
  similarity_network_fusion_proxy = plotly::plotlyProxy(outputId = "similarity_network_fusion_plot",
                                                        session = session)

  # shiny::observeEvent(input$similarity_network_fusion_plotbox,{
  #   if (input$similarity_network_fusion_plotbox$maximized) {
  #     plotly::plotlyProxyInvoke(p = similarity_network_fusion_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full,
  #                                    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  #                               ))
  #   } else {
  #     plotly::plotlyProxyInvoke(p = similarity_network_fusion_proxy,
  #                               method = "relayout",
  #                               list(width = dimensions_obj$xpx * dimensions_obj$x_plot,
  #                                    height = dimensions_obj$ypx * dimensions_obj$y_plot
  #                               ))
  #   }
  # })
}
