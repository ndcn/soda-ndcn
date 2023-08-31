#------------------------------------------------------- Explained variance ----

explained_variance_generate = function(r6, colour_list, dimensions_obj, input) {
  print_t("MOFA Explained variance: generating plot.")

  if (input$explained_variance_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_explained_variance(selected_plot = as.numeric(input$explained_variance_selected_plot))
}


explained_variance_spawn = function(r6, format, output) {
  print_t("MOFA Explained variance: spawning plot.")
  output$explained_variance_plot = shiny::renderPlot({
    r6$plots$explained_variance
  })
}


explained_variance_ui = function(dimensions_obj, session) {

  get_plot_box(id = "explained_variance",
               label = "Explained variance",
               dimensions_obj = dimensions_obj,
               session = session)

}


explained_variance_server = function(r6, output, session) {

  ns = session$ns

  print_t("MOFA Explained variance : START.")
  # Generate UI
  output$explained_variance_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("explained_variance_selected_plot"),
        label = "Selected plot",
        choices = c(1, 2),
        selected = r6$params$explained_variance$selected_plot
      )
    )
  })
}

explained_variance_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(input$explained_variance_selected_plot, {
    print_t("MOFA Explained variance: Updating params...")
    r6$params$explained_variance$selected_plot = input$explained_variance_selected_plot

    base::tryCatch({
      explained_variance_generate(r6, color_palette, dimensions_obj, input)
      explained_variance_spawn(r6, input$explained_variance_img_format, output)
    },error=function(e){
      print_t('MOFA Explained variance: error, missing data.')
    },finally={}
    )
  })

}

#-------------------------------------------------------------- Factor plot ----

factor_plot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_t("MOFA Factor plot: generating plot.")

  if (input$factor_plot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  color_by = input$factor_plot_color_by
  shape_by = input$factor_plot_shape_by

  if (color_by == "None") {color_by = NULL}
  if (shape_by == "None") {shape_by = NULL}

  r6$plot_factor_plot(factors = as.numeric(input$factor_plot_factors),
                      color_by = color_by,
                      shape_by = shape_by,
                      dodge = input$factor_plot_dodge,
                      add_violin = input$factor_plot_violin,
                      violin_alpha = 0.25,
                      color_palette = colour_list)

}


factor_plot_spawn = function(r6, format, output) {
  print_t("MOFA Factor plot: spawning plot.")
  output$factor_plot_plot = shiny::renderPlot({
    r6$plots$factor_plot
  })
}


factor_plot_ui = function(dimensions_obj, session) {

  get_plot_box(id = "factor_plot",
               label = "Factor plot",
               dimensions_obj = dimensions_obj,
               session = session)

}


factor_plot_server = function(r6, output, session) {

  ns = session$ns

  print_t("MOFA Factor plot : START.")
  # Generate UI
  output$factor_plot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("factor_plot_factors"),
        label = "Select factors",
        choices = r6$params$factor_list,
        selected = r6$params$factor_plot$factors,
        multiple = T
      ),

      shiny::selectInput(
        inputId = ns("factor_plot_color_by"),
        label = "Color by",
        choices = c("None", r6$params$sample_metadata),
        selected = r6$params$factor_plot$color_by
      ),

      shiny::selectInput(
        inputId = ns("factor_plot_shape_by"),
        label = "Shape by",
        choices = c("None", r6$params$sample_metadata),
        selected = r6$params$factor_plot$shape_by
      ),

      shinyWidgets::prettySwitch(inputId = ns("factor_plot_violin"),
                                 label = "Add violin",
                                 value = r6$params$factor_plot$add_violin,
                                 fill = TRUE,
                                 status = "primary"),

      shinyWidgets::prettySwitch(inputId = ns("factor_plot_dodge"),
                                 label = "Doge",
                                 value = r6$params$factor_plot$dodge,
                                 fill = TRUE,
                                 status = "primary")



    )
  })
}

factor_plot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$factor_plot_factors, input$factor_plot_color_by, input$factor_plot_shape_by, input$factor_plot_dodge, input$factor_plot_violin), {
    print_t("MOFA Factor plot: Updating params...")
    r6$params$factor_plot$factors = input$factor_plot_factors
    r6$params$factor_plot$color_by = input$factor_plot_color_by
    r6$params$factor_plot$shape_by = input$factor_plot_shape_by
    r6$params$factor_plot$dodge = input$factor_plot_dodge
    r6$params$factor_plot$add_violin = input$factor_plot_violin




    base::tryCatch({
      factor_plot_generate(r6, color_palette, dimensions_obj, input)
      factor_plot_spawn(r6, input$factor_plot_img_format, output)
    },error=function(e){
      print_t('MOFA Factor plot: error, missing data.')
    },finally={}
    )
  })

}


#---------------------------------------------------------- Feature weights ----

feature_weights_generate = function(r6, colour_list, dimensions_obj, input) {
  print_t("MOFA Feature weights: generating plot.")

  if (input$feature_weights_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  r6$plot_feature_weights(view = as.numeric(input$feature_weights_views),
                          factor = as.numeric(input$feature_weights_factor),
                          nfeatures = as.numeric(input$feature_weights_feature_count),
                          scale = input$feature_weights_scale,
                          abs = input$feature_weights_abs)

}


feature_weights_spawn = function(r6, format, output) {
  print_t("MOFA Feature weights: spawning plot.")
  output$feature_weights_plot = shiny::renderPlot({
    r6$plots$feature_weights
  })
}


feature_weights_ui = function(dimensions_obj, session) {

  get_plot_box(id = "feature_weights",
               label = "Feature weights",
               dimensions_obj = dimensions_obj,
               session = session)

}


feature_weights_server = function(r6, output, session) {

  ns = session$ns

  print_t("MOFA Feature weights : START.")
  # Generate UI
  output$feature_weights_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("feature_weights_views"),
        label = "Select view",
        choices = r6$params$views,
        selected = r6$params$feature_weights$views,
        multiple = F
      ),

      shiny::selectInput(
        inputId = ns("feature_weights_factor"),
        label = "Select factor",
        choices = r6$params$factor_list,
        selected = r6$params$feature_weights$factor
      ),

      shiny::textInput(
        inputId = ns("feature_weights_feature_count"),
        label = "Number of features",
        value = r6$params$feature_weights$nfeatures
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
        label = 'Absolute value',
        value = r6$params$feature_weights$abs,
        fill = TRUE,
        status = "primary"
      )

    )
  })
}

feature_weights_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$feature_weights_views, input$feature_weights_factor, input$feature_weights_feature_count, input$feature_weights_scale, input$feature_weights_abs), {
    print_t("MOFA Feature weights: Updating params...")
    r6$params$feature_weights$views = input$feature_weights_views
    r6$params$feature_weights$factor = input$feature_weights_factor
    r6$params$feature_weights$nfeatures = input$feature_weights_feature_count
    r6$params$feature_weights$scale = input$feature_weights_scale
    r6$params$feature_weights$abs = input$feature_weights_abs

    base::tryCatch({
      feature_weights_generate(r6, color_palette, dimensions_obj, input)
      feature_weights_spawn(r6, input$feature_weights_img_format, output)
    },error=function(e){
      print_t('MOFA Feature weights: error, missing data.')
    },finally={}
    )
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


#------------------------------------------------------------- MOFA heatmap ----

mofa_heatmap_generate = function(r6, colour_list, dimensions_obj, input) {
  print_t("MOFA MOFA heatmap: generating plot.")

  if (input$mofa_heatmap_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }
  r6$plot_mofa_heatmap(view = as.numeric(input$mofa_heatmap_views),
                       factor = as.numeric(input$mofa_heatmap_factor),
                       features = as.numeric(input$mofa_heatmap_feature_count),
                       cluster_rows = input$mofa_heatmap_cluster_rows,
                       cluster_cols = input$mofa_heatmap_cluster_cols,
                       show_rownames = input$mofa_heatmap_show_rownames,
                       show_colnames = input$mofa_heatmap_show_colnames)

}


mofa_heatmap_spawn = function(r6, format, output) {
  print_t("MOFA MOFA heatmap: spawning plot.")
  output$mofa_heatmap_plot = shiny::renderPlot({
    r6$plots$mofa_heatmap
  })
}


mofa_heatmap_ui = function(dimensions_obj, session) {

  get_plot_box(id = "mofa_heatmap",
               label = "MOFA heatmap",
               dimensions_obj = dimensions_obj,
               session = session)

}


mofa_heatmap_server = function(r6, output, session) {

  ns = session$ns

  print_t("MOFA MOFA heatmap : START.")
  # Generate UI
  output$mofa_heatmap_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("mofa_heatmap_views"),
        label = "Select view",
        choices = r6$params$views,
        selected = r6$params$mofa_heatmap$views,
        multiple = F
      ),

      shiny::selectInput(
        inputId = ns("mofa_heatmap_factor"),
        label = "Select factor",
        choices = r6$params$factor_list,
        selected = r6$params$mofa_heatmap$factor
      ),

      shiny::textInput(
        inputId = ns("mofa_heatmap_feature_count"),
        label = "Number of features",
        value = r6$params$mofa_heatmap$nfeatures
      ),

      shinyWidgets::prettySwitch(
        inputId = ns('mofa_heatmap_cluster_rows'),
        label = 'Cluster rows',
        value = r6$params$mofa_heatmap$cluster_rows,
        fill = TRUE,
        status = "primary"
      ),

      shinyWidgets::prettySwitch(
        inputId = ns('mofa_heatmap_cluster_cols'),
        label = 'Cluster columns',
        value = r6$params$mofa_heatmap$cluster_cols,
        fill = TRUE,
        status = "primary"
      ),

      shinyWidgets::prettySwitch(
        inputId = ns('mofa_heatmap_show_rownames'),
        label = 'Show rownames',
        value = r6$params$mofa_heatmap$show_rownames,
        fill = TRUE,
        status = "primary"
      ),

      shinyWidgets::prettySwitch(
        inputId = ns('mofa_heatmap_show_colnames'),
        label = 'Show colnames',
        value = r6$params$mofa_heatmap$show_colnames,
        fill = TRUE,
        status = "primary"
      )

    )
  })
}

mofa_heatmap_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$mofa_heatmap_views, input$mofa_heatmap_factor, input$mofa_heatmap_feature_count, input$mofa_heatmap_cluster_rows, input$mofa_heatmap_cluster_cols, input$mofa_heatmap_show_rownames, input$mofa_heatmap_show_colnames), {
    print_t("MOFA MOFA heatmap: Updating params...")
    r6$params$mofa_heatmap$views = input$mofa_heatmap_views
    r6$params$mofa_heatmap$factor = input$mofa_heatmap_factor
    r6$params$mofa_heatmap$nfeatures = input$mofa_heatmap_feature_count
    r6$params$mofa_heatmap$cluster_rows = input$mofa_heatmap_cluster_rows
    r6$params$mofa_heatmap$cluster_cols = input$mofa_heatmap_cluster_cols
    r6$params$mofa_heatmap$show_rownames = input$mofa_heatmap_show_rownames
    r6$params$mofa_heatmap$show_colnames = input$mofa_heatmap_show_colnames

    base::tryCatch({
      mofa_heatmap_generate(r6, color_palette, dimensions_obj, input)
      mofa_heatmap_spawn(r6, input$mofa_heatmap_img_format, output)
    },error=function(e){
      print_t('MOFA MOFA heatmap: error, missing data.')
    },finally={}
    )
  })

}

#-------------------------------------------------------------- Scatterplot ----

scatterplot_generate = function(r6, colour_list, dimensions_obj, input) {
  print_t("MOFA Scatterplot: generating plot.")

  if (input$scatterplot_plotbox$maximized){
    width = dimensions_obj$xpx_total * dimensions_obj$x_plot_full
    height = dimensions_obj$ypx_total * dimensions_obj$y_plot_full
  } else {
    width = dimensions_obj$xpx * dimensions_obj$x_plot
    height = dimensions_obj$ypx * dimensions_obj$y_plot
  }

  color_by = input$scatterplot_color_by
  if (color_by == "None") {color_by = NULL}

  r6$plot_scatterplot(view = as.numeric(input$scatterplot_views),
                      factor = as.numeric(input$scatterplot_factor),
                      features = as.numeric(input$scatterplot_feature_count),
                      add_lm = input$scatterplot_add_lm,
                      color_by = color_by)

}


scatterplot_spawn = function(r6, format, output) {
  print_t("MOFA Scatterplot: spawning plot.")
  output$scatterplot_plot = shiny::renderPlot({
    r6$plots$scatterplot
  })
}


scatterplot_ui = function(dimensions_obj, session) {

  get_plot_box(id = "scatterplot",
               label = "Scatterplot",
               dimensions_obj = dimensions_obj,
               session = session)

}


scatterplot_server = function(r6, output, session) {

  ns = session$ns

  print_t("MOFA Scatterplot : START.")
  # Generate UI
  output$scatterplot_sidebar_ui = shiny::renderUI({
    shiny::tagList(
      shiny::selectInput(
        inputId = ns("scatterplot_views"),
        label = "Select view",
        choices = r6$params$views,
        selected = r6$params$scatterplot$views,
        multiple = F
      ),

      shiny::selectInput(
        inputId = ns("scatterplot_factor"),
        label = "Select factor",
        choices = r6$params$factor_list,
        selected = r6$params$scatterplot$factor
      ),

      shiny::textInput(
        inputId = ns("scatterplot_feature_count"),
        label = "Number of features",
        value = r6$params$scatterplot$nfeatures
      ),

      shinyWidgets::prettySwitch(
        inputId = ns('scatterplot_add_lm'),
        label = 'Add lm',
        value = r6$params$scatterplot$add_lm,
        fill = TRUE,
        status = "primary"
      ),

      shiny::selectInput(
        inputId = ns("scatterplot_color_by"),
        label = "Color by",
        choices = c("None", r6$params$sample_metadata),
        selected = r6$params$scatterplot$color_by
      )

    )
  })
}

scatterplot_events = function(r6, dimensions_obj, color_palette, input, output, session) {

  # Generate the plot
  shiny::observeEvent(c(input$scatterplot_views, input$scatterplot_factor, input$scatterplot_feature_count, input$scatterplot_add_lm, input$scatterplot_color_by), {
    print_t("MOFA Scatterplot: Updating params...")
    r6$params$scatterplot$views = input$scatterplot_views
    r6$params$scatterplot$factor = input$scatterplot_factor
    r6$params$scatterplot$nfeatures = input$scatterplot_feature_count
    r6$params$scatterplot$add_lm = input$scatterplot_add_lm
    r6$params$scatterplot$color_by = input$scatterplot_color_by

    base::tryCatch({
      scatterplot_generate(r6, color_palette, dimensions_obj, input)
      scatterplot_spawn(r6, input$scatterplot_img_format, output)
    },error=function(e){
      print_t('MOFA Scatterplot: error, missing data.')
    },finally={}
    )
  })

}




