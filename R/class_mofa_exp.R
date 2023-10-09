#---------------------------- Class Mofa_data ---------------------------------
Mofa_data = R6::R6Class(
  "Mofa_exp",
  public = list(

    initialize = function(name = NA){
      self$name = name
    },

    #--------------------------------------------------------------- Global ----
    name = NULL,
    type = NULL,

    #----------------------------------------------------------- Parameters ----
    params = list(

      # Number of views / omics
      views = NULL,

      # Number of factors
      factor_list = NULL,
      sample_metadata = NULL,

      # Explained variance parameters
      explained_variance = list(
        selected_plot = 1,
        img_format = 'png'
      ),

      # Factor plot parameters
      factor_plot = list(
        factors = c(1,2),
        groups = "all",
        show_missing = T,
        scale = F,
        color_by = NULL,
        shape_by = NULL,
        color_name = NULL,
        shape_name = NULL,
        dot_size = 2,
        dot_alpha = 1,
        dodge = F,
        legend = T,
        add_violin = F,
        violin_alpha = NULL
      ),

      combined_factors = list(
        factors = c(1,2),
        scale = FALSE,
        color_by = NULL,
        img_format = 'png'
      ),

      # feature weights plot parameters
      feature_weights = list(
        views = 1,
        factors = 1,
        scale = TRUE,
        abs = FALSE,
        img_format = 'png'
      ),

      # Feature top weights parameters
      feature_top_weights = list(
        views = 1,
        factor = 1,
        nfeatures = 10
      ),

      # MOFA heatmap parameters
      mofa_heatmap = list(
        factor = 1,
        view = 1,
        features = 50,
        imputed = FALSE,
        denoise = FALSE,
        cluster_cols = TRUE,
        cluster_rows = TRUE,
        row_annotations = NULL,
        max_value = NULL,
        min_value = NULL,
        img_format = 'png'
      ),

      # Scatterplot parameters
      scatterplot = list(
        view = 1,
        factor = 1,
        features = 6,
        add_lm = TRUE,
        color_by = NULL,
        img_format = 'png'
      )

    ),
    #---------------------------------------------------- Parameter methods ----
    param_explained_variance = function(selected_plot, img_format) {
      self$params$explained_variance$selected_plot = selected_plot
      self$params$explained_variance$img_format = img_format

    },

    param_feature_weights = function(views, factors, scale, abs, img_format) {
      self$params$feature_weights$views = views
      self$params$feature_weights$factors = factors
      self$params$feature_weights$scale = scale
      self$params$feature_weights$abs = abs
      self$params$feature_weights$img_format = img_format
    },

    param_mofa_heatmap = function(factor, view, features, imputed, denoise, cluster_cols,
                                  cluster_rows, row_annotations, max_value, min_value, img_format) {
      self$params$mofa_heatmap$factor = factor
      self$params$mofa_heatmap$view = view
      self$params$mofa_heatmap$features = features
      self$params$mofa_heatmap$imputed = imputed
      self$params$mofa_heatmap$denoise = denoise
      self$params$mofa_heatmap$cluster_cols = cluster_cols
      self$params$mofa_heatmap$cluster_rows = cluster_rows
      self$params$mofa_heatmap$row_annotations = row_annotations
      self$params$mofa_heatmap$max_value = max_value
      self$params$mofa_heatmap$min_value = min_value
      self$params$mofa_heatmap$img_format = img_format
    },

    param_combined_factors = function(factors, scale, color_by, img_format) {
      self$params$combined_factors$factors = factors
      self$params$combined_factors$scale = scale
      self$params$combined_factors$color_by = color_by
      self$params$combined_factors$img_format = img_format
    },

    param_scatterplot = function(view, factor, features, add_lm, color_by, img_format) {
      self$params$scatterplot$view = view
      self$params$scatterplot$factor = factor
      self$params$scatterplot$features = features
      self$params$scatterplot$add_lm = add_lm
      self$params$scatterplot$color_by = color_by
      self$params$scatterplot$img_format = img_format
    },

    param_factor_plot = function(factors, color_by, scale, dodge, add_violin, img_format) {
      self$params$factor_plot$factors = factors
      self$params$factor_plot$color_by = color_by
      self$params$factor_plot$scale = scale
      self$params$factor_plot$dodge = dodge
      self$params$factor_plot$add_violin = add_violin
      self$params$factor_plot$img_format = img_format
    },

    #---------------------------------------------------------------- Plots ----

    plots = list(
      explained_variance = NULL,
      factor_plot = NULL,
      combined_factors = NULL,
      feature_weights = NULL,
      feature_top_weights = NULL,
      mofa_heatmap = NULL,
      scatterplot = NULL
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(
      metadata = NULL,
      omics_tables = list(),
      explained_variance = NULL,
      combined_factors = NULL,
      feature_weights = NULL,
      mofa_heatmap = NULL,
      scatterplot = NULL
    ),


    #--------------------------------------------------------- MOFA objects ----
    mofa_objects = list(
      pretrained = NULL,
      model = NULL
    ),

    #-------------------------------------------------------------- Methods ----
    create_mofa_object = function(matrix_list = self$tables$omics_tables,
                                  groups = NULL) {
      if (!is.null(groups )) {
        groups = gsub("/", "-", groups)
      }
      MOFAobject = MOFA2::create_mofa(data = matrix_list,
                                      groups = groups)
      self$params$views = 1:length(matrix_list)
      self$mofa_objects$pretrained = MOFAobject
    },

    get_example_data = function(n_views = 2,
                                n_samples = 200,
                                n_features = 1000,
                                n_factors = 10) {

      # Load data
      data = MOFA2::make_example_data(
        n_views = n_views,
        n_samples = n_samples,
        n_features = n_features,
        n_factors = n_factors
      )[[1]]

      self$params$views = 1:2

      # Create MOFA object
      MOFAobject = MOFA2::create_mofa(data)


      sample_metadata = data.frame(
        sample = MOFA2::samples_names(MOFAobject)[[1]],
        condition = sample(c("A","B"), size = n_samples, replace = T),
        age = sample(1:100, size = n_samples, replace = T)
      )
      self$tables$metadata = sample_metadata
      self$mofa_objects$pretrained = MOFAobject
    },

    prepare_mofa = function(pretrained = self$mofa_objects$pretrained,
                            scale_views = F,
                            scale_groups = F,
                            center_groups = T,
                            likelihoods = "gaussian",
                            num_factors = 15,
                            spikeslab_factors = F,
                            spikeslab_weights = F,
                            ard_factors = F,
                            ard_weights = T,
                            maxiter = 1000,
                            convergence_mode = "fast",
                            startELBO = 1,
                            freqELBO = 5,
                            stochastic = F,
                            weight_views = F) {
      # Retrieve all options
      data_opts = MOFA2::get_default_data_options(pretrained)
      model_opts = MOFA2::get_default_model_options(pretrained)
      train_opts = MOFA2::get_default_training_options(pretrained)

      # Set data options
      data_opts$scale_views = scale_views
      data_opts$scale_groups = scale_groups
      data_opts$center_groups = center_groups

      # Set model options
      likelihoods = base::rep(likelihoods, length(data_opts$views))
      names(likelihoods) = data_opts$views
      model_opts$likelihoods = likelihoods
      model_opts$num_factors = num_factors
      model_opts$spikeslab_factors = spikeslab_factors
      model_opts$spikeslab_weights = spikeslab_weights
      model_opts$ard_factors = ard_factors
      model_opts$ard_weights = ard_weights

      # Set training options
      train_opts$maxiter = maxiter
      train_opts$convergence_mode = convergence_mode
      train_opts$startELBO = startELBO
      train_opts$freqELBO = freqELBO
      train_opts$stochastic = stochastic
      train_opts$weight_views = weight_views

      # Set parameters to the object
      pretrained = MOFA2::prepare_mofa(
        object = pretrained,
        data_options = data_opts,
        model_options = model_opts,
        training_options = train_opts
      )
      self$mofa_objects$pretrained = pretrained
    },

    train_model = function(mofa_object = self$mofa_objects$pretrained,
                           outfile = NULL,
                           save_data = FALSE) {
      model = MOFA2::run_mofa(object = mofa_object,
                              outfile = outfile,
                              use_basilisk = T,
                              save_data = save_data)

      factor_list = MOFA2::get_factors(model)
      factor_list = colnames(factor_list$group1)
      factor_list = 1:length(factor_list)

      self$params$factor_list = factor_list

      self$mofa_objects$model = model
    },

    get_views = function(model = self$mofa_objects$pretrained) {
      self$params$views = 1:length(model@data)

    },

    add_metadata_to_mofa = function(model = self$mofa_objects$model,
                            metadata = data.frame(t(self$tables$metadata))) {

      metadata['sample'] = rownames(metadata)
      MOFA2::samples_metadata(model) = metadata
      meta_cols = colnames(metadata)

      self$params$sample_metadata = meta_cols
      self$mofa_objects$model = model
    },

    #----------------------------------------------------- Plotting methods ----

    # Plot explained variance
    plot_explained_variance = function(model = self$mofa_objects$model,
                                       x = "view",
                                       y = "factor",
                                       plot_total = TRUE,
                                       selected_plot = 1,
                                       split_by = NA,
                                       use_cache = TRUE,
                                       width = NULL,
                                       height = NULL) {

      selected_plot = as.numeric(selected_plot)

      if (is.na(split_by)) {
        split_by = setdiff(c("view", "factor", "group"), c(x, y, split_by))
      }
      if ((use_cache) & .hasSlot(model, "cache") && ("variance_explained" %in%
                                                      names(model@cache))) {
        r2_list <- model@cache$variance_explained
      } else {
        r2_list <- MOFA2::calculate_variance_explained(model, factors = factors,
                                                       ...)
      }

      data_table = r2_list$r2_per_factor$group1
      max_col = which.max(apply(data_table, 2, max))
      data_table = data_table[order(data_table[,max_col], decreasing = F),]

      if (selected_plot == 1) {
        fig = heatmaply::heatmaply(data_table,
                                   dendrogram = 'none',
                                   Rowv = F,
                                   Colv = F,
                                   trace = 'none',
                                   grid_gap = 1,
                                   scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                                     low = "gray",
                                     high = "blue",
                                     midpoint = max(data_table)/2,
                                     limits = c(0, max(data_table))
                                   ),
                                   width = width,
                                   height = height
        )

      } else if (selected_plot == 2) {
        data_table = as.data.frame(colSums(data_table))
        fig = plotly::plot_ly(
          x = rownames(data_table),
          y = data_table[,1],
          type = "bar"
        )
        fig = fig %>% layout(yaxis = list(title = 'Variance explained (%)'),
                             width = width,
                             height = height)
      }
      self$plots$explained_variance = fig
      self$tables$explained_variance = data_table
    },

    # Factor plot
    plot_factor_plot = function(model = self$mofa_objects$model,
                                factors = c(1,2),
                                groups = "all",
                                show_missing = T,
                                scale = F,
                                color_by = NULL,
                                shape_by = NULL,
                                color_name = NULL,
                                shape_name = NULL,
                                dot_size = 2,
                                dot_alpha = 1,
                                dodge = F,
                                add_violin = F,
                                violin_alpha = NULL,
                                width = NULL,
                                height = NULL) {

      factors = as.numeric(factors)

      if (add_violin) {violin_alpha = 0.25}

      plot = MOFA2::plot_factor(object = model,
                                factor = factors,
                                show_missing = show_missing,
                                scale = scale,
                                color_by = color_by,
                                shape_by = shape_by,
                                color_name = color_name,
                                shape_name = shape_name,
                                dot_size = dot_size,
                                dot_alpha = dot_alpha,
                                dodge = dodge,
                                legend = T,
                                add_violin = add_violin,
                                violin_alpha = violin_alpha)

      plot = plotly::ggplotly(plot)

      self$plots$factor_plot = plot
    },

    # Combnined factors
    plot_combined_factors = function(model = self$mofa_objects$model,
                                     factors = self$params$combined_factors$factors,
                                     scale = self$params$combined_factors$scale,
                                     color_by = self$params$combined_factors$color_by,
                                     width = NULL,
                                     height = NULL) {
      factors = as.numeric(factors)
      if (length(factors) < 2) {
        stop("At least 2 factors should be selected.")
      }
      if (color_by == "") {
        color_by = NULL
      }
      factors <- MOFA2:::.check_and_get_factors(model, factors)
      Z <- MOFA2::get_factors(model, factors = factors, groups = "all",
                              as.data.frame = TRUE)
      Z <- Z[stats::complete.cases(Z), ]

      factors <- MOFA2:::.check_and_get_factors(model, factors)
      Z <- MOFA2::get_factors(model, factors = factors, groups = "all",
                              as.data.frame = TRUE)
      color_by <- MOFA2:::.set_colorby(model, color_by)
      Z <- Z[complete.cases(Z), ]
      df <- merge(Z, color_by, by = "sample")
      df <- tidyr::spread(df, key = "factor", value = "value")

      if (length(unique(df$color_by)) > nrow(df)/2) {
        print('Less than two samples per group, removing groups')
        df$color_by = 1
      }

      if (scale) {
        for (factor in factors) {
          df[[factor]] <- df[[factor]]/max(abs(df[[factor]]))
        }
      }

      step = 1/length(factors)
      x = step/2
      y = 0.99
      i = 1
      annotations = c()
      for (factor in factors) {
        tmp_ann = list(
          x = x,
          y = y,
          text = factor,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE)
        annotations[[i]] = tmp_ann
        i = i + 1
        x = x + step
      }

      x = -0.04
      y = step/3

      for (factor in rev(factors)) {
        tmp_ann = list(
          x = x,
          y = y,
          text = factor,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE,
          textangle=-90)
        annotations[[i]] = tmp_ann
        i = i + 1
        y = y + step
      }

      groups = unique(as.character(df$color_by))
      color_map = setNames(sample(colors(), length(unique(df$color_by))), unique(df$color_by))
      cleared_groups = c()
      plot_list = list()
      j = 1
      for (factor_1 in factors) {
        for (factor_2 in factors) {
          if (factor_1 == factor_2) {
            fig = plotly::plot_ly()
            for (group in groups) {
              if (group %in% cleared_groups) {
                first_bool = F
              } else {
                first_bool = T
                cleared_groups = c(cleared_groups, group)
              }
              grp_bool = df$color_by == group
              density_data = density(df[grp_bool, factor_1])
              kept <- seq(1, length(density_data$x), by = 10)
              fig <- fig %>% add_trace(x = density_data$x[kept],
                                       y = density_data$y[kept],
                                       type = 'scatter',
                                       mode = 'lines',
                                       fill = 'tozeroy',
                                       name = group,
                                       color = color_map[group],
                                       legendgroup=group,
                                       showlegend = first_bool)
              fig = fig %>% layout(xaxis= list(tickfont = list(size = 8)),
                                   yaxis = list(tickfont = list(size = 8)))
            }
            plot_list[[j]] = plotly::plotly_build(fig)
            j = j + 1
          } else {
            fig = plotly::plot_ly()
            for (group in groups) {
              if (group %in% cleared_groups) {
                first_bool = F
              } else {
                first_bool = T
                cleared_groups = c(cleared_groups, group)
              }
              grp_bool = df$color_by == group
              fig <- fig %>% add_trace(x = df[[factor_1]][grp_bool],
                                       y = df[[factor_2]][grp_bool],
                                       type = 'scatter',
                                       name = group,
                                       color = color_map[group],
                                       legendgroup=group,
                                       showlegend = first_bool)
              fig = fig %>% layout(xaxis= list(tickfont = list(size = 8)),
                                   yaxis = list(tickfont = list(size = 8)))
            }
            plot_list[[j]] = plotly::plotly_build(fig)
            j = j + 1
          }
        }
      }
      fig = subplot(plot_list, nrows = length(factors), margin = 0.035, titleX = TRUE)
      fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                           annotations = annotations)
      self$plots$combined_factors = fig
      self$tables$combined_factors = df
    },

    # Feature weights
    plot_feature_weights = function(model = self$mofa_objects$model,
                                    views = self$params$feature_weights$views,
                                    factors = self$params$feature_weights$factors,
                                    scale = self$params$feature_weights$scale,
                                    abs = self$params$feature_weights$abs,
                                    width = NULL,
                                    height = NULL) {

      views = as.numeric(views)
      factors = as.numeric(factors)

      views <- MOFA2:::.check_and_get_views(model, views)
      factors <- MOFA2:::.check_and_get_factors(model, factors)
      W <- get_weights(model, views = views, factors = factors,
                       as.data.frame = TRUE)

      if (scale && sum(W$value > 0) > 0) {
        W$value <- W$value/max(abs(W$value))
      }

      if (abs) {
        W$value <- abs(W$value)
        plot_range = c(-0.1,max(abs(W$value)) + 0.1)
      } else {
        plot_range = c(-max(abs(W$value)) -0.1 ,max(abs(W$value)+0.1))
      }

      W$rank = base::rank(W$value)

      fig = plotly::plot_ly(x = W$value,
                            y = W$rank,
                            text = W$feature,
                            width = width,
                            height = height)
      fig = fig %>% layout(title = paste0(views, ', ', factors),
                           xaxis = list(range = plot_range,
                                        title = 'Weight'),
                           yaxis = list(title = 'Rank'))

      self$tables$feature_weights = W
      self$plots$feature_weights = fig

    },

    # Feature top weights
    plot_feature_top_weights = function(model = self$mofa_objects$model,
                                        view = 1,
                                        factors = 1,
                                        nfeatures = 10,
                                        abs = TRUE,
                                        scale = TRUE,
                                        sign = "all") {

      plot = MOFA2::plot_top_weights(object = model,
                                     view = view,
                                     factors = factors,
                                     nfeatures = nfeatures,
                                     abs = abs,
                                     scale = scale,
                                     sign = sign)

      self$plots$feature_top_weights = plot
    },

    # MOFA Heatmap
    plot_mofa_heatmap = function(model = self$mofa_objects$model,
                                 meta_table = t(self$tables$metadata),
                                 factor = 1,
                                 view = 1,
                                 features = 50,
                                 imputed = FALSE,
                                 denoise = FALSE,
                                 cluster_cols = TRUE,
                                 cluster_rows = TRUE,
                                 row_annotations = NULL,
                                 max_value = NULL,
                                 min_value = NULL,
                                 width = NULL,
                                 height = NULL) {


      factor = as.numeric(factor)
      view = as.numeric(view)
      features = as.numeric(features)

      if (!is.null(min_value)){
        if (min_value == "") {
          min_value = NULL
        } else {
          min_value = as.numeric(min_value)
        }
      }

      if (!is.null(max_value)){
        if (max_value == "") {
          max_value = NULL
        } else {
          max_value = as.numeric(max_value)
        }
      }


      groups = "all"

      groups <- MOFA2:::.check_and_get_groups(model, groups)
      factor <- MOFA2:::.check_and_get_factors(model, factor)
      view <- MOFA2:::.check_and_get_views(model, view)
      W <- do.call(rbind, MOFA2::get_weights(model, views = view, factors = factor,
                                             as.data.frame = FALSE))

      Z <- lapply(MOFA2::get_factors(model)[groups], function(z) as.matrix(z[,factor]))
      Z <- do.call(rbind, Z)[, 1]
      Z <- Z[!is.na(Z)]

      if (isTRUE(denoise)) {
        data <- MOFA2::predict(model, views = view, groups = groups)[[1]]
      } else {
        if (isTRUE(imputed)) {
          data <- MOFA2::get_imputed_data(model, view, groups)[[1]]
        }
        else {
          data <- MOFA2::get_data(model, views = view, groups = groups)[[1]]
        }
      }

      if (is(data, "list")) {
        data <- do.call(cbind, data)
      }
      if (is(features, "numeric")) {
        if (length(features) == 1) {
          features <- rownames(W)[tail(order(abs(W)), n = features)]
        } else {
          features <- rownames(W)[order(-abs(W))[features]]
        }
        features <- names(W[features, ])[order(W[features, ])]
      } else if (is(features, "character")) {
        stopifnot(all(features %in% features_names(model)[[view]]))
      }else {
        stop("Features need to be either a numeric or character vector")
      }

      data <- data[features, ]
      data <- data[, names(Z)]
      data <- data[, apply(data, 2, function(x) !all(is.na(x)))]
      order_samples <- names(sort(Z, decreasing = TRUE))
      order_samples <- order_samples[order_samples %in% colnames(data)]
      data <- data[, order_samples]

      if (!is.null(max_value)) {
        data[data >= max_value] = max_value
      }

      if (!is.null(min_value)) {
        data[data <= min_value] = min_value
      }


      # Annotations
      if (!is.null(row_annotations)) {
        if (length(row_annotations) > 1) {
          row_annotations = meta_table[, row_annotations]
          colnames(row_annotations) = stringr::str_replace_all(colnames(row_annotations), "_", " ")
        } else {
          row_names = row_annotations
          row_annotations = as.data.frame(meta_table[, row_annotations],
                                          row.names = rownames(meta_table))
          colnames(row_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }


      # Set the clustering
      if (cluster_rows & cluster_cols) {
        dendrogram_list = "both"
      } else if (cluster_rows) {
        dendrogram_list = "column" # Because of the transpose, rows => cols
      } else if (cluster_cols) {
        dendrogram_list = "row" # Because of the transpose, cols => rows
      } else {
        dendrogram_list = "none"
      }

      plot = heatmaply::heatmaply(x = data,
                                 scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                                   low = "blue",
                                   mid = "#faf4af",
                                   high = "red",
                                   midpoint = max(data, na.rm = T)/2,
                                   limits = c(min(data, na.rm = T), max(data, na.rm = T))
                                 ),
                                 width = width,
                                 height = height,
                                 limits = c(min(data, na.rm = T), max(data, na.rm = T)),
                                 col_side_colors = row_annotations,
                                 dendrogram = dendrogram_list)

      self$plots$mofa_heatmap = plot
      self$tables$mofa_heatmap = data

    },

    # Scatterplot
    plot_scatterplot = function(model = self$mofa_objects$model,
                                factor = 1,
                                view = 1,
                                groups = "all",
                                features = 10,
                                sign = "all",
                                color_by = "group",
                                legend = TRUE,
                                alpha = 1,
                                shape_by = NULL,
                                stroke = NULL,
                                dot_size = 2.5,
                                text_size = NULL,
                                add_lm = TRUE,
                                lm_per_group = TRUE,
                                imputed = FALSE,
                                width = NULL,
                                height = NULL) {

      if (!is.null(color_by)) {
        if (color_by == ""){
          color_by = NULL
        }
      }

      view = as.numeric(view)
      factor = as.numeric(factor)
      features = as.numeric(features)
      plot = MOFA2::plot_data_scatter(object = model,
                                      factor = factor,
                                      view = view,
                                      groups = groups,
                                      features = features,
                                      sign = sign,
                                      color_by = color_by,
                                      legend = legend,
                                      alpha = alpha,
                                      shape_by = shape_by,
                                      stroke = stroke,
                                      dot_size = dot_size,
                                      text_size = text_size,
                                      add_lm = add_lm,
                                      lm_per_group = lm_per_group,
                                      imputed = imputed)
      plot = plotly::ggplotly(plot)
      self$plots$scatterplot = plot
    }



  )
)