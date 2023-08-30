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
        selected_plot = 1
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

      # feature weights plot parameters
      feature_weights = list(
        views = 1,
        factor = 1,
        nfeatures = 10,
        scale = TRUE,
        abs = FALSE
      ),

      # Feature top weights parameters
      feature_top_weights = list(
        views = 1,
        factor = 1,
        nfeatures = 10
      ),

      # MOFA heatmap parameters
      mofa_heatmap = list(
        views = 1,
        factor = 1,
        nfeatures = 20,
        cluster_rows = TRUE,
        cluster_cols = FALSE,
        show_rownames = TRUE,
        show_colnames = FALSE
      ),

      # Scatterplot parameters
      scatterplot = list(
        views = 1,
        factor = 1,
        nfeatures = 5,
        add_lm = TRUE,
        color_by = NULL
      )

    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(
      metadata = NULL,
      omics_tables = list()
    ),


    #--------------------------------------------------------- MOFA objects ----
    mofa_objects = list(
      pretrained = NULL,
      model = NULL
    ),

    #---------------------------------------------------------------- Plots ----

    plots = list(
      explained_variance = NULL,
      factor_plot = NULL,
      factors_plot = NULL,
      feature_weights = NULL,
      feature_top_weights = NULL,
      mofa_heatmap = NULL,
      scatterplot = NULL
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
                            metadata = self$tables$metadata) {

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
                                       selected_plot = 1) {
      plot = MOFA2::plot_variance_explained(object = model,
                                            x=x,
                                            y=y,
                                            plot_total = plot_total)
      self$plots$explained_variance = plot[[selected_plot]]
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
                                legend = T,
                                add_violin = F,
                                violin_alpha = NULL,
                                color_palette) {

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
                                legend = legend,
                                add_violin = add_violin,
                                violin_alpha = violin_alpha)

      if (add_violin) {
        unique_groups = unique(self$mofa_objects$model@samples_metadata[color_by])
        color_vector = sample(color_palette, length(unique_groups[[1]]))
        names(color_vector) = unique_groups[[1]]

        plot = plot +
          ggplot2::scale_color_manual(values=color_vector) +
          ggplot2::scale_fill_manual(values=color_vector)
      }


      self$plots$factor_plot = plot
    },

    # Combnine factors
    # plot_combine_factors = function(model = self$mofa_objects$model,
    #                                 factors = c(1,2,3),
    #                                 show_missing = TRUE,
    #                                 color_by = NULL,
    #                                 shape_by = NULL,
    #                                 color_name = NULL,
    #                                 shape_name = NULL,
    #                                 dot_size = 2,
    #                                 legend = TRUE) {
    #
    #   print(model)
    #
    #   plot = MOFA2::plot_factors(object = model,
    #                              factors = factors,
    #                              show_missing = show_missing,
    #                              color_by = color_by,
    #                              shape_by = shape_by,
    #                              color_name = color_name,
    #                              shape_name = shape_name,
    #                              dot_size = dot_size,
    #                              legend = legend
    #                              )
    #   self$plots$factors_plot = plot
    # },

    # Feature weights
    plot_feature_weights = function(model = self$mofa_objects$model,
                                    view = 1,
                                    factors = 1,
                                    nfeatures = 10,
                                    color_by = NULL,
                                    shape_by = NULL,
                                    abs = FALSE,
                                    manual = NULL,
                                    color_manual = NULL,
                                    scale = TRUE,
                                    dot_size = 1,
                                    text_size = 5,
                                    legend = TRUE,
                                    return_data = FALSE) {

      plot = MOFA2::plot_weights(object = model,
                                 view = view,
                                 factors = factors,
                                 nfeatures = nfeatures,
                                 color_by = color_by,
                                 shape_by = shape_by,
                                 abs = abs,
                                 manual = manual,
                                 color_manual = color_manual,
                                 scale = scale,
                                 dot_size = dot_size,
                                 text_size = text_size,
                                 legend = legend,
                                 return_data = return_data)

      self$plots$feature_weights = plot

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
                            factor,
                            view = 1,
                            groups = "all",
                            features = 50,
                            annotation_features = NULL,
                            annotation_samples = NULL,
                            transpose = FALSE,
                            imputed = FALSE,
                            denoise = FALSE,
                            max.value = NULL,
                            min.value = NULL,
                            cluster_rows = TRUE,
                            cluster_cols = FALSE,
                            show_rownames = TRUE,
                            show_colnames = FALSE) {

      plot = MOFA2::plot_data_heatmap(object = model,
                                      factor = factor,
                                      view = view,
                                      groups = groups,
                                      features = features,
                                      annotation_features = annotation_features,
                                      annotation_samples = annotation_samples,
                                      transpose = transpose,
                                      imputed = imputed,
                                      denoise = denoise,
                                      max.value = max.value,
                                      min.value = min.value,
                                      cluster_rows = cluster_rows,
                                      cluster_cols = cluster_cols,
                                      show_rownames = show_rownames,
                                      show_colnames = show_colnames)

      self$plots$mofa_heatmap = plot
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
                                imputed = FALSE) {

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

      self$plots$scatterplot = plot
    }



  )
)