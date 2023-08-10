#---------------------------------------------- Proteomics experiment class ----
Prot_exp = R6::R6Class(
  "Prot_exp",
  public = list(
    initialize = function(name, id = NA, slot = NA){
      self$name = name
      self$id = id
      self$slot = slot
    },
    #--------------------------------------------------------------- Global ----
    name = NA,
    id = NA,
    slot = NA,

    #----------------------------------------------------------- Parameters ----
    params = list(

      # Volcano plot parameters self$params$volcano_plot$
      volcano_plot = list(
        data_table = 'Total normalized table',
        adjustment = "Benjamini-Hochberg",
        group_col = NULL,
        groups = NULL,
        selected_function = "median",
        selected_test = "t-Test",
        img_format = "png"
      ),

      # Heatmap parameters self$params$heatmap$
      heatmap = list(
        dataset = 'Z-scored table',
        impute = F,
        cluster_samples = F,
        cluster_features = F,
        map_sample_data = NULL,
        group_column_da = NULL,
        apply_da = TRUE,
        alpha_da = 0.8,
        img_format = "png"
      ),

      # PCA parameters self$params$pca$
      pca = list(
        dataset = 'Z-scored table',
        group_column = NULL,
        apply_da = TRUE,
        alpha_da = 0.8,
        img_format = "png"
      )
    ),

    #--------------------------------------------------------------- Indices ----

    indices = list(
      id_col_meta = NA,
      id_col_data = NA,
      type_col = NA,
      group_col = NA,
      batch_col = NA,

      idx_blanks = NULL,
      idx_qcs = NULL,
      idx_pools = NULL,
      idx_samples = NULL,

      rownames_blanks = NULL,
      rownames_qcs = NULL,
      rownames_pools = NULL,
      rownames_samples = NULL,

      excluded_cols = NULL
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(

      imp_meta = NULL,
      raw_meta = NULL,

      imp_data = NULL,
      raw_data = NULL,

      blank_table = NULL,

      feature_table = NULL,

      # Normalised
      total_norm_data = NULL,

      # Z-scored
      z_scored_data = NULL,
      z_scored_total_norm_data = NULL,

      # Plot tables
      volcano_table = NULL,
      heatmap_table = NULL,
      pca_scores_table = NULL,
      pca_loadings_table = NULL


    ),

    #---------------------------------------------------------------- Plots ----
    plots = list(
      volcano_plot = NULL,
      heatmap = NULL,
      pca_plot = NULL
    ),

    #---------------------------------------------------- Parameter methods ----

    param_volcano_plot = function(data_table, adjustment, group_col, groups, selected_function, selected_test, img_format) {

      self$params$volcano_plot$data_table = data_table
      self$params$volcano_plot$adjustment = adjustment
      self$params$volcano_plot$group_col = group_col
      self$params$volcano_plot$groups = groups
      self$params$volcano_plot$selected_function = selected_function
      self$params$volcano_plot$selected_test = selected_test
      self$params$volcano_plot$img_format = img_format

    },

    param_heatmap = function(dataset, impute, cluster_samples, cluster_features, map_sample_data, group_column_da, apply_da, alpha_da, img_format) {
      self$params$heatmap$dataset = dataset
      self$params$heatmap$impute = impute
      self$params$heatmap$cluster_samples = cluster_samples
      self$params$heatmap$cluster_features = cluster_features
      self$params$heatmap$map_sample_data = map_sample_data
      self$params$heatmap$group_column_da = group_column_da
      self$params$heatmap$apply_da = apply_da
      self$params$heatmap$alpha_da = alpha_da
      self$params$heatmap$img_format = img_format
    },

    param_pca = function(dataset, group_column, apply_da, alpha_da, img_format) {
      self$params$pca$dataset = dataset
      self$params$pca$group_column = group_column
      self$params$pca$apply_da = apply_da
      self$params$pca$alpha_da = alpha_da
      self$params$pca$img_format = img_format
    },

    #-------------------------------------------------------- Table methods ----

    set_raw_meta = function(){

      if (!is.na(self$indices$id_col_meta) & !is.null(self$tables$imp_meta)){
        data_table = self$tables$imp_meta
        rownames(data_table) = data_table[,self$indices$id_col_meta]
        data_table[,self$indices$id_col_meta] = NULL
        self$tables$raw_meta = data_table
      }
    },

    set_raw_data = function(apply_imputation = T,
                            impute_before = T,
                            apply_filtering = T,
                            imputation_function = 'median',
                            val_threshold = 0.6,
                            blank_multiplier = 0.8,
                            sample_threshold = 0.8,
                            group_threshold = 0.8,
                            norm_col = "") {

      if (!is.na(self$indices$id_col_data) & !is.null(self$tables$imp_data) & !is.null(self$tables$raw_meta)){

        # Copy imported table
        data_table = self$tables$imp_data

        # Set ID column
        rownames(data_table) = data_table[,self$indices$id_col_data]
        data_table[,self$indices$id_col_data] = NULL
        data_table = as.matrix(data_table)

        # Keep only rows from raw_meta
        data_table = data_table[rownames(self$tables$raw_meta),]

        # Remove columns from exclusion list
        if (!is.null(self$indices$excluded_cols)) {
          data_table = drop_cols(data_table, self$indices$excluded_cols)
        }

        # Remove empty columns
        data_table = remove_empty_cols(data_table)

        # Imputation and filtering
        if (apply_imputation & impute_before & apply_filtering) {
          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)

          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)

        } else if (apply_imputation & !impute_before & apply_filtering) {
          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)

          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)
        } else if (apply_imputation & !apply_filtering) {
          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)
        } else if (!apply_imputation & apply_filtering) {
          # Filtering alone
          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)
        }

        if (norm_col != "") {
          if (is_num_coercible(self$tables$raw_meta[,norm_col]) & !base::any(is.na(self$tables$raw_meta[,norm_col]))) {
            print_tm(self$name, paste0('Normalizing data by ', norm_col))
            data_table = data_table/as.numeric(self$tables$raw_meta[,norm_col])
          } else {
            print_tm(self$name, 'Warning: Normalization skipped, selected column contains either non numeric or missing data.')
          }
        }
        self$tables$raw_data = data_table
      }
    },

    get_blank_table = function() {
      blank_table = self$tables$imp_data[self$indices$idx_blanks,]
      rownames(blank_table) = blank_table[,self$indices$id_col_data]
      blank_table[,self$indices$id_col_data] = NULL
      self$tables$blank_table = as.matrix(blank_table)
    },

    # Total or Row normalisation
    normalise_total = function(){
      self$tables$total_norm_data = self$tables$raw_data/rowSums(self$tables$raw_data, na.rm = T)
    },

    # Z-score normalisation
    normalise_z_score = function() {
      self$tables$z_scored_data = z_score_normalisation(data_table = self$tables$raw_data)
    },

    # Total and z-score normalisation
    normalise_total_z_score = function() {
      self$tables$z_scored_total_norm_data = z_score_normalisation(data_table = self$tables$total_norm_data)
    },


    derive_data_tables = function() {
      # Derive tables

      self$normalise_total()
      self$normalise_z_score()
      self$normalise_total_z_score()


      # Set plotting parameters


      self$param_volcano_plot(data_table = 'Total normalized table',
                              adjustment = 'Benjamini-Hochberg',
                              group_col = self$indices$group_col,
                              groups = unique(self$tables$raw_meta[,self$indices$group_col])[c(1,2)],
                              selected_function = 'median',
                              selected_test = 't-Test',
                              img_format = 'png')

      self$param_heatmap(dataset = 'Z-scored table',
                         impute = F,
                         cluster_samples = F,
                         cluster_features = F,
                         map_sample_data = NULL,
                         group_column_da = self$indices$group_col,
                         apply_da = TRUE,
                         alpha_da = 0.8,
                         img_format = "png")

      self$param_pca(dataset = 'Z-scored table',
                     group_column = self$indices$group_col,
                     apply_da = TRUE,
                     alpha_da = 0.8,
                     img_format = "png")

    },

    #--------------------------------------------------- Plot table methods ----

    # Volcano table
    get_volcano_table = function(data_table = self$tables$raw_data,
                                 group_col = self$indices$group_col,
                                 used_function = "median",
                                 test = "t-Test",
                                 group_1 = self$params$volcano_plot$groups[1],
                                 group_2 = self$params$volcano_plot$groups[2]) {

      volcano_table = data.frame(matrix(data = NA, nrow = ncol(data_table), ncol = 0))
      rownames(volcano_table) = colnames(data_table)

      # Get the rownames for each group
      idx_group_1 = rownames(self$tables$raw_meta)[self$tables$raw_meta[, group_col] == group_1]
      idx_group_2 = rownames(self$tables$raw_meta)[self$tables$raw_meta[, group_col] == group_2]

      # Get all row names from both groups
      idx_all = c(idx_group_1, idx_group_2)
      idx_all = sort(unique(idx_all))

      # Filter data to keep only the two groups
      data_table = data_table[idx_all,]

      # Remove empty columns
      dead_features = colnames(data_table)
      data_table = remove_empty_cols(table = data_table)
      dead_features = setdiff(dead_features, colnames(data_table))

      if (length(dead_features) > 0) {
        dead_features = which(rownames(volcano_table) %in% dead_features)
        volcano_table = volcano_table[-dead_features,]
      }

      # Collect fold change and p-values
      stat_vals = get_fc_and_pval(data_table, idx_group_1, idx_group_2, used_function, test)
      fold_change = stat_vals$fold_change
      p_value = stat_vals$p_value
      p_value_bh_adj = stat_vals$p_value_bh_adj


      volcano_table$fold_change = fold_change
      volcano_table$p_value = p_value
      volcano_table$minus_log10_p_value = -log10(p_value)
      volcano_table$log2_fold_change = log2(fold_change)
      volcano_table$minus_log10_p_value_bh_adj = -log10(p_value_bh_adj)

      # Drop NA p-values
      if (length(which(is.na(volcano_table[,'p_value']))) > 0) {
        volcano_table = volcano_table[-which(is.na(volcano_table[,'p_value'])),]
      }

      self$tables$volcano_table = volcano_table
    },

    #----------------------------------------------------- Plotting methods ----

    ## Volcano plot
    plot_volcano = function(data_table = self$tables$volcano_table,
                            adjustment = "minus_log10_p_value_bh_adj",
                            colour_list,
                            group_1 = self$params$volcano_plot$groups[1],
                            group_2 = self$params$volcano_plot$groups[2],
                            width = NULL,
                            height = NULL){


      max_fc = ceiling(max(abs(data_table[, "log2_fold_change"])))
      data_table$coloring = rep("gray", nrow(data_table))

      upper_left = which(data_table[,adjustment] > -log10(0.05) & data_table[, "log2_fold_change"] < -1)
      upper_right = which(data_table[,adjustment] > -log10(0.05) & data_table[, "log2_fold_change"] > 1)
      lower_corners = which(data_table[,adjustment] <= -log10(0.05) & data_table[, "log2_fold_change"] < -1)
      lower_corners = c(lower_corners, which(data_table[,adjustment] <= -log10(0.05) & data_table[, "log2_fold_change"] > 1))


      if (length(upper_left)>0) {
        data_table[upper_left,"coloring"] = "blue"
      }

      if (length(upper_right)>0) {
        data_table[upper_right,"coloring"] = "red"
      }

      if (length(lower_corners)>0) {
        data_table[lower_corners,"coloring"] = "black"
      }

      fig = plotly::plot_ly(x = data_table[, "log2_fold_change"],
                            y = data_table[, adjustment],
                            text = rownames(data_table),
                            hoverinfo = "text",
                            color = data_table[, "coloring"],
                            colors = c("black", "blue", "gray", "red"),
                            opacity = 0.5,
                            type  = "scatter",
                            mode  = "markers",
                            width = width,
                            height = height)

      fig = fig %>% layout(showlegend = F,
                           shapes = list(vline(x = -1, dash = "dot"), vline(x = 1, dash = "dot"), hline(-log10(0.05), dash = "dot")),
                           title = paste0(group_1, " (left), ", group_2, " (right)"),
                           xaxis = list(title = "Log2(fold change)",
                                        range = c(-max_fc,max_fc)
                           ),
                           yaxis = list(title = adjustment_title_switch(adjustment)))
      self$plots$volcano_plot = fig
    },

    ## Heatmap plot
    plot_heatmap = function(data_table = self$tables$z_scored_total_norm_data,
                            impute = self$params$heatmap$impute,
                            meta_table = self$tables$raw_meta,
                            cluster_rows = self$params$heatmap$cluster_samples,
                            cluster_cols = self$params$heatmap$cluster_features,
                            row_annotations = self$params$heatmap$map_sample_data,
                            apply_da = self$params$heatmap$apply_da,
                            group_column_da = self$params$heatmap$group_column_da,
                            alpha_da = self$params$heatmap$alpha_da,
                            width = NULL,
                            height = NULL) {

      if (apply_da) {
        data_table = apply_discriminant_analysis(data_table = data_table,
                                                 group_list = meta_table[,group_column_da],
                                                 nlambda = 100,
                                                 alpha = alpha_da)

      }

      # Save table as heatmap table
      self$tables$heatmap_table = data_table

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

      val_list = as.vector(data_table)
      val_list = na.omit(val_list)
      val_list = sort(val_list)

      zmax = min(c(abs(min(val_list)), max(val_list)))
      zmin = -zmax

      # Filter out the data
      data_table[data_table > zmax] = zmax
      data_table[data_table < zmin] = zmin

      # Annotations
      if (!is.null(row_annotations)) {
        row_annotations = meta_table[, row_annotations]
      } else {
        row_annotations = NULL
      }

      if (impute) {
        print_tm(self$name, 'Imputing NAs')
        data_table[is.na(data_table)] = zmin
      }

      # Plot the data
      self$plots$heatmap = heatmaply::heatmaply(x = t(data_table),
                                                scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                                                  low = "blue",
                                                  mid = "#faf4af",
                                                  high = "red",
                                                  midpoint = 0,
                                                  # limits = c(-col_lim, col_lim)
                                                  limits = c(zmin, zmax)
                                                ),
                                                # scale_fill_gradient_fun = ggplot2::scale_fill_gradient(
                                                #   low = "blue4",
                                                #   high = "red",
                                                #   # limits = c(-col_lim, col_lim)
                                                # ),
                                                width = width,
                                                height = height,
                                                limits = c(zmin, zmax),
                                                col_side_colors = row_annotations,
                                                dendrogram = dendrogram_list)

    },

    ## PCA scores and loading plots
    plot_pca = function(data_table = self$tables[[self$params$pca$dataset]],
                        group_column = self$params$pca$group_column,
                        apply_da = self$params$pca$apply_da,
                        alpha_da = self$params$pca$alpha_da,
                        width = NULL,
                        height = NULL,
                        colour_list) {


      if (apply_da) {
        data_table = apply_discriminant_analysis(data_table = data_table,
                                                 group_list = self$tables$raw_meta[,group_column],
                                                 nlambda = 100,
                                                 alpha = alpha_da)
      }

      ncol_1 = ncol(data_table)
      data_table = data_table[,!is.na(colSums(data_table))]
      ncol_2 = ncol(data_table)
      if(ncol_2 != ncol_1) {
        print_tm(self$name, paste0("PCA : dropped ", ncol_1 - ncol_2, " features with no signal variation."))
      }

      pca_data = get_pca_data(data_table = data_table)

      fig = c()

      # Store tables
      self$tables$pca_scores_table = pca_data@scores
      self$tables$pca_loadings_table = pca_data@loadings

      fig[[1]] = pca_plot_scores(x = pca_data@scores[, "PC1"],
                                 y = pca_data@scores[, "PC2"],
                                 meta_table = self$tables$raw_meta[rownames(data_table),],
                                 group_col = group_column,
                                 width = width,
                                 height = height,
                                 colour_list = colour_list)
      fig[[1]] = fig[[1]] %>% layout(
        xaxis = list(title = paste0("PC1 (", round(pca_data@R2[1] * 100), "% of the variance)")))

      fig[[2]] = pca_plot_loadings(x = pca_data@loadings[, "PC1"],
                                   y =  pca_data@loadings[, "PC2"],
                                   feature_list = colnames(data_table),
                                   width = width,
                                   height = height,
                                   colour_list = colour_list)
      fig[[2]] = fig[[2]] %>% layout(
        xaxis = list(title = paste0("PC1 (", round(pca_data@R2[1] * 100), "% of the variance)")))

      fig = plotly::subplot(fig, nrows = 1, margin = 0.035, titleX = TRUE)
      fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                           yaxis = list(title = paste0("PC2 (", round(pca_data@R2[2] * 100), "% of the variance)"))
      )

      self$plots$pca_plot = fig
    }


  )
)
