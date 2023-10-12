#---------------------------------------------- Lipidomics experiment class ----
Lips_exp = R6::R6Class(
  "Lips_exp",
  public = list(
    initialize = function(name, id = NA, slot = NA, preloaded = F){
      self$name = name
      self$id = id
      self$slot = slot
      self$preloaded_data = preloaded
    },
    #--------------------------------------------------------------- Global ----
    name = NA,
    id = NA,
    slot = NA,
    type = 'Lipidomics',
    preloaded_data = F,

    #----------------------------------------------------------- Parameters ----
    params = list(
      # Class distribution parameters
      class_distribution = list(
        dataset = 'Class table total normalized',
        group_col = NULL,
        img_format = "png"
      ),

      # Class comparison parameters
      class_comparison = list(
        dataset = 'Class table total normalized',
        group_col = NULL,
        img_format = "png"
      ),

      # Volcano plot parameters self$params$volcano_plot$
      volcano_plot = list(
        data_table = 'Total normalized table',
        adjustment = "Benjamini-Hochberg",
        group_col = NULL,
        groups = NULL,
        classes = NULL,
        selected_function = "median",
        selected_test = "t-Test",
        colouring = "Lipid class",
        img_format = "png"
      ),

      # Heatmap parameters self$params$heatmap$
      heatmap = list(
        dataset = 'Z-scored total normalized table',
        impute = F,
        cluster_samples = F,
        cluster_features = F,
        map_sample_data = NULL,
        map_feature_data = NULL,
        group_column_da = NULL,
        apply_da = FALSE,
        alpha_da = 0.8,
        img_format = "png"
      ),

      # PCA parameters self$params$pca$
      pca = list(
        dataset = 'Z-scored total normalized table',
        group_column = NULL,
        apply_da = FALSE,
        alpha_da = 0.8,
        img_format = "png"
      ),

      # Double bonds parameters self$params$db_plot$
      db_plot = list(
        dataset = "Total normalized table",
        adjustment = "Benjamini-Hochberg",
        group_column = NULL,
        selected_groups = NULL,
        selected_lipid_class = NULL,
        selected_carbon_chain = 'Carbon count (chain 1)',
        selected_unsat = 'Double bonds (chain 1)',
        selected_function = "median",
        selected_test = "T-test",
        fc_range = c(-5, 5),
        fc_values = c(-1, 1),
        pval_range = c(0, 5),
        pval_values = c(1, 5),
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

      # Group summaries
      summary_species_table = NULL,
      summary_class_table = NULL,

      # Normalised
      class_norm_data = NULL,
      total_norm_data = NULL,

      # Z-scored
      z_scored_data = NULL,
      z_scored_class_norm_data = NULL,
      z_scored_total_norm_data = NULL,

      # class tables
      class_table= NULL,
      class_table_z_scored = NULL,
      class_table_total_norm = NULL,
      class_table_z_scored_total_norm = NULL,

      # Plot tables
      class_distribution_table = NULL,
      volcano_table = NULL,
      heatmap_table = NULL,
      pca_scores_table = NULL,
      pca_loadings_table = NULL,
      dbplot_table = NULL


    ),

    #---------------------------------------------------------------- Plots ----
    plots = list(
      class_distribution = NULL,
      class_comparison = NULL,
      volcano_plot = NULL,
      heatmap = NULL,
      pca_plot = NULL,
      double_bond_plot = NULL
    ),

    #---------------------------------------------------- Parameter methods ----

    param_class_distribution = function(dataset, group_col, img_format) {
      self$params$class_distribution$dataset = dataset
      self$params$class_distribution$group_col = group_col
      self$params$class_distribution$img_format = img_format
    },

    param_class_comparison = function(dataset, group_col, img_format) {
      self$params$class_comparison$dataset = dataset
      self$params$class_comparison$group_col = group_col
      self$params$class_comparison$img_format = img_format
    },

    param_volcano_plot = function(data_table, adjustment, group_col, groups, classes, selected_function, selected_test, colouring, img_format) {

      self$params$volcano_plot$data_table = data_table
      self$params$volcano_plot$adjustment = adjustment
      self$params$volcano_plot$group_col = group_col
      self$params$volcano_plot$groups = groups
      self$params$volcano_plot$classes = classes
      self$params$volcano_plot$selected_function = selected_function
      self$params$volcano_plot$selected_test = selected_test
      self$params$volcano_plot$colouring = colouring
      self$params$volcano_plot$img_format = img_format

    },

    param_heatmap = function(dataset, impute, cluster_samples, cluster_features, map_sample_data, map_feature_data, group_column_da, apply_da, alpha_da, img_format) {
      self$params$heatmap$dataset = dataset
      self$params$heatmap$impute = impute
      self$params$heatmap$cluster_samples = cluster_samples
      self$params$heatmap$cluster_features = cluster_features
      self$params$heatmap$map_sample_data = map_sample_data
      self$params$heatmap$map_feature_data = map_feature_data
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

    param_db_plot = function(dataset, adjustment, group_column, selected_groups, selected_lipid_class,
                             selected_carbon_chain, selected_unsat, selected_function,
                             selected_test, fc_range, fc_values, pval_range,
                             pval_values, img_format) {

      self$params$db_plot$dataset = dataset
      self$params$db_plot$adjustment = adjustment
      self$params$db_plot$group_column = group_column
      self$params$db_plot$selected_groups = selected_groups
      self$params$db_plot$selected_lipid_class = selected_lipid_class
      self$params$db_plot$selected_carbon_chain = selected_carbon_chain
      self$params$db_plot$selected_unsat = selected_unsat
      self$params$db_plot$selected_function = selected_function
      self$params$db_plot$selected_test = selected_test
      self$params$db_plot$fc_range = fc_range
      self$params$db_plot$fc_values = fc_values
      self$params$db_plot$pval_range = pval_range
      self$params$db_plot$pval_values = pval_values
      self$params$db_plot$img_format = img_format

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
            print(paste0('Normalizing data by ', norm_col))
            data_table = data_table/as.numeric(self$tables$raw_meta[,norm_col])
          } else {
            print('Warning: Normalization skipped, selected column contains either non numeric or missing data.')
          }
        }
        self$tables$raw_data = data_table
      }
    },

    get_feature_table = function() {
      self$tables$feature_table = get_feature_metadata(data_table = self$tables$raw_data)
    },

    get_blank_table = function() {
      blank_table = self$tables$imp_data[self$indices$idx_blanks,]
      rownames(blank_table) = blank_table[,self$indices$id_col_data]
      blank_table[,self$indices$id_col_data] = NULL
      self$tables$blank_table = as.matrix(blank_table)
    },


    # Class normalisation
    normalise_class = function(){
      self$tables$class_norm_data = normalise_lipid_class(self$tables$raw_data)
    },

    # Total or Row normalisation
    normalise_total = function(){
      self$tables$total_norm_data = self$tables$raw_data/rowSums(self$tables$raw_data, na.rm = T)
    },

    # Z-score normalisation
    normalise_z_score = function() {
      self$tables$z_scored_data = z_score_normalisation(data_table = self$tables$raw_data)
    },

    # Class and z-score normalisation
    normalise_class_z_score = function() {
      self$tables$z_scored_class_norm_data = z_score_normalisation(data_table = self$tables$class_norm_data)
    },

    # Total and z-score normalisation
    normalise_total_z_score = function() {
      self$tables$z_scored_total_norm_data = z_score_normalisation(data_table = self$tables$total_norm_data)
    },

    # Class table
    get_class_table = function(){
      self$tables$class_table = get_lipid_class_table(self$tables$raw_data)
    },

    # Class table z-scored
    get_class_table_z_scored = function(){
      self$tables$class_table_z_scored = z_score_normalisation(data_table = self$tables$class_table)
    },

    # Class table total norm
    class_grouping_total_norm = function(){
      self$tables$class_table_total_norm = get_lipid_class_table(self$tables$total_norm_data)
    },

    # Z-score the class table (generated by the class_grouping method)
    normalise_class_table_z_score = function() {
      self$tables$class_table_z_scored_total_norm = z_score_normalisation(data_table = self$tables$class_table_total_norm)
    },

    get_group_summary_species = function() {
      self$tables$summary_species_table = get_group_median_table(data_table = self$tables$raw_data,
                                                         meta_table = self$tables$raw_meta,
                                                         group_col = self$indices$group_col)
    },

    get_group_summary_classes = function() {
      self$tables$summary_class_table = get_group_median_table(data_table = self$tables$class_table,
                                                               meta_table = self$tables$raw_meta,
                                                               group_col = self$indices$group_col)
    },

    derive_data_tables = function() {
      # Derive tables

      self$get_feature_table()
      self$normalise_class()
      self$normalise_total()
      self$normalise_z_score()
      self$normalise_class_z_score()
      self$normalise_total_z_score()
      self$get_class_table()
      self$get_class_table_z_scored()
      self$class_grouping_total_norm()
      self$normalise_class_table_z_score()
      self$get_group_summary_species()
      self$get_group_summary_classes()

      # Set plotting parameters
      self$param_class_distribution(dataset = 'Class table total normalized',
                                    group_col = self$indices$group_col,
                                    img_format = "png")

      self$param_class_comparison(dataset = 'Class table total normalized',
                                  group_col = self$indices$group_col,
                                  img_format = "png")

      self$param_volcano_plot(data_table = 'Total normalized table',
                              adjustment = 'Benjamini-Hochberg',
                              group_col = self$indices$group_col,
                              groups = unique(self$tables$raw_meta[,self$indices$group_col])[c(1,2)],
                              classes = NULL,
                              selected_function = 'median',
                              selected_test = 't-Test',
                              colouring = 'Lipid class',
                              img_format = 'png')

      self$param_heatmap(dataset = 'Z-scored total normalized table',
                         impute = F,
                         cluster_samples = F,
                         cluster_features = F,
                         map_sample_data = NULL,
                         map_feature_data = NULL,
                         group_column_da = self$indices$group_col,
                         apply_da = FALSE,
                         alpha_da = 0.8,
                         img_format = "png")

      self$param_pca(dataset = 'Z-scored total normalized table',
                     group_column = self$indices$group_col,
                     apply_da = FALSE,
                     alpha_da = 0.8,
                     img_format = "png")

      self$param_db_plot(dataset = "Total normalized table",
                         adjustment = "Benjamini-Hochberg",
                         group_column = self$indices$group_col,
                         selected_groups = unique(self$tables$raw_meta[,self$indices$group_col])[c(1,2)],
                         selected_lipid_class = NULL,
                         selected_carbon_chain = 'Carbon count (chain 1)',
                         selected_unsat = 'Double bonds (chain 1)',
                         selected_function = "median",
                         selected_test = "t-Test",
                         fc_range = c(-5, 5),
                         fc_values = c(-1, 1),
                         pval_range = c(0, 5),
                         pval_values = c(1, 5),
                         img_format = "png")








    },

    #--------------------------------------------------- Plot table methods ----

    # Volcano table
    get_volcano_table = function(data_table = self$tables$raw_data,
                                 volcano_table = self$tables$feature_table,
                                 group_col = self$indices$group_col,
                                 used_function = "mean",
                                 test = "t-Test",
                                 group_1 = self$params$volcano_plot$groups[1],
                                 group_2 = self$params$volcano_plot$groups[2]) {





      rownames_group_1 = rownames(self$tables$raw_meta)[self$tables$raw_meta[, group_col] == group_1]
      rownames_group_2 = rownames(self$tables$raw_meta)[self$tables$raw_meta[, group_col] == group_2]
      all_rownames = sort(unique(c(rownames_group_1, rownames_group_2)))

      # Filter data to keep only the two groups
      data_table = data_table[all_rownames,]

      # Get the indices for each group
      idx_group_1 = which(rownames(data_table) %in% rownames_group_1)
      idx_group_2 = which(rownames(data_table) %in% rownames_group_2)


      # Remove empty columns
      dead_features = colnames(data_table)
      data_table = remove_empty_cols(table = data_table)
      dead_features = setdiff(dead_features, colnames(data_table))

      if (length(dead_features) > 0) {
        dead_features = which(rownames(volcano_table) %in% dead_features)
        volcano_table = volcano_table[-dead_features,]
      }


      # Collect fold change and p-values
      volcano_table$fold_change = get_fold_changes(data_table = data_table,
                                                   idx_group_1 = idx_group_1,
                                                   idx_group_2 = idx_group_2,
                                                   used_function = used_function)


      volcano_table$p_val = get_p_val(data_table = data_table,
                                      idx_group_1 = idx_group_1,
                                      idx_group_2 = idx_group_2,
                                      used_function = test)
      volcano_table$q_val_bh = stats::p.adjust(volcano_table$p_val, method = "BH")

      volcano_table$minus_log10_p_value = -log10(volcano_table$p_val)
      volcano_table$log2_fold_change = log2(volcano_table$fold_change)
      volcano_table$minus_log10_p_value_bh_adj = -log10(volcano_table$q_val_bh)

      self$tables$volcano_table = volcano_table
    },

    # Double bond plot table
    get_dbplot_table_single = function(data_table = self$tables[[self$params$db_plot$dataset]],
                                       dbplot_table = self$tables$feature_table,
                                       col_group = self$params$db_plot$group_column,
                                       used_function = self$params$db_plot$selected_function,
                                       group_1 = self$params$db_plot$selected_groups[1]){

      # Set the averaging function
      if (used_function == "median") {
        av_function = function(x) {return(median(x, na.rm = T))}
      } else {
        av_function = function(x) {return(mean(x, na.rm = T))}
      }

      # Get the rownames for each group
      idx_group_1 = rownames(self$tables$raw_meta)[self$tables$raw_meta[, col_group] == group_1]

      # Remove empty columns
      dead_features = colnames(data_table)
      data_table = remove_empty_cols(table = data_table)
      dead_features = setdiff(dead_features, colnames(data_table))

      if (length(dead_features) > 0) {
        dead_features = which(rownames(dbplot_table) %in% dead_features)
        dbplot_table = dbplot_table[-dead_features,]
      }


      averages = apply(data_table,2,av_function)
      dbplot_table[, "averages"] = averages

      lips = rownames(dbplot_table)
      txt_medians = as.character(round(dbplot_table[,"averages"],5))
      dbplot_table$text = paste0(lips, " | ", used_function, ": ", txt_medians)

      self$tables$dbplot_table = dbplot_table
    },

    get_dbplot_table_double = function(data_table,
                                       dbplot_table = self$tables$feature_table,
                                       col_group = self$params$db_plot$group_column,
                                       used_function = self$params$db_plot$selected_function,
                                       test = self$params$db_plot$selected_test,
                                       group_1 = self$params$db_plot$selected_groups[1],
                                       group_2 = self$params$db_plot$selected_groups[2]) {

      # Get the rownames for each group
      idx_group_1 = which(meta_table[, group_col] == group_1)
      idx_group_2 = which(meta_table[, group_col] == group_2)

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
        dead_features = which(rownames(dbplot_table) %in% dead_features)
        dbplot_table = dbplot_table[-dead_features,]
      }


      # Collect fold change and p-values
      dbplot_table$fold_change = get_fold_changes(data_table = data_table,
                                                   idx_group_1 = idx_group_1,
                                                   idx_group_2 = idx_group_2,
                                                   used_function = used_function)


      dbplot_table$p_val = get_p_val(data_table = data_table,
                                      idx_group_1 = idx_group_1,
                                      idx_group_2 = idx_group_2,
                                      used_function = test)
      dbplot_table$q_val_bh = stats::p.adjust(dbplot_table$p_val, method = "BH")

      dbplot_table$minus_log10_p_value = -log10(dbplot_table$p_val)
      dbplot_table$log2_fold_change = log2(dbplot_table$fold_change)
      dbplot_table$minus_log10_p_value_bh_adj = -log10(dbplot_table$q_val_bh)

      lips = rownames(dbplot_table)
      fc = as.character(round(dbplot_table[,"log2_fold_change"],2))
      pval = as.character(round(dbplot_table[,"minus_log10_p_value_bh_adj"],2))
      dbplot_table$text = paste0(lips, " | log2(fc): ", fc, " | -log10(bh(pval)): ", pval)



      self$tables$dbplot_table = dbplot_table
    },

    #----------------------------------------------------- Plotting methods ----
    # Class distribution
    plot_class_distribution = function(table = self$tables$class_table_total_norm,
                                       meta_table = self$tables$raw_meta,
                                       group_col = self$indices$group_col,
                                       colour_list,
                                       width = NULL,
                                       height = NULL){

      # Produce the class x group table
      samp_list = rownames(table)
      class_list = colnames(table)
      group_list = sort(unique(meta_table[,group_col]))

      plot_table = data.frame(matrix(data = 0.0,
                                     nrow = length(class_list),
                                     ncol = length(group_list)))
      rownames(plot_table) = class_list
      colnames(plot_table) = group_list

      for (c in class_list) {
        for (g in group_list){
          s = rownames(meta_table)[meta_table[,group_col] == g]
          m = mean(as.matrix(table[s, c]))
          plot_table[c,g] = m
        }
      }

      # Store the plot_table
      self$tables$class_distribution_table = plot_table

      # Produce the plot
      i = 1
      fig = plotly::plot_ly(colors = colour_list, width = width, height = height)
      for (col in colnames(plot_table)) {
        fig = fig %>% add_trace(x = rownames(plot_table), y = plot_table[,col],
                                name = col, color = colour_list[i], type  = "bar")
        fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                             yaxis = list(title = "Concentration"))
        i = i + 1
      }

      self$plots$class_distribution = fig
    },

    # Class comparison
    plot_class_comparison = function(data_table = self$tables$class_table_total_norm,
                                     meta_table = self$tables$raw_meta,
                                     group_col = self$indices$group_col,
                                     colour_list,
                                     width = NULL,
                                     height = NULL){

      # Get sample groups and the list of classes
      groups = sort(unique(meta_table[,group_col]))
      class_list = colnames(data_table)

      x_dim = ceiling(sqrt(length(class_list)))
      y_dim = floor(sqrt(length(class_list)))


      x_step = 1/x_dim
      y_step = 1/y_dim

      x = x_step/2
      y = 0.97 - y_step
      i = 1

      annotations = c()
      for (c in class_list) {
        tmp_ann = list(
          x = x,
          y = y,
          text = c,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "bottom",
          showarrow = FALSE)
        annotations[[i]] = tmp_ann
        i = i + 1
        x = x + x_step
        if (x >= 1) {
          x = x_step/2
          y = y - y_step}
        }
      annotations[[i]] = list(x = -0.08, y = 0.5, text = "Concentration",
                              font = list(size = 10),
                              textangle = 270, showarrow = FALSE, xref='paper',
                              yref='paper')

      # Plot list will be the list of subplots
      plot_list = c()

      # Cleared groups is created for the legends
      cleared_groups = c()
      j = 1
      for (c in class_list) {
        i = 1
        subplot = plot_ly(colors = colour_list, width = width, height = height)
        for (g in groups){
          if (g %in% cleared_groups) {
            first_bool = FALSE
          }else{
            first_bool = TRUE
            cleared_groups = c(cleared_groups, g)
          }

          # For each class, each group
          s = rownames(meta_table)[meta_table[, group_col] == g] # Get the samples for the current group
          d = data_table[s, c] # Get the concentrations for all s samples in the current class c
          m = mean(d) # Get the mean concentration for samples s for class c

          # Subplot for the bar chart displaying the mean concentration
          subplot = subplot %>% add_trace(x = g, y = m, type  = "bar", name = g,
                                          color = colour_list[i], alpha = 1,
                                          legendgroup=i, showlegend = first_bool)

          # Subplot for boxplots displaying the median and all datapoints
          subplot = subplot %>% add_trace(x = g, y = d, type  = "box", boxpoints = "all",
                                          pointpos = 0, name = g, color = colour_list[i],
                                          line = list(color = 'rgb(100,100,100)'),
                                          marker = list(color = 'rgb(100,100,100)'), alpha = 1,
                                          legendgroup=i, showlegend = FALSE,
                                          text = s,
                                          hoverinfo = "text")
          subplot = subplot %>% layout(xaxis= list(showticklabels = FALSE),
                                       yaxis = list(tickfont = list(size = 8)))
          i = i + 1
        }
        plot_list[[j]] = plotly_build(subplot)
        j = j + 1
      }

      fig = subplot(plot_list, nrows = y_dim, margin = 0.035, titleX = TRUE)
      fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                           annotations = annotations)

      self$plots$class_comparison = fig
    },

    ## Volcano plot
    plot_volcano = function(data_table = self$tables$volcano_table,
                            adjustment = self$params$volcano_plot$adjustment,
                            colour_list,
                            group_1 = self$params$volcano_plot$groups[1],
                            group_2 = self$params$volcano_plot$groups[2],
                            displayed_classes = self$params$volcano_plot$classes,
                            colouring = self$params$volcano_plot$colouring,
                            width = NULL,
                            height = NULL){

      adjustment = adjustment_switch(adjustment)

      # Select the colouring column
      feat_col = feature_table_cols_switch(colouring)

      # If null, display all classes
      if ((is.null(displayed_classes)) | any((displayed_classes == ""))) {
        displayed_classes = unique(data_table[, "lipid_class"])
      }

      # Filter out classes to skip
      removed_classes = setdiff(unique(data_table[, "lipid_class"]), displayed_classes)
      if (length(removed_classes) > 0) {
        del_rows = c()
        for (lipclass in removed_classes) {
          del_rows = c(del_rows, which(data_table[, "lipid_class"] == lipclass))
        }
        data_table = data_table[-del_rows,]
      }

      feature_vector = sort(unique(data_table[, feat_col]))

      max_fc = ceiling(max(abs(data_table[, "log2_fold_change"])))
      i = 1
      fig = plotly::plot_ly(colors = colour_list, type  = "scatter", mode  = "markers", width = width, height = height)


      for (feature in feature_vector) {
        tmp_idx = rownames(data_table)[data_table[, feat_col] == feature]
        fig = fig %>% add_trace(x = data_table[tmp_idx, "log2_fold_change"],
                                y = data_table[tmp_idx, adjustment],
                                name = feature,
                                color = colour_list[i],
                                text = tmp_idx,
                                hoverinfo = "text"
        )
        i = i + 1
      }
      fig = fig %>% layout(shapes = list(vline(x = -1, dash = "dot"), vline(x = 1, dash = "dot"), hline(-log10(0.05), dash = "dot")),
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
                            meta_table_features = self$tables$feature_table,
                            cluster_rows = self$params$heatmap$cluster_samples,
                            cluster_cols = self$params$heatmap$cluster_features,
                            row_annotations = self$params$heatmap$map_sample_data,
                            col_annotations = self$params$heatmap$map_feature_data,
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

        meta_table_features = meta_table_features[colnames(data_table),]
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

      # Reorder the feature metadata according to the data_table order
      meta_table_features = meta_table_features[c(colnames(data_table)),]

      if (!is.null(col_annotations)) {
        clean_names = col_annotations
        if (length(col_annotations) == 1) {
          col_annotations = feature_table_cols_switch(col_annotations)
          col_annotations = as.data.frame(meta_table_features[, col_annotations],
                                          row.names = rownames(meta_table_features))
          colnames(col_annotations) = clean_names
        } else {
          new_cols = c()
          for (i in 1:length(col_annotations)) {
            new_cols = c(new_cols, feature_table_cols_switch(col_annotations[i]))
          }
          col_annotations = meta_table_features[, new_cols]
          colnames(col_annotations) = clean_names
        }
      }

      if (impute) {
        print('Imputing NAs')
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
                                                row_side_colors = col_annotations,
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
      data_table = data_table[,!is.na(colSums(data_table, na.rm = T))]
      ncol_2 = ncol(data_table)
      if(ncol_2 != ncol_1) {
        print_time(paste0("PCA : dropped ", ncol_1 - ncol_2, " features with no signal variation."))
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
    },

    ## Double bond plot
    plot_doublebonds_single = function(data_table = self$tables$dbplot_table,
                                       lipid_class = self$params$db_plot$selected_lipid_class,
                                       carbon_selection = self$params$db_plot$selected_carbon_chain,
                                       unsat_selection = self$params$db_plot$selected_unsat,
                                       group_1 = self$params$db_plot$selected_groups[1],
                                       width = NULL,
                                       height = NULL){
      x_label = carbon_selection
      y_label = unsat_selection
      carbon_selection = feature_table_cols_switch(carbon_selection)
      unsat_selection = feature_table_cols_switch(unsat_selection)
      selected_rows = rownames(data_table)[data_table["lipid_class"] == lipid_class]
      data_table = data_table[selected_rows,]
      x_lims = c(min(data_table[,carbon_selection]) -1, max(data_table[,carbon_selection]) +1)
      y_lims = c(min(data_table[,unsat_selection]) -0.5, max(data_table[,unsat_selection]) +1)

      fig = plotly::plot_ly(data_table,
                            x = data_table[,carbon_selection],
                            y = data_table[,unsat_selection],
                            type = "scatter",
                            mode = "markers",
                            size = ~averages,
                            sizes = ~c(5,40),
                            marker = list(sizemode ='diameter',
                                          opacity = 0.5,
                                          sizeref=1
                            ),
                            text = data_table$text,
                            hoverinfo = "text",
                            width = width,
                            height = height)


      fig = fig %>% layout(
        title = paste0("Lipids in class ", lipid_class, " - ", group_1),
        xaxis = list(title = x_label,
                     range = x_lims
        ),
        yaxis = list(title = y_label,
                     range = y_lims
        )
      )
      self$plots$double_bond_plot = fig
    },

    plot_doublebonds_double = function(data_table = self$tables$dbplot_table,
                                       adjustment = self$params$db_plot$adjustment,
                                       carbon_selection = self$params$db_plot$selected_carbon_chain,
                                       unsat_selection = self$params$db_plot$selected_unsat,
                                       lipid_class = self$params$db_plot$selected_lipid_class,
                                       fc_limits = self$params$db_plot$fc_values,
                                       pval_limits = self$params$db_plot$pval_values,
                                       group_1 = self$params$db_plot$selected_groups[1],
                                       group_2 = self$params$db_plot$selected_groups[2],
                                       width = NULL,
                                       height = NULL){

      x_label = carbon_selection
      y_label = unsat_selection
      carbon_selection = feature_table_cols_switch(carbon_selection)
      unsat_selection = feature_table_cols_switch(unsat_selection)
      selected_rows = rownames(data_table)[data_table["lipid_class"] == lipid_class]
      data_table = data_table[selected_rows,]
      x_lims = c(min(data_table[,carbon_selection]) -1, max(data_table[,carbon_selection]) +1)
      y_lims = c(min(data_table[,unsat_selection]) -0.5, max(data_table[,unsat_selection]) +1)
      data_table = data_table[!dplyr::between(data_table[,"log2_fold_change"], fc_limits[1], fc_limits[2]),]
      data_table = data_table[dplyr::between(data_table[,adjustment], pval_limits[1], pval_limits[2]),]
      if (nrow(data_table) > 0) {
        fig = plotly::plot_ly(data_table,
                              x = data_table[,carbon_selection],
                              y = data_table[,unsat_selection],
                              type = "scatter",
                              mode = "markers",
                              size = data_table[,adjustment],
                              sizes = ~c(5,40),
                              marker = list(color = ~log2_fold_change,
                                            sizemode ='diameter',
                                            opacity = 0.5,
                                            sizeref=1,
                                            colorscale = 'RdBu',
                                            cmax = max(abs(data_table[, "log2_fold_change"])),
                                            cmin = -max(abs(data_table[, "log2_fold_change"])),
                                            colorbar=list(
                                              title='Log2(fold change)'
                                            ),
                                            line = list(width = 0)
                              ),
                              text = data_table$text,
                              hoverinfo = "text",
                              width = width,
                              height = height)
      } else {
        fig = plotly::plot_ly(data_table,
                              x = data_table[,carbon_selection],
                              y = data_table[,unsat_selection],
                              type = "scatter",
                              mode = "markers",
                              width = width,
                              height = height)
      }

      fig = fig %>% layout(
        legend= list(itemsizing='constant'),
        title = paste0("Comparison in ", lipid_class, " - ", group_1, " (blue), ", group_2, " (red)"),
        xaxis = list(title = x_label,
                     range = x_lims
        ),
        yaxis = list(title = y_label,
                     range = y_lims
        )
      )
      self$plots$double_bond_plot = fig
    }








  )
)
