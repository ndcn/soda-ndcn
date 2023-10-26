#---------------------------------------------- Proteomics experiment class ----
Prot_exp = R6::R6Class(
  "Prot_exp",
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
    type = 'Proteomics',
    preloaded_data = F,

    #----------------------------------------------------------- Parameters ----
    params = list(

      # Volcano plot parameters self$params$volcano_plot$
      volcano_plot = list(
        data_table = 'Total normalized table',
        adjustment = "Benjamini-Hochberg",
        group_col = NULL,
        groups = NULL,
        selected_function = "mean",
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
      ),

      #GSEA parameters self$params$gsea
      gsea = list(
        data_table = NULL,
        meta_table = NULL,
        group_col = NULL,
        groups = NULL,
        used_function = NULL,
        test = NULL,
        p_value_cutoff_prep = NULL,
        prot_list = NULL,
        ont = NULL,
        minGSSize = NULL,
        maxGSSize = NULL,
        p_value_cutoff = NULL,
        verbose = NULL,
        OrgDb = NULL,
        pAdjustMethod = NULL,
        termsim_method = NULL,
        termsim_showcat = NULL
      ),

      # Over representation analysis parameters self$params$overrepresentation
      overrepresentation = list(
        pval_cutoff_features = 0.05,
        padjust_features = "Benjamini-Hochberg",
        pval_cutoff = 0.05,
        pAdjustMethod = "BH",
        fc_threshold = 2,
        ont = "ALL",
        qval_cutoff = 0.05,
        minGSSize = 10,
        maxGSSize = 500
      ),

      # Dot plot parameters self$params$dot_plot
      dot_plot = list(
        showCategory = 10,
        mode = "Both",
        img_format = "png"
      ),

      # Ridge plot parameters self$params$ridge_plot
      ridge_plot = list(
        showCategory = 30,
        img_format = "png"
      ),

      # CNET plot parameters self$params$cnet_plot
      cnet_plot = list(
        showCategory = 3,
        enable_physics = TRUE
      ),

      # eMap plot parameters self$params$emap_plot
      emap_plot = list(
        showCategory = 20,
        color = "p.adjust" ,
        size = "Count",
        score_threshold = 0.2,
        similarity_score = 'JC',
        edge_magnifier = 1,
        node_magnifier = 0.1,
        enable_physics = FALSE
      ),

      # Over representation dot plot parameters self$params$or_dot_plot
      or_dot_plot = list(
        showCategory = 10,
        img_format = "png"
      ),

      # Over representation bar plot parameters self$params$or_bar_plot
      or_bar_plot = list(
        x = 'Count',
        color = 'p.adjust',
        showCategory = 10,
        img_format = "png"
      ),

      # Over representation CNET plot parameters self$params$or_cnet_plot
      or_cnet_plot = list(
        showCategory = 3,
        enable_physics = TRUE
      ),

      # Over representation eMap plot parameters self$params$or_emap_plot
      or_emap_plot = list(
        showCategory = 20,
        color = "p.adjust" ,
        size = "Count",
        score_threshold = 0.2,
        similarity_score = 'JC',
        edge_magnifier = 1,
        node_magnifier = 0.1,
        enable_physics = FALSE
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

      excluded_cols = NULL,

      feature_id_type = NULL
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
      pca_loadings_table = NULL,


      # GSEA & over representation
      gsea_prot_list = NULL,
      ora_prot_list = NULL,
      gsea_object = NULL,
      go_enrich = NULL


    ),

    #-------------------------------------------------------------- Local table

    table_switch_local = function(table_name) {
      switch(EXPR = table_name,
             'Imported metadata table' = self$tables$imp_meta,
             'Raw metadata table' = self$tables$raw_meta,
             'Imported data table' = self$tables$imp_data,
             'Raw data table' = self$tables$raw_data,
             'Feature table' = self$tables$feature_table,
             'Blank table' = self$tables$blank_table,
             'Class normalized table' = self$tables$class_norm_data,
             'Total normalized table' = self$tables$total_norm_data,
             'Z-scored table' = self$tables$z_scored_data,
             'Z-scored class normalized table' = self$tables$z_scored_class_norm_data,
             'Z-scored total normalized table' = self$tables$z_scored_total_norm_data,
             'Class table' = self$tables$class_table,
             'Class table z-scored' = self$tables$class_table_z_scored,
             'Class table total normalized' = self$tables$class_table_total_norm,
             'Class table z-scored total normalized' = self$tables$class_table_z_scored_total_norm,
             'Species summary table' = self$tables$summary_species_table,
             'Class summary table' = self$tables$summary_class_table,
             'GSEA prot list' = self$tables$gsea_prot_list,
             'ORA prot list' = self$tables$ora_prot_list
      )
    },

    #---------------------------------------------------------------- Plots ----
    plots = list(
      volcano_plot = NULL,
      heatmap = NULL,
      pca_plot = NULL,
      dotplot = NULL,
      ridgeplot = NULL,
      emap_plot = NULL,
      cnetplot = NULL,
      or_dotplot = NULL,
      or_emap_plot = NULL,
      or_cnetplot = NULL,
      or_barplot = NULL
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

    param_gsea = function(data_table, meta_table, group_col, groups, used_function, test,
                          p_value_cutoff_prep, prot_list, ont, minGSSize, maxGSSize, p_value_cutoff,
                          verbose, OrgDb, pAdjustMethod, termsim_method, termsim_showcat) {
      self$params$gsea$data_table = data_table
      self$params$gsea$meta_table = meta_table
      self$params$gsea$group_col = group_col
      self$params$gsea$groups = groups
      self$params$gsea$used_function = used_function
      self$params$gsea$test = test
      self$params$gsea$p_value_cutoff_prep = p_value_cutoff_prep
      self$params$gsea$prot_list = prot_list
      self$params$gsea$ont = ont
      self$params$gsea$minGSSize = minGSSize
      self$params$gsea$maxGSSize = maxGSSize
      self$params$gsea$p_value_cutoff = p_value_cutoff
      self$params$gsea$verbose = verbose
      self$params$gsea$OrgDb = OrgDb
      self$params$gsea$pAdjustMethod = pAdjustMethod
      self$params$gsea$termsim_method = termsim_method
      self$params$gsea$termsim_showcat = termsim_showcat
    },

    param_overrepresentation = function(pval_cutoff_features, padjust_features, pval_cutoff, fc_threshold,
                                        pAdjustMethod, ont, qval_cutoff, minGSSize, maxGSSize) {
      self$params$overrepresentation$pval_cutoff_features = pval_cutoff_features
      self$params$overrepresentation$padjust_features = padjust_features
      self$params$overrepresentation$pval_cutoff = pval_cutoff
      self$params$overrepresentation$pAdjustMethod = pAdjustMethod
      self$params$overrepresentation$fc_threshold = fc_threshold
      self$params$overrepresentation$ont = ont
      self$params$overrepresentation$qval_cutoff = qval_cutoff
      self$params$overrepresentation$minGSSize = minGSSize
      self$params$overrepresentation$maxGSSize = maxGSSize

    },

    param_dot_plot = function(showCategory, mode, img_format) {
      self$params$dot_plot$showCategory = showCategory
      self$params$dot_plot$mode = mode
      self$params$dot_plot$img_format = img_format
    },

    param_ridge_plot = function(showCategory, img_format) {
      self$params$ridge_plot$showCategory = showCategory
      self$params$ridge_plot$img_format = img_format
    },

    param_cnet_plot = function(showCategory, enable_physics) {
      self$params$cnet_plot$showCategory = showCategory
      self$params$cnet_plot$enable_physics = enable_physics
    },

    param_emap_plot = function(showCategory, color, size, score_threshold, similarity_score, edge_magnifier, node_magnifier, enable_physics) {
      self$params$emap_plot$showCategory = showCategory
      self$params$emap_plot$color = color
      self$params$emap_plot$size = size
      self$params$emap_plot$score_threshold = score_threshold
      self$params$emap_plot$similarity_score = similarity_score
      self$params$emap_plot$edge_magnifier = edge_magnifier
      self$params$emap_plot$node_magnifier = node_magnifier
      self$params$emap_plot$enable_physics = enable_physics
    },

    param_or_dot_plot = function(showCategory, img_format) {
      self$params$or_dot_plot$showCategory = showCategory
      self$params$or_dot_plot$img_format = img_format
    },

    param_or_bar_plot = function(x, color, showCategory, img_format) {
      self$params$or_bar_plot$x = x
      self$params$or_bar_plot$color = color
      self$params$or_bar_plot$showCategory = showCategory
      self$params$or_bar_plot$img_format = img_format
    },

    param_or_cnet_plot = function(showCategory, enable_physics) {
      self$params$or_cnet_plot$showCategory = showCategory
      self$params$or_cnet_plot$enable_physics = enable_physics
    },

    param_or_emap_plot = function(showCategory, color, size, score_threshold, similarity_score, edge_magnifier, node_magnifier, enable_physics) {
      self$params$or_emap_plot$showCategory = showCategory
      self$params$or_emap_plot$color = color
      self$params$or_emap_plot$size = size
      self$params$or_emap_plot$score_threshold = score_threshold
      self$params$or_emap_plot$similarity_score = similarity_score
      self$params$or_emap_plot$edge_magnifier = edge_magnifier
      self$params$or_emap_plot$node_magnifier = node_magnifier
      self$params$or_emap_plot$enable_physics = enable_physics
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
                              selected_function = 'mean',
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

      self$indices$feature_id_type = 'SYMBOL'

      self$param_gsea(data_table = 'Raw data table',
                      meta_table = 'Raw metadata table',
                      group_col = self$indices$group_col,
                      groups = unique(self$tables$raw_meta[,self$indices$group_col])[c(1,2)],
                      used_function = "median",
                      test = "t-Test",
                      p_value_cutoff_prep = 0.05,
                      prot_list = 'GSEA prot list',
                      ont = 'ALL',
                      minGSSize = 3,
                      maxGSSize = 800,
                      p_value_cutoff = 0.05,
                      verbose = TRUE,
                      OrgDb = "org.Hs.eg.db",
                      pAdjustMethod = 'BH',
                      termsim_method = 'JC',
                      termsim_showcat = 200)

      self$param_overrepresentation(pval_cutoff_features = 0.05,
                                    padjust_features = 'Benjamini-Hochberg',
                                    pval_cutoff = 0.05,
                                    pAdjustMethod = "BH",
                                    fc_threshold = 2,
                                    ont = "ALL",
                                    qval_cutoff = 0.05,
                                    minGSSize = 10,
                                    maxGSSize = 500)

      self$param_dot_plot(showCategory = 10,
                          mode = "Both",
                          img_format = "png")

      self$param_ridge_plot(showCategory = 30,
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

      volcano_table = data.frame(matrix(data = NA, nrow = ncol(data_table), ncol = 0))
      rownames(volcano_table) = colnames(data_table)

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

    #---------------------------------------------------- GSEA & OR methods ----

    # GSEA table
    get_prot_list = function(data_table = self$table_switch_local(self$params$gsea$data_table),
                             meta_table = self$table_switch_local(self$params$gsea$meta_table),
                             group_col = self$params$gsea$group_col,
                             group_1 = self$params$gsea$groups[1],
                             group_2 = self$params$gsea$groups[2],
                             used_function = self$params$gsea$used_function,
                             test = self$params$gsea$test,
                             context = 'gsea'
    ) {





      rownames_group_1 = rownames(meta_table)[meta_table[, group_col] == group_1]
      rownames_group_2 = rownames(meta_table)[meta_table[, group_col] == group_2]
      all_rownames = sort(unique(c(rownames_group_1, rownames_group_2)))

      # Filter data to keep only the two groups
      data_table = data_table[all_rownames,]

      # Get the indices for each group
      idx_group_1 = which(rownames(data_table) %in% rownames_group_1)
      idx_group_2 = which(rownames(data_table) %in% rownames_group_2)


      # Remove empty columns
      data_table = remove_empty_cols(table = data_table)


      prot_list = data.frame(row.names = colnames(data_table))


      # Collect fold changes
      prot_list$fold_change = get_fold_changes(data_table = data_table,
                                               idx_group_1 = idx_group_1,
                                               idx_group_2 = idx_group_2,
                                               used_function = used_function)


      prot_list$log2_fold_change = log2(prot_list$fold_change)

      # Collect p-values
      prot_list$p_val = get_p_val(data_table = data_table,
                                  idx_group_1 = idx_group_1,
                                  idx_group_2 = idx_group_2,
                                  used_function = test)

      prot_list$minus_log10_p_value = -log10(prot_list$p_val)
      prot_list$p_val_bh = stats::p.adjust(prot_list$p_val, method = "BH")
      prot_list$minus_log10_p_value_bh_adj = -log10(prot_list$p_val_bh)

      if (context == 'gsea') {
        self$tables$gsea_prot_list = prot_list
      } else if (context == 'ora') {
        self$tables$ora_prot_list = prot_list
      }
    },


    # GSEA object
    get_gsea_object = function(prot_list = self$table_switch_local(self$params$gsea$prot_list),
                               keyType = self$indices$feature_id_type,
                               ont = self$params$gsea$ont,
                               minGSSize = self$params$gsea$minGSSize,
                               maxGSSize = self$params$gsea$maxGSSize,
                               p_value_cutoff = self$params$gsea$p_value_cutoff,
                               verbose = self$params$gsea$verbose,
                               OrgDb = self$params$gsea$OrgDb,
                               pAdjustMethod = self$params$gsea$pAdjustMethod,
                               termsim_method = self$params$gsea$termsim_method,
                               termsim_showcat = self$params$gsea$termsim_showcat) {

      prot_names = rownames(prot_list)
      prot_list = prot_list$log2_fold_change
      names(prot_list) = prot_names

      # NA omit and sort
      prot_list = na.omit(prot_list)
      prot_list = sort(prot_list, decreasing = TRUE)

      gsea = clusterProfiler::gseGO(geneList=prot_list,
                                    ont = ont,
                                    keyType = keyType,
                                    minGSSize = minGSSize,
                                    maxGSSize = maxGSSize,
                                    pvalueCutoff = p_value_cutoff,
                                    verbose = verbose,
                                    OrgDb = OrgDb,
                                    pAdjustMethod = pAdjustMethod)

      if (nrow(gsea@result) > 0) {
        gsea = enrichplot::pairwise_termsim(gsea, method = termsim_method, semData = NULL, showCategory = termsim_showcat)
      }
      self$tables$gsea_object = gsea

    },

    over_representation_analysis = function(prot_list = self$tables$ora_prot_list,
                                            pval_cutoff_features = self$params$overrepresentation$pval_cutoff_features,
                                            padjust_features = self$params$overrepresentation$padjust_features,
                                            pval_cutoff = self$params$overrepresentation$pval_cutoff,
                                            pAdjustMethod = self$params$overrepresentation$pAdjustMethod,
                                            fc_threshold = self$params$overrepresentation$fc_threshold,
                                            keyType = self$indices$feature_id_type,
                                            ont = self$params$overrepresentation$ont,
                                            qval_cutoff = self$params$overrepresentation$qval_cutoff,
                                            minGSSize = self$params$overrepresentation$minGSSize,
                                            maxGSSize  = self$params$overrepresentation$maxGSSize) {


      # Get universe (all features)
      universe = prot_list$log2_fold_change
      names(universe) = rownames(prot_list)
      universe = na.omit(universe)
      universe = sort(universe, decreasing = TRUE)
      universe = names(universe)

      # Get significant features
      if (padjust_features == "Benjamini-Hochberg") {
        features = prot_list[prot_list$p_val_bh <= pval_cutoff_features,]
      } else {
        features = prot_list[prot_list$p_val <= pval_cutoff_features,]
      }
      features = features[abs(features$log2_fold_change) >= log2(fc_threshold),]
      features = rownames(features)


      if (length(features) == 0) {
        return()
      }

      go_enrich = clusterProfiler::enrichGO(gene = features,
                                            universe = universe,
                                            OrgDb = 'org.Hs.eg.db',
                                            keyType = keyType,
                                            readable = T,
                                            ont = ont,
                                            pvalueCutoff = pval_cutoff,
                                            pAdjustMethod = pAdjustMethod,
                                            qvalueCutoff = qval_cutoff,
                                            minGSSize = minGSSize,
                                            maxGSSize  = maxGSSize)

      self$tables$go_enrich = go_enrich
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
        if (length(row_annotations) > 1) {
          row_annotations = meta_table[rownames(data_table), row_annotations]
          colnames(row_annotations) = stringr::str_replace_all(colnames(row_annotations), "_", " ")
        } else {
          row_names = row_annotations
          row_annotations = as.data.frame(meta_table[rownames(data_table), row_annotations],
                                          row.names = rownames(data_table))

          colnames(row_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
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
    },

    plot_dot_plot = function(object = self$tables$gsea_object,
                             x = "GeneRatio",
                             color = "p.adjust",
                             showCategory = self$params$dot_plot$showCategory,
                             size = NULL,
                             split = ".sign",
                             orderBy="x",
                             mode = self$params$dot_plot$mode,
                             width = NULL,
                             height = NULL){

      if (is.na(showCategory)) {
        base::warning("Invalid showCategory, setting to 10 by default")
        showCategory = 10
      }

      colorBy <- match.arg(color, c("pvalue", "p.adjust", "qvalue"))
      if (x == "geneRatio" || x == "GeneRatio") {
        x <- "GeneRatio"
        if (is.null(size))
          size <- "Count"
      } else if (x == "count" || x == "Count") {
        x <- "Count"
        if (is.null(size))
          size <- "GeneRatio"
      } else if (is(x, "formula")) {
        x <- as.character(x)[2]
        if (is.null(size))
          size <- "Count"
      } else {
        if (is.null(size))
          size  <- "Count"
      }

      if (inherits(object, c("enrichResultList", "gseaResultList"))) {
        ldf <- lapply(object, fortify, showCategory=showCategory, split=split)
        df <- dplyr::bind_rows(ldf, .id="category")
        df$category <- factor(df$category, levels=names(object))
      } else {
        df <- fortify(object, showCategory = showCategory, split=split)
      }

      if (orderBy !=  'x' && !orderBy %in% colnames(df)) {
        message('wrong orderBy parameter; set to default `orderBy = "x"`')
        orderBy <- "x"
      }

      if (orderBy == "x") {
        df <- dplyr::mutate(df, x = eval(parse(text=x)))
      }

      df$hover = paste0(
        paste0(df[,"Description"], "\n"),
        paste0("GeneRatio:", as.character(round(df[,"x"],2)), "\n"),
        paste0(size, ": ", as.character(df[,size]), "\n"),
        paste0(colorBy, ": ", as.character(round(df[,colorBy],5)), "\n"),
        df$.sign
      )

      df[,"Description"] = as.character(df[,"Description"])

      if (mode == "Activated") {
        df = df[df$.sign == "activated",]
        trace_hline = FALSE
      } else if (mode == "Suppressed") {
        df = df[df$.sign == "suppressed",]
        trace_hline = FALSE
      } else if (mode == "Both") {
        mode = "Activated (top) - Suppressed (bottom)"
        trace_hline = TRUE
      } else {
        warning("Invalid mode, setting to 'Both' by default")
        mode = "Activated (top) - Suppressed (bottom)"
        trace_hline = TRUE
      }


      fig = plotly::plot_ly(data = df,
                            x = ~x,
                            y = df[,"Description"],
                            size = df[,size],
                            type = "scatter",
                            mode = "markers",
                            marker = list(color = df[,colorBy],
                                          sizemode ='diameter',
                                          opacity = 0.5,
                                          sizeref=1,
                                          colorscale = 'RdBu',
                                          colorbar=list(
                                            title=colorBy
                                          ),
                                          line = list(width = 0),
                                          cmax = max(df[, colorBy]),
                                          cmin = min(df[, colorBy])
                            ),
                            text = df$hover,
                            hoverinfo = "text",
                            width = width,
                            height = height
      )
      fig = fig %>% layout(
        legend= list(itemsizing='constant'),
        title = mode,
        xaxis = list(title = 'GeneRatio'),
        yaxis = list(title =  NA,
                     categoryorder = "array",
                     categoryarray = base::rev(df[,"Description"]))
      )
      if (trace_hline) {
        fig = fig %>% layout(
          shapes = list(hline(showCategory - 0.5))
        )
      }
      print_tm(self$name, "Dot plot completed")
      self$plots$dotplot = fig
    },

    plot_or_dot_plot = function(object = self$tables$go_enrich,
                                x = "GeneRatio",
                                color = "p.adjust",
                                showCategory = self$params$or_dot_plot$showCategory,
                                size = NULL,
                                split = NULL,
                                orderBy="x",
                                width = NULL,
                                height = NULL){

      colorBy <- match.arg(color, c("pvalue", "p.adjust", "qvalue"))
      if (x == "geneRatio" || x == "GeneRatio") {
        x <- "GeneRatio"
        if (is.null(size))
          size <- "Count"
      } else if (x == "count" || x == "Count") {
        x <- "Count"
        if (is.null(size))
          size <- "GeneRatio"
      } else if (is(x, "formula")) {
        x <- as.character(x)[2]
        if (is.null(size))
          size <- "Count"
      } else {
        ## message("invalid x, setting to 'GeneRatio' by default")
        ## x <- "GeneRatio"
        ## size <- "Count"
        if (is.null(size))
          size  <- "Count"
      }

      if (inherits(object, c("enrichResultList", "gseaResultList"))) {
        ldf <- lapply(object, fortify, showCategory=showCategory, split=split)
        df <- dplyr::bind_rows(ldf, .id="category")
        df$category <- factor(df$category, levels=names(object))
      } else {
        df <- fortify(object, showCategory = showCategory, split=split)
        ## already parsed in fortify
        ## df$GeneRatio <- parse_ratio(df$GeneRatio)
      }

      if (orderBy !=  'x' && !orderBy %in% colnames(df)) {
        message('wrong orderBy parameter; set to default `orderBy = "x"`')
        orderBy <- "x"
      }

      if (orderBy == "x") {
        df <- dplyr::mutate(df, x = eval(parse(text=x)))
      }


      df$hover = paste0(
        paste0(df[,"Description"], "\n"),
        paste0("GeneRatio:", as.character(round(df[,"x"],2)), "\n"),
        paste0(size, ": ", as.character(df[,size]), "\n"),
        paste0(colorBy, ": ", as.character(round(df[,colorBy],5)), "\n"),
        df$.sign
      )

      df[,"Description"] = as.character(df[,"Description"])



      fig = plotly::plot_ly(data = df,
                            x = df$GeneRatio,
                            y = df[,"Description"],
                            size = df[,size],
                            type = "scatter",
                            mode = "markers",
                            marker = list(color = df[,colorBy],
                                          sizemode ='diameter',
                                          opacity = 0.5,
                                          sizeref=1,
                                          colorscale = 'RdBu',
                                          colorbar=list(
                                            title=colorBy
                                          ),
                                          line = list(width = 0),
                                          cmax = max(df[, colorBy]),
                                          cmin = min(df[, colorBy])
                            ),
                            text = df$hover,
                            hoverinfo = "text",
                            width = width,
                            height = height
      )
      fig = fig %>% layout(
        legend= list(itemsizing='constant'),
        title = mode,
        xaxis = list(title = 'GeneRatio'),
        yaxis = list(title =  NA,
                     categoryorder = "array",
                     categoryarray = base::rev(df[,"Description"]))
      )

      print_tm(self$name, "Dot plot completed")
      self$plots$or_dotplot = fig

    },

    plot_cnet_plot = function(x = self$tables$gsea_object,
                              showCategory = self$params$cnet_plot$showCategory,
                              enable_physics = self$params$cnet_plot$enable_physics,
                              context = "gsea") {

      showCategory = as.numeric(showCategory)

      if (context == 'gsea') {
        prot_list = self$tables$gsea_prot_list
      } else if (context == 'ora') {
        prot_list = self$tables$ora_prot_list
      }


      geneSets = enrichplot:::extract_geneSets(x, showCategory)

      main_nodes = names(geneSets)

      secondary_nodes = c()
      for (n in geneSets) {
        secondary_nodes = c(secondary_nodes, n)
      }
      secondary_nodes = sort(unique(secondary_nodes))



      all_nodes = c(main_nodes, secondary_nodes)

      node_table = data.frame(matrix(nrow = length(all_nodes), ncol = 1))
      colnames(node_table) = c("id")
      node_table$id = all_nodes
      node_table$label = all_nodes
      node_table$shape = rep("dot", nrow(node_table))

      feature_values = prot_list[all_nodes,]
      rownames(feature_values) = rownames(node_table)

      node_table = base::cbind(node_table, feature_values)

      normalized_values = scales::rescale(feature_values$log2_fold_change, to = c(0, 1))

      color_gradient = grDevices::colorRampPalette(c("blue", "white", "red"))
      hex_colors = color_gradient(100)[round(normalized_values * 99) + 1]
      hex_colors[which(is.na(hex_colors))] = "#FFD800"
      node_table$color = hex_colors



      source_nodes = c()
      target_nodes = c()
      for (n in main_nodes) {
        target_nodes = c(target_nodes, geneSets[[n]])
        source_nodes = c(source_nodes, rep(n, length(geneSets[[n]])))
      }

      edge_table = data.frame(matrix(nrow = length(target_nodes), ncol = 2))
      colnames(edge_table) = c("from", "to")
      edge_table$from = source_nodes
      edge_table$to = target_nodes
      edge_table$width = rep(1, nrow(edge_table))

      plot = visNetwork::visNetwork(node_table, edge_table)
      plot = visNetwork::visPhysics(plot, enabled = enable_physics)


      if (context == "gsea") {
        self$plots$cnetplot = plot
      } else if (context == "ora") {
        self$plots$or_cnetplot = plot
      }
    },

    plot_ridge_plot = function(x = self$tables$gsea_object,
                               showCategory = self$params$dot_plot$showCategory,
                               fill="p.adjust",
                               core_enrichment = TRUE,
                               orderBy = "NES",
                               decreasing = FALSE,
                               width = NULL,
                               height = NULL) {

      print_tm(self$name, "Ridgeplot initiated")

      if (is.na(showCategory)) {
        base::warning("Invalid showCategory, setting to 30 by default")
        showCategory = 30
      }

      n = showCategory
      if (core_enrichment) {
        gs2id = geneInCategory(x)[seq_len(n)]
      } else {
        gs2id = x@geneSets[x$ID[seq_len(n)]]
      }

      if (x@readable && length(x@gene2Symbol) > 0) {
        id = match(names(x@geneList), names(x@gene2Symbol))
        names(x@geneList) = x@gene2Symbol[id]
      }

      gs2val = lapply(gs2id, function(id) {
        res = x@geneList[id]
        res = res[!is.na(res)]
      })

      nn = names(gs2val)
      i = match(nn, x$ID)
      nn = x$Description[i]

      j = order(x@result[[orderBy]][i], decreasing = decreasing)
      len = sapply(gs2val, length)
      gs2val.df = data.frame(category = rep(nn, times=len),
                             color = rep(x[i, fill], times=len),
                             value = unlist(gs2val))

      colnames(gs2val.df)[2] = fill
      gs2val.df$category = factor(gs2val.df$category, levels=nn[j])

      xdata = na.omit(data.frame(x=gs2val.df$value, group=gs2val.df$category))
      xs = split(xdata$x, xdata$group)
      xs_mask = vapply(xs, length, numeric(1)) > 1
      bws = vapply(xs[xs_mask], bw.nrd0, numeric(1))
      bw = mean(bws, na.rm = TRUE)

      all_traces = levels(gs2val.df$category)
      total_seq = seq(floor(min(gs2val.df$value)),ceiling(max(gs2val.df$value)), by=bw)

      col_values_hex = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 9, name = 'YlOrRd'))(length(unique(gs2val.df[,"p.adjust"])))
      col_values = c()
      for (col in col_values_hex){
        col_values = c(col_values, paste0("rgba(",paste(as.vector(col2rgb(col)), collapse = ","), ",0.5)"))
      }
      names(col_values) = seq(1, length(col_values), by = 1)
      col_pvals = sort(unique(gs2val.df[,"p.adjust"]))

      p = plotly::plot_ly(width = width,
                          height = height)
      incr = 0
      for (trace in all_traces) {
        tmp_table = gs2val.df[gs2val.df[,"category"] == trace,]
        fill_value = tmp_table[1, "p.adjust"]
        fill_col = col_values[which(col_pvals == fill_value)]

        tmp_table = as.data.frame(table(cut(tmp_table$value, breaks=total_seq)))
        tmp_table$Var1 = gsub("\\(|]", "", levels(tmp_table$Var1))
        x_values = c()
        for (l in tmp_table$Var1) {
          x_values = c(x_values, mean(as.numeric(stringr::str_split(l, ",")[[1]])))
        }
        tmp_table$Var1 = x_values

        tmp_table$text = paste0(trace, ":\n", "Count: ", tmp_table$Freq, "\n", fill, ": ", fill_value, "\n", "x: ", round(tmp_table$Var1,2))

        tmp_table$Freq = tmp_table$Freq / max(tmp_table$Freq) + incr


        # if ((tmp_table$Freq[nrow(tmp_table)] - incr) > 0) {
        #   tmp_table = base::rbind(tmp_table, c(tmp_table$Var1[tmp_table] +0.5, incr, paste0(trace, ":\n", "Count: ", "0", "\n", fill, ": ", fill_value, "\n", "x: ", round(tmp_table$Var1[1] -1,2))))
        # }
        # if ((tmp_table$Freq[1] - incr) > 0) {
        #   tmp_table = rbind(c(tmp_table$Var1[1] -0.5, incr, paste0(trace, ":\n", "Count: ", "0", "\n", fill, ": ", fill_value, "\n", "x: ", round(tmp_table$Var1[1] -1,2))),
        #                     tmp_table)
        # }



        p = add_trace(p,
                      line = list(
                        color = "#FFFFFF",
                        width = 0.1
                      ),
                      mode = "lines",
                      type = "scatter",
                      x = c(0, max(gs2val.df$value)),
                      y = c(incr-0.01, incr-0.01),
                      legendgroup=0,
                      showlegend = F)

        incr = incr + 1

        p = add_trace(p,
                      fill = "tonexty",
                      line = list(color = "#000000",
                                  width = 0.5,
                                  shape = "spline",
                                  smoothing = 1.3),
                      mode = "lines",
                      type = "scatter",
                      x=tmp_table$Var1,
                      y=tmp_table$Freq,
                      name = trace,
                      fillcolor = fill_col,
                      text = tmp_table$text,
                      hoverinfo = "text",
                      legendgroup=0,
                      showlegend = F)

      }

      p = add_trace(p,
                    x = col_pvals,
                    y = col_pvals,
                    type = "scatter",
                    mode = "markers",
                    marker = list(
                      color = col_pvals,
                      colorscale = "YlOrRd",
                      colorbar=list(title = fill),
                      size = 1,
                      opacity = 0.1),
                    legendgroup=0,
                    showlegend = F
      )


      p = layout(p ,
                 showlegend = T,
                 yaxis = list(
                   type = "linear",
                   range = c(0, length(all_traces)),
                   ticklen = 4,
                   showgrid = TRUE,
                   showline = FALSE,
                   ticktext = all_traces,
                   tickvals = seq(from = 0, to = length(all_traces)-1, by = 1),
                   zeroline = FALSE,
                   gridcolor = "rgb(255,255,255)",
                   gridwidth = 1
                 ),
                 xaxis = list(title = "Log2(fold change)"))

      self$plots$ridgeplot = p
    },

    plot_emap_plot = function(x = self$tables$gsea_object,
                              showCategory = 20,
                              color = "p.adjust",
                              size = "Count",
                              score_threshold = 0.2,
                              similarity_score = 'JC',
                              edge_magnifier = 1,
                              node_magnifier = 5,
                              enable_physics = FALSE,
                              context = "gsea") {

      # Format data
      showCategory = as.numeric(showCategory)
      score_threshold = as.numeric(score_threshold)
      edge_magnifier = as.numeric(edge_magnifier)
      node_magnifier = as.numeric(node_magnifier)

      # Adding data based on GSEA or ORA
      if (context == 'gsea') {
        total_count = x@result$setSize
        count = lapply(x@result$core_enrichment, function(x) {
          elements = unlist(strsplit(x, "/"))
          length(elements)
        })
        count = unlist(count)
        x@result$Count = count
        x@result$total_count = total_count
        x@result$gene_ratio = round(count/total_count, 2)
      } else if (context == 'or') {
        total_count = base::strsplit(x@result$GeneRatio, '/')
        total_count = as.numeric(sapply(total_count, "[[", 2))
        x@result$total_count = total_count
        x@result$gene_ratio = round(x@result$Count / x@result$total_count, 2)
      }


      # Calculate similarities
      x = enrichplot::pairwise_termsim(x= x,
                                       method = similarity_score,
                                       semData = NULL,
                                       showCategory = showCategory)

      # Get the igraph object
      g = enrichplot:::get_igraph(x=x,
                                  nCategory=showCategory,
                                  color=color,
                                  cex_line=edge_magnifier,
                                  min_edge=score_threshold)



      # Extract data from the igraph object
      edge_table = igraph::as_data_frame(g, what = "edges")
      if (length(edge_table) > 0) {
        edge_table$color = "gray"
      }
      node_table = igraph::as_data_frame(g, what = "vertices")
      node_table = cbind(node_table$name, node_table)
      colnames(node_table) = c("id", "label", "size", "color_values")
      node_table$size = x@result[[size]][1:showCategory]
      node_table$shape = "dot"
      rownames(node_table) = 1:nrow(node_table)
      node_table$size = node_table$size * node_magnifier

      # Setting the color gradient
      normalized_values = scales::rescale(node_table$color_values , to = c(0, 1))
      color_gradient = grDevices::colorRampPalette(c("red", "white", "blue"))
      hex_colors = color_gradient(100)[round(normalized_values * 99) + 1]
      node_table$color = hex_colors

      # Produce the network object
      plot = visNetwork::visNetwork(node_table, edge_table)
      plot = visNetwork::visPhysics(plot, enabled = enable_physics)

      ### Store the network object
      if (context == "gsea") {
        self$plots$emap_plot = plot
      } else if (context == "or") {
        self$plots$or_emap_plot = plot
      }
    },

    plot_or_bar_plot = function(object = self$tables$go_enrich,
                                x = self$params$or_bar_plot$x,
                                color = self$params$or_bar_plot$color,
                                showCategory = self$params$or_bar_plot$showCategory,
                                width = NULL,
                                height = NULL) {

      colorBy <- match.arg(color, c("pvalue", "p.adjust", "qvalue"))
      if (x == "geneRatio" || x == "GeneRatio") {
        x <- "GeneRatio"
      } else if (x == "count" || x == "Count") {
        x <- "Count"
      }

      df <- fortify(object, showCategory=showCategory, by=x)

      fig = plotly::plot_ly(df,
                            x = ~Count,
                            y = df$Description,
                            type = 'bar',
                            orientation = 'h',
                            marker = list(
                              colorscale = list(c(0,1), c("red", "blue")),
                              colorbar = list(title = "p.adjust"),
                              color = ~p.adjust),
                            width = width,
                            height = height) %>%
        layout(xaxis = list(title = 'Count')
        )
      self$plots$or_barplot = fig
    }
    #------------------------------------------------------------------ END ----
  )
)

