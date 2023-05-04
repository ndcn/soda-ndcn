#---------------------------- Class Prot_data ---------------------------------
Prot_data = R6::R6Class(
  "Prot_data",
  public = list(
    #--------------------------------------------------------------- Global ----
    name = NULL,
    type = NULL,
    non_unique_ids_meta = FALSE,
    non_unique_ids_data = FALSE,
    
    #---------------------------------------------------------------- Texts ----
    texts = list(
      
      ### Columns
      col_id_meta = NULL,
      col_id_data = NULL,
      col_group = NULL
    ),
    
    #--------------------------------------------------------------- Tables ----
    tables = list(
      
      # Raw
      meta_raw = NULL,
      data_raw = NULL,
      
      # Filtered
      meta_filtered = NULL,
      data_filtered = NULL,
      
      # Normalised
      data_total_norm = NULL,
      
      # Z-scored
      data_z_scored = NULL,
      data_total_norm_z_scored = NULL,
      
      # Plot tables
      volcano_table = NULL,
      heatmap_table = NULL,
      pca_scores_table = NULL,
      pca_loadings_table = NULL,
      
      # GSEA tables
      prot_list = NULL,
      gsea_object = NULL
      
    ),
    
    #---------------------------------------------------------------- Plots ----
    plots = list(
      volcano_plot = NULL,
      heatmap = NULL,
      pca_plot = NULL,
      dotplot = NULL,
      emapplot = NULL,
      cnetplot = NULL,
      ridgeplot = NULL
    ),
    
    ### Methods
    initialize = function(name = NA, type = NA){
      self$name = name
      self$type = type
    },
    
    #----------------------------------------------------------- Parameters ----
    params = list(

      # Volcano plot parameters
      volcano_plot = shiny::reactiveValues(
        data_table = "Total normalised data table",
        adjustment = "Benjamini-Hochberg",
        group_column = NULL,
        groups = NULL,
        selected_function = "median",
        selected_test = "Wilcoxon"
      ),
      
      # Heatmap parameters
      heatmap = shiny::reactiveValues(
        dataset = "Z-scored total normalised data table",
        clustering = NULL,
        map_sample_data = character(0),
        map_feature_data = character(0),
        percentile = 95,
        group_column_da = NULL,
        apply_da = TRUE,
        alpha_da = 0.8
      ),
      
      # PCA parameters
      pca = shiny::reactiveValues(
        dataset = "Z-scored total normalised data table",
        group_column = NULL,
        apply_da = FALSE,
        alpha_da = 0.8
      )
    ),
    
    
    #--------------------------------------------------------- Text methods ----
    
    # Class data
    set_name = function(val) {
      self$name = val
    },
    set_type = function(val) {
      self$type = val
    },
    
    # Column attributes
    set_col = function(col, type) {
      if (type == "group"){
        self$texts$col_group = col
      } else if (type == "id_meta") {
        self$texts$col_id_meta = col
      } else if (type == "id_data") {
        self$texts$col_id_data = col
      }
    },
    
    #-------------------------------------------------------- Table methods ----
    # Set raw metadata
    set_raw_meta = function(val) {
      self$tables$meta_raw = val
    },
    
    # Set raw data
    set_raw_data = function(val) {
      self$tables$data_raw = val
    },
    
    # Set or reset the filtered data
    set_data_filtered = function(id_col = self$texts$col_id_data) {
      
      # Check if non-unique IDs
      if (length(self$tables$data_raw[, id_col]) != length(unique(self$tables$data_raw[, id_col]))){
        self$tables$data_filtered = NULL
        self$non_unique_ids_data = TRUE
        return()
      }

      # If ID column is correct
      self$non_unique_ids_data = FALSE
      table = self$tables$data_raw
      rownames(table) = table[,id_col]
      table = table[,-which(colnames(table) == id_col)]
      # if there is a meta_filtered table, keep only rows from there
      if (!is.null(self$tables$meta_filtered)) {
        table = table[rownames(self$tables$meta_filtered),]
        table = remove_empty_cols(table)
      }
      
      # Coerce to numeric matrix and save
      table = as.matrix(table)
      class(table) = "numeric"
      self$tables$data_filtered = table
    },
    
    # Set or reset the filtered Metadata
    set_meta_filtered = function(id_col = self$texts$col_id_meta) {
      # Creates the most basic filtered metadata: raw metadata with an ID column
      
      # First, checks if the ID column contains only unique values (if not, error)
      if (length(self$tables$meta_raw[, id_col]) != length(unique(self$tables$meta_raw[, id_col]))){
        self$tables$meta_filtered = NULL
        self$non_unique_ids_meta = TRUE
        return()
      }
      
      # If unique IDs, proceed
      self$non_unique_ids_meta = FALSE
      table = self$tables$meta_raw
      rownames(table) = table[,id_col]
      table = table[,-which(colnames(table) == id_col)]
      self$tables$meta_filtered = table
      
    },
    
    # Volcano table
    get_volcano_table = function(data_table = self$tables$data_filtered,
                                 col_group = self$texts$col_group,
                                 used_function = "median",
                                 test = "Wilcoxon",
                                 group_1,
                                 group_2) {

      volcano_table = data.frame(matrix(data = NA, nrow = ncol(data_table), ncol = 0))
      
      # Get the rownames for each group
      idx_group_1 = rownames(self$tables$meta_filtered)[self$tables$meta_filtered[, col_group] == group_1]
      idx_group_2 = rownames(self$tables$meta_filtered)[self$tables$meta_filtered[, col_group] == group_2]
      
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
    
    # GSEA table
    get_prot_list = function(data_table = self$tables$data_filtered,
                             meta_table = self$tables$meta_filtered,
                             col_group,
                             group_1,
                             group_2,
                             used_function = "median",
                             test = "T-test",
                             p_value_cutoff = NA) {
      
      # Get the rownames for each group
      idx_group_1 = rownames(meta_table)[meta_table[, col_group] == group_1]
      idx_group_2 = rownames(meta_table)[meta_table[, col_group] == group_2]
      
      # Get all row names from both groups
      idx_all = c(idx_group_1, idx_group_2)
      idx_all = unique(idx_all)
      
      # Filter data to keep only the two groups
      data_table = data_table[idx_all,]
      
      # Remove empty columns
      dead_features = colnames(data_table)
      data_table = remove_empty_cols(table = data_table)
      dead_features = setdiff(dead_features, colnames(data_table))
      
      if (length(dead_features) > 0) {
        dead_features = which(rownames(data_table) %in% dead_features)
        data_table = data_table[,-dead_features]
      }
      
      # Prepare the protein list with log2 fold changes
      stat_vals = get_fc_and_pval(data_table = data_table,
                                  idx_group_1 = idx_group_1,
                                  idx_group_2 = idx_group_2,
                                  used_function = used_function,
                                  test = test)

      stat_vals = as.data.frame(stat_vals, row.names = colnames(data_table))
      
      if (!is.na(p_value_cutoff)) {
        stat_vals = stat_vals[stat_vals$p_value <= p_value_cutoff,]
      }
      
      prot_list = log2(stat_vals$fold_change)
      names(prot_list) = rownames(stat_vals)
      
      # NA omit and sort
      prot_list = na.omit(prot_list)
      prot_list = sort(prot_list, decreasing = TRUE)
      self$tables$prot_list = prot_list
    },
    
    # GSEA object 
    get_gsea_object = function(prot_list = self$tables$prot_list,
                               ont = "ALL",
                               nPerm = 10000,
                               minGSSize = 3,
                               maxGSSize = 800, 
                               pvalueCutoff = 0.05, 
                               verbose = TRUE, 
                               OrgDb = "org.Hs.eg.db", 
                               pAdjustMethod = "none") {
      self$tables$gsea_object = clusterProfiler::gseGO(geneList=prot_list, 
                                                       ont = ont, 
                                                       keyType = "UNIPROT", 
                                                       nPerm = nPerm, 
                                                       minGSSize = minGSSize, 
                                                       maxGSSize = maxGSSize, 
                                                       pvalueCutoff = pvalueCutoff, 
                                                       verbose = TRUE, 
                                                       OrgDb = "org.Hs.eg.db", 
                                                       pAdjustMethod = pAdjustMethod)
      
    },
    
    #------------------------------------------------ Normalisation methods ----
    
    # Z-score normalisation
    normalise_z_score = function() {
      self$tables$data_z_scored = z_score_normalisation(data_table = self$tables$data_filtered,
                                                        impute = NA)
    },
    
    # Total or Row normalisation
    normalise_total = function(){
      self$tables$data_total_norm = self$tables$data_filtered/rowSums(self$tables$data_filtered, na.rm = T)
    },
    
    # Total and z-score normalisation
    normalise_total_z_score = function() {
      self$tables$data_total_norm_z_scored = z_score_normalisation(data_table = self$tables$data_total_norm,
                                                                   impute = 0)
    },
    
    
    
    
    #----------------------------------------------------- Plotting methods ----
    
    ## Volcano plot
    plot_volcano = function(data_table = self$tables$volcano_table,
                            adjustment = "minus_log10_p_value_bh_adj",
                            colour_list,
                            group_1,
                            group_2,
                            width,
                            height){
      

      max_fc = ceiling(max(abs(data_table[, "log2_fold_change"])))
      
      fig = plotly::plot_ly(x = data_table[, "log2_fold_change"],
                            y = data_table[, adjustment],
                            text = rownames(data_table),
                            hoverinfo = "text",
                            colors = colour_list,
                            type  = "scatter",
                            mode  = "markers",
                            width = width,
                            height = height)
      
      fig = fig %>% layout(shapes = list(vline(x = -1, dash = "dot"), vline(x = 1, dash = "dot"), hline(-log10(0.05), dash = "dot")),
                           title = paste0(group_1, " (left), ", group_2, " (right)"),
                           xaxis = list(title = "Log2(fold change)",
                                        range = c(-max_fc,max_fc)
                           ),
                           yaxis = list(title = adjustment_title_switch(adjustment)))
      self$plots$volcano_plot = fig
    },
    
    
    ## Heatmap plot
    plot_heatmap = function(data_table = self$tables$data_total_norm_z_scored,
                            meta_table = self$tables$meta_filtered,
                            percentile,
                            cluster_rows = TRUE,
                            cluster_cols = TRUE,
                            row_annotations = c("Genotype", "GroupName"),
                            width,
                            height
    ) {
      
      
      # Save table as heatmap table
      self$tables$heatmap_table = data_table
      meta_table = meta_table[rownames(data_table),]
      
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
      
      # Set percentiles
      percentile = percentile/100
      alpha = (1 - percentile)
      
      val_list = c()
      for (col in colnames(data_table)) {
        val_list = c(val_list, data_table[,col])
      }
      
      val_list = sort(val_list)
      zmin = quantile(val_list, alpha/2)
      zmax = quantile(val_list, 1 - alpha/2)
      col_lim = round(max(abs(c(zmin, zmax))), 2)
      
      # Filter out the data using the percentiles
      data_table[data_table > zmax] = zmax
      data_table[data_table < zmin] = zmin
      
      # Annotations
      if (!is.null(row_annotations)) {
        row_annotations = meta_table[, row_annotations]
      } else {
        row_annotations = NULL
      }
      
      # Plot the data
      self$plots$heatmap = heatmaply::heatmaply(x = t(data_table),
                                                scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                                                  low = "blue",
                                                  high = "red",
                                                  midpoint = 0,
                                                  limits = c(-col_lim, col_lim)
                                                ),
                                                width = width,
                                                height = height,
                                                limits = c(zmin, zmax),
                                                col_side_colors = row_annotations,
                                                dendrogram = dendrogram_list)
      
    },
    
    ## PCA scores and loading plots
    plot_pca = function(data_table, col_group, width, height, colour_list) {
      
      pca_data = get_pca_data(data_table = data_table)
      
      fig = c()
      
      # Store tables
      self$tables$pca_scores_table = pca_data@scores
      self$tables$pca_loadings_table = pca_data@loadings
      
      fig[[1]] = pca_plot_scores(x = pca_data@scores[, "PC1"],
                                 y = pca_data@scores[, "PC2"],
                                 meta_table = self$tables$meta_filtered,
                                 group_col = col_group,
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
    
    plot_dot_plot = function(gsea_object = self$tables$gsea_object, showCategory=10, split=".sign") {
      self$plots$dotplot = enrichplot::dotplot(gsea_object,
                                               showCategory = showCategory,
                                               split = split) + facet_grid(.~.sign)
      # print(self$plots$dotplot)
      
    },
    
    plot_cnet_plot = function(gsea_object = self$tables$gsea_object, categorySize="pvalue", foldChange=self$tables$prot_list, showCategory = 3) {
      self$plots$cnetplot = enrichplot::cnetplot(gsea_object,
                                                 categorySize=categorySize,
                                                 foldChange=foldChange,
                                                 showCategory = showCategory)
      # print(self$plots$cnetplot)
    },
    
    plot_ridge_plot = function(gsea_object = self$tables$gsea_object) {
      self$plots$ridgeplot = enrichplot::ridgeplot(gsea_object) + labs(x = "enrichment distribution")
      
      # print(self$plots$ridgeplot)
    }
    
    



    
    
    #------------------------------------------------------------------ END ----
  )
)
