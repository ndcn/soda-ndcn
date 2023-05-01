#---------------------------- Class Lips_data ---------------------------------
Lips_data = R6::R6Class(
  "Lips_data",
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
      col_type = NULL,
      col_group = NULL,
      col_batch = NULL,

      ### Text patterns
      pattern_qc = NULL,
      pattern_blank = NULL,
      pattern_pool = NULL
    ),

    #-------------------------------------------------------------- Indices ----
    indices = list(
      idx_blanks = NULL,
      idx_qcs = NULL,
      idx_pools = NULL,
      idx_samples = NULL,
      rownames_blanks = NULL,
      rownames_qcs = NULL,
      rownames_pools = NULL,
      rownames_samples = NULL,
      missing_vals = NULL,
      batch_list = NULL
    ),
    
    #--------------------------------------------------------------- Tables ----
    tables = list(

      # Raw
      meta_raw = NULL,
      data_raw = NULL,
      
      # Blank table
      blank_table = NULL,

      # Filtered
      meta_filtered = NULL,
      data_filtered = NULL,
      feat_raw = NULL,
      feat_filtered = NULL,
      
      # Summary
      group_species = NULL,
      group_classes = NULL,

      # Normalised
      data_class_norm = NULL,
      data_total_norm = NULL,
      
      # Z-scored
      data_z_scored = NULL,
      data_class_norm_z_scored = NULL,
      data_total_norm_z_scored = NULL,

      # class tables
      data_class_table_raw = NULL,
      data_class_table_total_norm = NULL,
      data_class_table_z_scored = NULL,

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

    ### Methods
    initialize = function(name = NA, type = NA){
      self$name = name
      self$type = type
    },

    #----------------------------------------------------------- Parameters ----
    params = list(
      # Class distribution parameters
      class_distribution = shiny::reactiveValues(
        dataset = "Total normalised class table",
        group_col = NULL
      ),
      
      # Class comparison parameters
      class_comparison = shiny::reactiveValues(
        dataset = "Total normalised class table",
        group_col = NULL
      ),
      
      # Volcano plot parameters
      volcano_plot = shiny::reactiveValues(
        data_table = "Filtered data table",
        adjustment = "Benjamini-Hochberg",
        group_column = NULL,
        groups = NULL,
        classes = NULL,
        selected_function = "median",
        selected_test = "Wilcoxon",
        colouring = "Lipid class"
      ),
      
      # Heatmap parameters
      heatmap = shiny::reactiveValues(
        dataset = "Z-scored total normalised data table",
        clustering = NULL,
        map_sample_data = character(0),
        map_feature_data = character(0),
        percentile = 95,
        group_column_da = NULL,
        apply_da = FALSE,
        alpha_da = 0.8
      ),
      
      # PCA parameters
      pca = shiny::reactiveValues(
        dataset = "Z-scored total normalised data table",
        group_column = NULL,
        apply_da = FALSE,
        alpha_da = 0.8
      ),
      
      # Double bonds parameters
      db_plot = shiny::reactiveValues(
        dataset = "Filtered data table",
        adjustment = "Benjamini-Hochberg",
        group_column = NULL,
        selected_groups = NULL,
        selected_lipid_class = NULL,
        selected_function = "median",
        selected_test = "Wilcoxon",
        fc_range = c(-5, 5),
        fc_values = c(-1, 1),
        pval_range = c(0, 5),
        pval_values = c(1, 5)
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
      } else if (type == "type") {
        self$texts$col_type = col
      } else if (type == "id_meta") {
        self$texts$col_id_meta = col
      } else if (type == "id_data") {
        self$texts$col_id_data = col
      } else if (type == "batch") {
        self$texts$col_batch = col
      }
    },

    # Text patterns
    set_text_pattern = function(pattern, type) {
      if (type == "qc"){
        self$texts$pattern_qc = pattern
      } else if (type == "blank"){
        self$texts$pattern_blank = pattern
      } else if (type == "pool"){
        self$texts$pattern_pool = pattern
      } else {
        stop("Text pattern selection : choose either qc, blank or pool.")
      }
    },


    #-------------------------------------------------------- Index methods ----
    # Index functions - Blanks
    get_idx_blanks = function() {
      idx_blanks = get_idx_by_pattern(table = self$tables$meta_raw,
                                      col = self$texts$col_type,
                                      pattern = self$texts$pattern_blank,
                                      row_names = F)
      if (length(idx_blanks) == 0) {idx_blanks = NULL}
      self$indices$idx_blanks = idx_blanks
    },
    get_rownames_blanks = function() {
      row_names = get_rownames_from_idx(idx = self$indices$idx_blanks,
                                        id_col = self$texts$col_id_meta,
                                        data_table = self$tables$meta_raw)
      if (length(row_names) == 0) {row_names = NULL}
      self$indices$rownames_blanks = row_names
    },
    
    # Index functions - QCs
    get_idx_qcs = function() {
      idx_qcs = get_idx_by_pattern(table = self$tables$meta_raw,
                                   col = self$texts$col_type,
                                   pattern = self$texts$pattern_qc,
                                   row_names = F)
      if (length(idx_qcs) == 0) {idx_qcs = NULL}
      self$indices$idx_qcs = idx_qcs
    },
    get_rownames_qcs = function() {
      row_names = get_rownames_from_idx(idx = self$indices$idx_qcs,
                                        id_col = self$texts$col_id_meta,
                                        data_table = self$tables$meta_raw)
      if (length(row_names) == 0) {row_names = NULL}
      self$indices$rownames_qcs = row_names
    },
    
    # Index functions - Pools
    get_idx_pools = function() {
      idx_pools = get_idx_by_pattern(table = self$tables$meta_raw,
                                     col = self$texts$col_type,
                                     pattern = self$texts$pattern_pool,
                                     row_names = F)
      if (length(idx_pools) == 0) {idx_pools = NULL}
      self$indices$idx_pools = idx_pools
    },
    get_rownames_pools = function() {
      row_names = get_rownames_from_idx(idx = self$indices$idx_pools,
                                        id_col = self$texts$col_id_meta,
                                        data_table = self$tables$meta_raw)
      if (length(row_names) == 0) {row_names = NULL}
      self$indices$rownames_pools = row_names
    },
    
    # Index functions - Samples
    get_idx_samples = function(){
      idx_samples = as.numeric(rownames(self$tables$meta_raw))
      idx_non_samples = c(self$indices$idx_blanks,
                          self$indices$idx_qcs,
                          self$indices$idx_pools)
      
      if (length(idx_non_samples) > 0) {
        idx_non_samples = sort(unique(idx_non_samples))
        idx_samples = idx_samples[!(idx_samples %in% idx_non_samples)]
      }
      self$indices$idx_samples = idx_samples
    },
    
    get_rownames_samples = function() {
      row_names = get_rownames_from_idx(idx = self$indices$idx_samples,
                                        id_col = self$texts$col_id_meta,
                                        data_table = self$tables$meta_raw)
      if (length(row_names) == 0) {row_names = NULL}
      self$indices$rownames_samples = row_names
    },
    
    get_missing_vals = function() {
      self$indices$missing_vals = is.na(self$tables$data_raw)
    },
    
    get_batch_list = function() {
      self$indices$batch_list = self$tables$meta_raw[,self$texts$col_batch]
    },
    
    set_all_indices = function() {
      # Get rows by default index
      self$get_idx_blanks()
      self$get_idx_qcs()
      self$get_idx_pools()
      self$get_idx_samples()
      self$get_batch_list()
      
      # Get rows by rowname
      self$get_rownames_blanks()
      self$get_rownames_qcs()
      self$get_rownames_pools()
      self$get_rownames_samples()

    },
    
    
    #---------------------------------------------------- Parameter methods ----
    set_params_class_distribution = function(val){
      self$params$class_distribution$group_col = val
    },
    
    set_params_class_comparison = function(val){
      self$params$class_comparison$group_col = val
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
    
    # Impute NAs
    na_imputation_raw = function(imputation_factor = NA) {
      self$tables$data_raw = batch_na_imputation(data_table = self$tables$data_raw,
                                                 mask = self$indices$missing_vals,
                                                 batch_list = self$indices$batch_list,
                                                 imputation_factor = as.numeric(imputation_factor))
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

    # Set feature tables
    set_feat_raw = function() {
      id_col = self$texts$col_id_data
      data_table = self$tables$data_raw
      rownames(data_table) = data_table[,id_col]
      data_table = data_table[,-which(colnames(data_table) == id_col)]
      self$tables$feat_raw = get_feature_metadata(data_table = data_table)
    },
    set_feat_filtered = function() {
      self$tables$feat_filtered = get_feature_metadata(data_table = self$tables$data_filtered)
    },
    
    # Set blank table
    set_blank_table = function() {
      
      # Get table with definitive index
      blank_table = set_index_col(data_table = self$tables$data_raw,
                                  idx = self$texts$col_id_data)
      
      # Get blank rownames
      rownames_blanks = self$indices$rownames_blanks
      
      # Get blank table
      blank_table = blank_table[rownames_blanks, ]
      self$tables$blank_table = blank_table
    },
    
    set_group_summary_species = function() {
      
      self$tables$group_species = get_group_median_table(data_table = self$tables$data_filtered,
                                                         meta_table = self$tables$meta_filtered,
                                                         col_group = self$texts$col_group) 
    },
    
    set_group_summary_classes = function() {
      
      self$tables$group_classes = get_group_median_table(data_table = self$tables$data_class_table_raw,
                                                         meta_table = self$tables$meta_filtered,
                                                         col_group = self$texts$col_group) 
    },

    # Filter filtered table
    feature_filter = function(blank_multiplier, sample_threshold, group_threshold) {
      
      del_cols = lips_get_del_cols(data_table = self$tables$data_filtered,
                                   blank_table = self$tables$blank_table,
                                   meta_table_raw = self$tables$meta_raw,
                                   meta_table_filtered = self$tables$meta_filtered,
                                   idx_blanks = self$indices$idx_blanks,
                                   idx_samples = self$indices$idx_samples,
                                   col_id_meta = self$texts$col_id_meta,
                                   col_group = self$texts$col_group,
                                   col_batch = self$texts$col_batch,
                                   blank_multiplier = blank_multiplier,
                                   sample_threshold = sample_threshold,
                                   group_threshold = group_threshold
                                   )
      if (length(del_cols) > 0) {
        self$tables$data_filtered = self$tables$data_filtered[,!(colnames(self$tables$data_filtered) %in% del_cols)]
      }
    },
    
    # Normalise to a metadata column
    normalise_to_metadata = function(meta_col) {
      if (meta_col == "None") {
        return()
      }
      
      data_table = self$tables$data_filtered
      norm_list = as.numeric(self$tables$meta_filtered[, meta_col])
      if (any(is.na(norm_list))) {
        print_time("Normalisation: selected column contains non-numeric values, skipping.")
        return()
      }
      self$tables$data_filtered = meta_normalise(data_table = data_table,
                                                 norm_list = norm_list)
    },

    # Class tables
    class_grouping_raw = function(table = self$tables$data_filtered){
      self$tables$data_class_table_raw = get_lipid_class_table(table)
    },
    
    
    class_grouping_total_norm = function(table = self$tables$data_total_norm){
      self$tables$data_class_table_total_norm = get_lipid_class_table(table)
    },

    # Volcano table
    get_volcano_table = function(data_table = self$tables$data_filtered,
                                 volcano_table = self$tables$feat_filtered,
                                 col_group = self$texts$col_group,
                                 used_function = "median",
                                 test = "Wilcoxon",
                                 group_1,
                                 group_2) {

      idx_group_1 = get_idx_by_pattern(table = self$tables$meta_filtered,
                                       col = col_group,
                                       pattern = group_1,
                                       row_names = T)

      idx_group_2 = get_idx_by_pattern(table = self$tables$meta_filtered,
                                       col = col_group,
                                       pattern = group_2,
                                       row_names = T)

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

    # Double bond plot table
    
    get_dbplot_table_single = function(data_table = self$tables$data_filtered,
                                       dbplot_table = self$tables$feat_filtered,
                                       col_group = self$texts$col_group,
                                       used_function = "median",
                                       group_1){
      
      # Set the averaging function
      if (used_function == "median") {
        av_function = function(x) {return(median(x, na.rm = T))}
      } else {
        av_function = function(x) {return(mean(x, na.rm = T))}
      }
      
      # Get the rownames for each group
      idx_group_1 = get_idx_by_pattern(table = self$tables$meta_filtered,
                                       col = col_group,
                                       pattern = group_1,
                                       row_names = T)
      
      
      
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
    
    get_dbplot_table_double = function(data_table = self$tables$data_filtered,
                                       dbplot_table = self$tables$feat_filtered,
                                       col_group = self$texts$col_group,
                                       used_function = "median",
                                       test = "Wilcoxon",
                                       group_1,
                                       group_2) {

      # Get the rownames for each group
      idx_group_1 = get_idx_by_pattern(table = self$tables$meta_filtered,
                                       col = col_group,
                                       pattern = group_1,
                                       row_names = T)

      idx_group_2 = get_idx_by_pattern(table = self$tables$meta_filtered,
                                       col = col_group,
                                       pattern = group_2,
                                       row_names = T)

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

      stat_vals = get_fc_and_pval(data_table, idx_group_1, idx_group_2, used_function, test)
      fold_change = stat_vals$fold_change
      p_value = stat_vals$p_value
      p_value_bh_adj = stat_vals$p_value_bh_adj
      


      dbplot_table$fold_change = fold_change
      dbplot_table$p_value = p_value
      dbplot_table$minus_log10_p_value = -log10(p_value)
      dbplot_table$log2_fold_change = log2(fold_change)
      dbplot_table$minus_log10_p_value_bh_adj = -log10(p_value_bh_adj)
      
      lips = rownames(dbplot_table)
      fc = as.character(round(dbplot_table[,"log2_fold_change"],2))
      pval = as.character(round(dbplot_table[,"minus_log10_p_value_bh_adj"],2))
      dbplot_table$text = paste0(lips, " | log2(fc): ", fc, " | -log10(bh(pval)): ", pval)
      
      # Drop NA p-values
      if (length(which(is.na(dbplot_table[,'p_value']))) > 0) {
        dbplot_table = dbplot_table[-which(is.na(dbplot_table[,'p_value'])),]
      }
      

      self$tables$dbplot_table = dbplot_table
    },

    # Set all tables
    set_all_tables = function(){
      
      # Normalisation
      self$normalise_z_score()
      self$normalise_class()
      self$normalise_total()
      self$normalise_class_z_score()
      self$normalise_total_z_score()
      
      # Create feature table
      self$set_feat_filtered()

      # Class table
      self$class_grouping_raw()
      self$class_grouping_total_norm()
      self$normalise_class_table_z_score()
      
      # Summary tables
      self$set_group_summary_species()
      self$set_group_summary_classes()
      
    },
    
    #------------------------------------------------ Normalisation methods ----

    # Z-score normalisation
    normalise_z_score = function() {
      self$tables$data_z_scored = z_score_normalisation(data_table = self$tables$data_filtered,
                                                 impute = NA)
    },

    # Class normalisation
    normalise_class = function(){
      self$tables$data_class_norm = normalise_lipid_class(self$tables$data_filtered)
    },

    # Total or Row normalisation
    normalise_total = function(){
      self$tables$data_total_norm = self$tables$data_filtered/rowSums(self$tables$data_filtered, na.rm = T)
    },

    # Class and z-score normalisation
    normalise_class_z_score = function() {
      self$tables$data_class_norm_z_scored = z_score_normalisation(data_table = self$tables$data_class_norm,
                                                            impute = 0)
    },

    # Total and z-score normalisation
    normalise_total_z_score = function() {
      self$tables$data_total_norm_z_scored = z_score_normalisation(data_table = self$tables$data_total_norm,
                                                            impute = 0)
    },

    # Z-score the class table (generated by the class_grouping method)
    normalise_class_table_z_score = function() {
      self$tables$data_class_table_z_scored = z_score_normalisation(data_table = self$tables$data_class_table_total_norm,
                                                            impute = 0)
    },




    #----------------------------------------------------- Plotting methods ----
    # Class distribution
    plot_class_distribution = function(table = self$tables$data_class_table_total_norm,
                                       meta_table = self$tables$meta_filtered,
                                       col_group = self$texts$col_group,
                                       colour_list,
                                       width,
                                       height){

      # Produce the class x group table
      samp_list = rownames(table)
      class_list = colnames(table)
      group_list = sort(unique(meta_table[,col_group]))

      plot_table = data.frame(matrix(data = 0.0,
                                     nrow = length(class_list),
                                     ncol = length(group_list)))
      rownames(plot_table) = class_list
      colnames(plot_table) = group_list

      for (c in class_list) {
        for (g in group_list){
          s = rownames(meta_table)[meta_table[,col_group] == g]
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

    ## Class comparison
    plot_class_comparison = function(data_table = self$tables$data_class_table_total_norm,
                                     meta_table = self$tables$meta_filtered,
                                     col_group = self$texts$col_group,
                                     colour_list,
                                     width,
                                     height){

      # Get sample groups and the list of classes
      groups = sort(unique(meta_table[,col_group]))
      class_list = colnames(data_table)

      # Annotations are for now a way to display each subplot title
      annotations = get_subplot_titles(class_list)

      # dims makes the grid of subplots to be generated
      dims = get_subplot_dim(class_list)

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
          s = rownames(meta_table)[meta_table[, col_group] == g] # Get the samples for the current group
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

      fig = subplot(plot_list, nrows = dims, margin = 0.035, titleX = TRUE)
      fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5),
                           annotations = annotations)
      self$plots$class_comparison = fig
    },

    ## Volcano plot
    plot_volcano = function(data_table = self$tables$volcano_table,
                            adjustment = "minus_log10_p_value_bh_adj",
                            colour_list,
                            group_1,
                            group_2,
                            displayed_classes = NULL,
                            colouring = "Lipid class",
                            width,
                            height){
      
      # Select the colouring column
      if (colouring == "Lipid class") {
        feat_col = "lipid_class"
      } else if (colouring == "Double bonds") {
        feat_col = "unsat_1"
      } else if (colouring == "Total carbons") {
        feat_col = "carbons_1"
      }
      
      # If null, display all classes
      if (is.null(displayed_classes)) {
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
    plot_heatmap = function(data_table = self$tables$data_total_norm_z_scored,
                            meta_table = self$tables$meta_filtered,
                            meta_table_features = self$tables$feat_filtered,
                            percentile,
                            cluster_rows = TRUE,
                            cluster_cols = TRUE,
                            row_annotations = c("Genotype", "GroupName"),
                            col_annotations = c("Class"),
                            width,
                            height
                            ) {


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

      # Reorder the feature metadata according to the data_table order
      meta_table_features = meta_table_features[c(colnames(data_table)),]

      if (!is.null(col_annotations)) {
        # Convert annotations to their column names in the feature metadata table
        col_annotations = feature_switch(col_annotations)
        col_annotations = meta_table_features[, col_annotations]
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
                                          row_side_colors = col_annotations,
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

    ## Double bond plot
    plot_doublebonds_single = function(data_table = self$tables$dbplot_table, lipid_class, group_1, width, height){
      
      selected_rows = rownames(data_table)[data_table["lipid_class"] == lipid_class]
      data_table = data_table[selected_rows,]
      x_lims = c(min(data_table$carbons_1) -1, max(data_table$carbons_1) +1)
      y_lims = c(min(data_table$unsat_1) -0.5, max(data_table$unsat_1) +1)
      
      fig = plotly::plot_ly(data_table,
                            x = ~carbons_1,
                            y = ~unsat_1,
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
        xaxis = list(title = 'Total carbons',
                     range = x_lims
        ),
        yaxis = list(title = 'Total double bonds',
                     range = y_lims
        )
      )
      fig = fig %>% config(modeBarButtonsToAdd = c('drawline',
                                                   'drawopenpath',
                                                   'drawclosedpath',
                                                   'drawcircle',
                                                   'drawrect',
                                                   'eraseshape'))
      self$plots$double_bond_plot = fig
    },
    
    
    plot_doublebonds_double = function(data_table = self$tables$dbplot_table,
                                       adjustment = "minus_log10_p_value_bh_adj",
                                       lipid_class,
                                       fc_limits,
                                       pval_limits,
                                       group_1,
                                       group_2,
                                       width,
                                       height){

      selected_rows = rownames(data_table)[data_table["lipid_class"] == lipid_class]
      data_table = data_table[selected_rows,]
      x_lims = c(min(data_table$carbons_1) -1, max(data_table$carbons_1) +1)
      y_lims = c(min(data_table$unsat_1) -0.5, max(data_table$unsat_1) +1)
      data_table = data_table[!dplyr::between(data_table[,"log2_fold_change"], fc_limits[1], fc_limits[2]),]
      data_table = data_table[dplyr::between(data_table[,adjustment], pval_limits[1], pval_limits[2]),]
      if (nrow(data_table) > 0) {
        fig = plotly::plot_ly(data_table,
                              x = ~carbons_1,
                              y = ~unsat_1,
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
                              x = ~carbons_1,
                              y = ~unsat_1,
                              type = "scatter",
                              mode = "markers",
                              width = width,
                              height = height)
      }

      fig = fig %>% layout(
        legend= list(itemsizing='constant'),
        title = paste0("Comparison in ", lipid_class, " - ", group_1, " (blue), ", group_2, " (red)"),
        xaxis = list(title = 'Total carbons',
                     range = x_lims
                     ),
        yaxis = list(title = 'Total double bonds',
                     range = y_lims
                     )
      )
      self$plots$double_bond_plot = fig
    }
    #------------------------------------------------------------------ END ----
    
    
  )
)
