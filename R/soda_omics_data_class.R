#---------------------------- Class Omics_data ---------------------------------
Omics_data = R6::R6Class(
  "Omics_data",
  public = list(
    ### Global 
    name = NULL,
    type = NULL,
    non_unique_ids_meta = FALSE,
    non_unique_ids_data = FALSE,
    
    ### Tables
    # Raw
    meta_raw = NULL,
    data_raw = NULL,
    
    # Filtered
    data_filtered = NULL,
    meta_filtered = NULL,
    meta_features = NULL,
    
    # Normalised
    data_z_scored = NULL,
    data_class_norm = NULL,
    data_total_norm = NULL,
    data_class_norm_z_scored = NULL,
    data_total_norm_z_scored = NULL,
    
    # class tables
    data_class_table = NULL,
    data_class_table_z_scored = NULL,
    
    # Plot table
    volcano_table = NULL,
    dbplot_table = NULL,
    
    ### Columns
    col_id_meta = NULL,
    col_id_data = NULL,
    col_type = NULL,
    col_group = NULL,
    
    ### Text patterns
    pattern_qc = NULL,
    pattern_blank = NULL,
    pattern_pool = NULL,
    
    ### Plots
    class_distribution = NULL,
    class_comparison = NULL,
    volcano_plot = NULL,
    heatmap = NULL,
    pca_plot = NULL,
    double_bond_plot = NULL,
    
    ### Functions
    initialize = function(name = NA, type = NA){
      self$name = name
      self$type = type
    },
    set_name = function(val) {
      self$name = val
    },
    set_type = function(val) {
      self$type = val
    },
    ## Set raw data and metadata
    set_raw_meta = function(val) {
      self$meta_raw = val
    },
    set_raw_data = function(val) {
      self$data_raw = val
    },
    ## Initialise or reset the filtered data
    # Data
    set_filtered_data = function(id_col = self$col_id_data) {
      if (length(self$data_raw[, id_col]) != length(unique(self$data_raw[, id_col]))){
        self$data_filtered = NULL
        self$non_unique_ids_data = TRUE
        return()
      }
      self$non_unique_ids_data = FALSE
      table = self$data_raw
      rownames(table) = table[,id_col]
      table = table[,-which(colnames(table) == id_col)]
      self$data_filtered = table
    },
    # Metadata
    set_filtered_meta = function(id_col = self$col_id_meta) {
      if (length(self$meta_raw[, id_col]) != length(unique(self$meta_raw[, id_col]))){
        self$meta_filtered = NULL
        self$non_unique_ids_meta = TRUE
        return()
      }
      self$non_unique_ids_meta = FALSE
      table = self$meta_raw
      rownames(table) = table[,id_col]
      table = table[,-which(colnames(table) == id_col)]
      self$meta_filtered = table
    },
    
    get_feature_metadata = function(data_table = self$data_filtered) {
      # Initialise table
      feature_metadata = data.frame(row.names = sort(colnames(data_table)))
      
      # Add lipid classes
      feature_metadata$lipid_class = get_lipid_classes(feature_list = rownames(feature_metadata),
                                                         uniques = FALSE)
      
      # Collect carbon and unsaturation counts
      c_count_1 = c() # Main carbon count / total carbon count (TGs)
      s_count_1 = c() # Main saturation count
      c_count_2 = c() # Secondary carbon count (asyl groups or TGs)
      s_count_2 = c() # Secondary saturation (asyl groups or TGs)
      for (c in unique(feature_metadata$lipid_class)) {
        idx = rownames(feature_metadata)[feature_metadata$lipid_class == c]
        
        if (c == "TG") {
          # For triglycerides
          for (i in stringr::str_split(string = idx, pattern = " |:|-FA")) {
            c_count_1 = c(c_count_1, i[2])
            c_count_2 = c(c_count_2, i[4]) 
            s_count_1 = c(s_count_1, i[3])
            s_count_2 = c(s_count_2, i[5])
          }
        } else if (sum(stringr::str_detect(string = idx, pattern = "/|_")) >0) {
          # For species with asyl groups ("/" or "_")
          for (i in stringr::str_split(string = idx, pattern = " |:|_|/")) {
            c_count_1 = c(c_count_1, gsub("[^0-9]", "", i[2]))
            c_count_2 = c(c_count_2, i[4])
            s_count_1 = c(s_count_1, i[3])
            s_count_2 = c(s_count_2, i[5])
          }
        } else {
          # For the rest
          for (i in stringr::str_split(string = idx, pattern = " |:")) {
            c_count_1 = c(c_count_1, i[2])
            c_count_2 = c(c_count_2, i[2])
            s_count_1 = c(s_count_1, i[3])
            s_count_2 = c(s_count_2, i[3])
          }
        }
      }
      
      feature_metadata$carbons_1 = c_count_1
      feature_metadata$carbons_2 = c_count_2
      feature_metadata$unsat_1 = s_count_1
      feature_metadata$unsat_2 = s_count_2
      self$meta_features = feature_metadata
    },
    
    ## Columns
    set_col = function(col, type) {
      if (type == "group"){
        self$col_group = col
      } else if (type == "type") {
        self$col_type = col
      } else if (type == "id_meta") {
        self$col_id_meta = col
      } else if (type == "id_data") {
        self$col_id_data = col
      }
    },
    
    ## Text patterns
    set_text_pattern = function(pattern, type) {
      if (type == "qc"){
        self$pattern_qc = pattern
      } else if (type == "blank"){
        self$pattern_blank = pattern
      } else if (type == "pool"){
        self$pattern_pool = pattern
      } else {
        stop("Text pattern selection : choose either qc, blank or pool.")
      }
    },
    
    ## Filtering functions
    feature_filter = function(blank_multiplier, sample_threshold, group_threshold) {
      
      # Find features / columns below threshold
      del_cols = blank_filter(data_table = self$data_filtered,
                              blank_table = self$data_raw[self$get_idx_blanks(table = self$meta_raw),-which(colnames(self$data_raw) == self$col_id_data)],
                              blank_multiplier = blank_multiplier,
                              sample_threshold = sample_threshold)
      
      # Salvage some of the features with a group filtering (same as above but applied to groups)
      if (!is.null(del_cols)) {
        saved_cols = group_filter(data_table = self$data_filtered,
                                  blank_table = self$data_raw[self$get_idx_blanks(table = self$meta_raw),-which(colnames(self$data_raw) == self$col_id_data)],
                                  meta_table= self$meta_filtered,
                                  del_cols = del_cols,
                                  col_group = self$col_group,
                                  blank_multiplier = blank_multiplier,
                                  group_threshold = group_threshold)
        del_cols = setdiff(del_cols,saved_cols)
        if (length(del_cols) == 0) {del_cols = NULL}
      }
      
      if (!is.null(del_cols)) {
        self$data_filtered = self$data_filtered[,!(colnames(self$data_filtered) %in% del_cols)]
      }
    },
    
    ## Index functions
    get_idx_blanks = function(table = self$meta_filtered, row_names = T) {
      idx_blanks = get_idx_by_pattern(table = table,
                                      col = self$col_type,
                                      pattern = self$pattern_blank,
                                      row_names = row_names)
      if (length(idx_blanks) == 0) {idx_blanks = NULL}
      return(idx_blanks)
    },
    get_idx_qcs = function(table = self$meta_filtered, row_names = T) {
      idx_qcs = get_idx_by_pattern(table = table,
                                   col = self$col_type,
                                   pattern = self$pattern_qc,
                                   row_names = row_names)
      if (length(idx_qcs) == 0) {idx_qcs = NULL}
      return(idx_qcs)
    },
    get_idx_pools = function(table = self$meta_filtered, row_names = T) {
      idx_pools = get_idx_by_pattern(table = table,
                                     col = self$col_type,
                                     pattern = self$pattern_pool,
                                     row_names = row_names)
      if (length(idx_pools) == 0) {idx_pools = NULL}
      return(idx_pools)
    },
    get_idx_samples = function(table = self$meta_filtered){
      idx_samples = rownames(self$meta_filtered)
      idx_non_samples = c(self$get_idx_blanks(table),
                          self$get_idx_qcs(table),
                          self$get_idx_pools(table))
      
      

      if (length(idx_non_samples) > 0) {
        idx_non_samples = sort(unique(idx_non_samples))
        idx_samples = idx_samples[!(idx_samples %in% idx_non_samples)]
      }
      return(idx_samples)
    },
    
    ## Normalisation functions
    normalise_z_score = function() {
      self$data_z_scored = z_score_normalisation(data_table = self$data_filtered,
                                                 impute = NA)
    },

    normalise_class = function(){
      self$data_class_norm = normalise_lipid_class(self$data_filtered)
    },
    normalise_total = function(){
      self$data_total_norm = self$data_filtered/rowSums(self$data_filtered, na.rm = T)
    },
    
    normalise_class_z_score = function() {
      self$data_class_norm_z_scored = z_score_normalisation(data_table = self$data_class_norm,
                                                            impute = 0)
    },
    
    normalise_total_z_score = function() {
      self$data_total_norm_z_scored = z_score_normalisation(data_table = self$data_total_norm,
                                                            impute = 0)
    },
    
    normalise_class_table_z_score = function() {
      self$data_class_table_z_scored = z_score_normalisation(data_table = self$data_class_table,
                                                            impute = 0)
    },
    
    ## Class tables
    class_grouping = function(table = self$data_total_norm){
      self$data_class_table = get_lipid_class_table(table)
    },
    
    ## Volcano table
    get_volcano_table = function(data_table = self$data_filtered, data_table_normalised = self$data_z_scored, col_group = self$col_group, group_1, group_2) {
      
      # Get the rownames for each group
      idx_group_1 = get_idx_by_pattern(table = self$meta_filtered,
                                       col = col_group,
                                       pattern = group_1,
                                       row_names = T)
      
      idx_group_2 = get_idx_by_pattern(table = self$meta_filtered,
                                       col = col_group,
                                       pattern = group_2,
                                       row_names = T)
      
      # Get all row names from both groups
      idx_all = c(idx_group_1, idx_group_2)
      idx_all = sort(unique(idx_all))
      
      # Filter data to keep only the two groups
      data_table = data_table[idx_all,]
      data_table_normalised = data_table_normalised[idx_all,]
      
      # Collect fold change and p-values
      fold_change = c()
      p_value = c()
      for (col in colnames(data_table)) {
        fold_change = c(fold_change, median(data_table[idx_group_1, col], na.rm = T) / median(data_table[idx_group_2, col], na.rm = T))
        p_value = c(p_value, wilcox.test(data_table_normalised[idx_group_1, col], data_table_normalised[idx_group_2, col])$p.value)
      }
      p_value_bh_adj = p.adjust(p_value, method = "BH")
      
      volcano_table = data.frame(log2_fold_change = log2(fold_change),
                                 minus_log10_p_value_bh_adj = -log10(p_value_bh_adj),
                                 lipid_class = get_lipid_classes(feature_list = colnames(data_table),
                                                                           uniques = FALSE),
                                 row.names = colnames(data_table))
      self$volcano_table = volcano_table
    },
    
    ## Double bond plot table
    get_dbplot_table = function(data_table = self$data_filtered, data_table_normalised = self$data_z_scored, dbplot_table = self$meta_features, col_group = self$col_group, group_1, group_2) {
      
      # Get the rownames for each group
      idx_group_1 = get_idx_by_pattern(table = self$meta_filtered,
                                       col = col_group,
                                       pattern = group_1,
                                       row_names = T)
      
      idx_group_2 = get_idx_by_pattern(table = self$meta_filtered,
                                       col = col_group,
                                       pattern = group_2,
                                       row_names = T)
      
      # Get all row names from both groups
      idx_all = c(idx_group_1, idx_group_2)
      idx_all = sort(unique(idx_all))
      
      # Filter data to keep only the two groups
      data_table = data_table[idx_all,]
      data_table_normalised = data_table_normalised[idx_all,]
      
      # Collect fold change and p-values
      fold_change = c()
      p_value = c()
      for (col in rownames(dbplot_table)) {
        fold_change = c(fold_change, median(data_table[idx_group_1, col], na.rm = T) / median(data_table[idx_group_2, col], na.rm = T))
        p_value = c(p_value, wilcox.test(data_table_normalised[idx_group_1, col], data_table_normalised[idx_group_2, col])$p.value)
      }
      p_value_bh_adj = p.adjust(p_value, method = "BH")
      
      dbplot_table$log2_fold_change = log2(fold_change)
      dbplot_table$log10_p_value_bh_adj = log10(p_value_bh_adj)
      
      self$dbplot_table = dbplot_table
    },
    
    
    ### Plotting
    ## Class distribution
    plot_class_distribution = function(table = self$data_class_table[self$get_idx_samples(), ],
                                       meta_table = self$meta_filtered[self$get_idx_samples(), ],
                                       col_group = self$col_group,
                                       colour_list,
                                       width,
                                       height){
      
      # Produce the class x group table
      samp_list = rownames(table)
      class_list = colnames(table)
      group_list = unique(meta_table[,col_group])
      
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
      
      # Produce the plot
      i = 1
      fig = plotly::plot_ly(colors = colour_list, width = width, height = height)
      for (col in colnames(plot_table)) {
        fig = fig %>% add_trace(x = rownames(plot_table), y = plot_table[,col],
                                name = col, color = colour_list[i], type  = "bar")
        fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5))
        i = i + 1
      }
      self$class_distribution = fig
    },
    
    ## Class comparison
    plot_class_comparison = function(data_table = self$data_class_table[self$get_idx_samples(), ],
                                     meta_table = self$meta_filtered[self$get_idx_samples(), ],
                                     col_group = self$col_group,
                                     colour_list,
                                     width,
                                     height){
      groups = unique(meta_table[,col_group])
      class_list = colnames(data_table)
      annotations = get_subplot_titles(class_list)
      dims = get_subplot_dim(class_list)
      plot_list = c()
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
          s = rownames(meta_table)[meta_table[, col_group] == g]
          d = data_table[s, c]
          m = mean(d)
          subplot = subplot %>% add_trace(x = g, y = m, type  = "bar", name = g,
                                          color = colour_list[i], alpha = 1,
                                          legendgroup=i, showlegend = first_bool)
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
      self$class_comparison = fig
    },
    
    ## Volcano plot
    plot_volcano = function(data_table = self$volcano_table,
                            colour_list,
                            width,
                            height){
      i = 1
      fig = plotly::plot_ly(colors = colour_list, type  = "scatter", mode  = "markers", width = width, height = height)
      for (lip_class in unique(self$volcano_table$lipid_class)) {
        tmp_idx = rownames(self$volcano_table)[self$volcano_table$lipid_class == lip_class]
        fig = fig %>% add_trace(x = self$volcano_table[tmp_idx, "log2_fold_change"],
                                y = self$volcano_table[tmp_idx, "minus_log10_p_value_bh_adj"],
                                name = lip_class,
                                color = colour_list[i],
                                text = tmp_idx,
                                hoverinfo = "text"
        )
        i = i + 1
      }
      fig = fig %>% layout(shapes = list(vline(x = -1, dash = "dot"), vline(x = 1, dash = "dot")),
                           xaxis = list(title = "log2(fold change)"),
                           yaxis = list(title = "-log10(BH adjusted p value)"))
      self$volcano_plot = fig
    },
    
    ## Heatmap plot
    plot_heatmap = function(data_table,
                            width,
                            height) {
      fig = plotly::plot_ly(
        x = rownames(data_table),
        y = colnames(data_table),
        z = t(data_table),
        colors = colorRamp(c("blue","white", "red")),
        type = "heatmap",
        width = width,
        height = height
      )
      fig = fig %>% layout(
        list(
          coloraxis=list(
            cauto = FALSE,
            cmin = min(data_table),
            cmid = 0,
            cmax = -min(data_table)
          )
        )
      )
      self$heatmap = fig
    },
    
    ## PCA scores and loading plots
    plot_pca = function(data_table, col_group, width, height, colour_list) {
      
      pca_data = get_pca_data(data_table = data_table)
      
      fig = c()
      
      fig[[1]] = pca_plot_scores(x = pca_data@scores[, "PC1"],
                                 y = pca_data@scores[, "PC2"],
                                 meta_table = self$meta_filtered,
                                 group_col = col_group,
                                 width = width,
                                 height = height,
                                 colour_list = colour_list)
      
      fig[[2]] = pca_plot_loadings(x = pca_data@loadings[, "PC1"],
                                   y =  pca_data@loadings[, "PC2"],
                                   feature_list = colnames(data_table),
                                   width = width,
                                   height = height,
                                   colour_list = colour_list)
      
      fig = plotly::subplot(fig, nrows = 1, margin = 0.035, titleX = TRUE)
      fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5))
      self$pca_plot = fig
    },
    
    ## Double bond plot
    
    plot_doublebonds = function(data_table = self$dbplot_table, lipid_class, width, height){
      
      selected_rows = rownames(data_table)[data_table["lipid_class"] == lipid_class]
      
      fig = plotly::plot_ly(data_table[selected_rows,],
                            x = ~carbons_1,
                            y = ~unsat_1,
                            color = ~log10_p_value_bh_adj,
                            size = ~log2_fold_change,
                            mode = "markers",
                            sizes = ~c(5,40),
                            marker = list(sizemode ='diameter' , opacity = 0.5,sizeref=1),
                            showlegend=T,
                            type = "scatter",
                            text = selected_rows,
                            hoverinfo = "text",
                            width = width,
                            height = height)
      fig = fig %>% layout(
        legend= list(itemsizing='constant'),
        title = paste0("Double bonds in ", lipid_class),
        xaxis = list(title = 'Total carbons'),
        yaxis = list(title = 'Total double bonds')
      )
      fig = fig %>% config(modeBarButtonsToAdd = c('drawline',
                                                   'drawopenpath',
                                                   'drawclosedpath',
                                                   'drawcircle',
                                                   'drawrect',
                                                   'eraseshape'))
      self$double_bond_plot = fig
    }
  )
)