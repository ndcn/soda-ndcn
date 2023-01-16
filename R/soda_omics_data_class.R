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
    
    # Normalised
    data_class_norm = NULL,
    data_total_norm = NULL,
    
    # class tables
    data_class_table = NULL,
    
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
      
      idx_samples = self$get_idx_samples()
      idx_blanks = self$get_idx_blanks()
      col_group = self$col_group
      
      
      blank_means = colMeans(self$data_filtered[idx_blanks,], na.rm = TRUE)
      blank_means[is.na(blank_means)] = 0
      
      # Find features / columns below threshold
      del_cols = blank_filter(data_table = self$data_filtered,
                              blank_table = self$data_raw[self$get_idx_blanks(),-which(colnames(self$data_raw) == self$col_id_data)],
                              blank_multiplier = blank_multiplier,
                              sample_threshold = sample_threshold)
      
      # Salvage some of the features with a group filtering (same as above but applied to groups)
      if (!is.null(del_cols)) {
        salvaged_cols = group_filter(data_table = self$data_filtered,
                                     blank_table = self$data_raw[self$get_idx_blanks(),-which(colnames(self$data_raw) == self$col_id_data)],
                                     meta_table= self$meta_filtered,
                                     del_cols = del_cols,
                                     col_group = col_group,
                                     blank_multiplier = blank_multiplier,
                                     group_threshold = group_threshold)
        del_cols = del_cols[-salvaged_cols]
        if (length(del_cols) == 0) {del_cols = NULL}
      }
      
      if (!is.null(del_cols)) {
        self$data_filtered = self$data_filtered[,-del_cols]
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
    normalise_class = function(){
      self$data_class_norm = normalise_lipid_class(self$data_filtered)
    },
    normalise_total = function(){
      self$data_total_norm = self$data_filtered/rowSums(self$data_filtered, na.rm = T)
    },
    
    ## Class tables
    class_grouping = function(table = self$data_total_norm){
      self$data_class_table = get_lipid_class_table(table)
    },
    
    ### Plotting
    ## Class distribution
    plot_class_distribution = function(table = self$data_class_table[self$get_idx_samples(), ],
                                       meta_table = self$meta_filtered[self$get_idx_samples(), ],
                                       col_group = self$col_group,
                                       colour_list){
      
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
      fig = plotly::plot_ly(colors = colour_list)
      for (col in colnames(plot_table)) {
        fig = fig %>% add_trace(x = rownames(plot_table), y = plot_table[,col],
                                name = col, color = colour_list[i], type  = "bar")
        fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5))
        i = i + 1
      }
      self$class_distribution = fig
    },
    
    ## Class comparison
    plot_class_comparison = function(table = self$data_class_table[self$get_idx_samples(), ],
                                     meta_table = self$meta_filtered[self$get_idx_samples(), ],
                                     col_group = self$col_group,
                                     colour_list){
      groups = unique(meta_table[,col_group])
      class_list = colnames(table)
      annotations = get_subplot_titles(class_list)
      dims = get_subplot_dim(class_list)
      plot_list = c()
      cleared_groups = c()
      j = 1
      for (c in class_list) {
        i = 1
        subplot = plot_ly(colors = colour_list)
        for (g in groups){
          if (g %in% cleared_groups) {
            first_bool = FALSE
          }else{
            first_bool = TRUE
            cleared_groups = c(cleared_groups, g)
          }
          s = rownames(meta_table)[meta_table[, col_group] == g]
          d = table[s, c]
          m = mean(d)
          subplot = subplot %>% add_trace(x = g, y = m, type  = "bar", name = g,
                                          color = colour_list[i], alpha = 1,
                                          legendgroup=i, showlegend = first_bool)
          subplot = subplot %>% add_trace(x = g, y = d, type  = "box", boxpoints = "all",
                                          pointpos = 0, name = g, color = colour_list[i],
                                          line = list(color = 'rgb(100,100,100)'),
                                          marker = list(color = 'rgb(100,100,100)'), alpha = 1,
                                          legendgroup=i, showlegend = FALSE)
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
    }
  )
)