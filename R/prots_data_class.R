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
      
      # Global
      global = shiny::reactiveValues(
        feature_id_type = NULL
      ),

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
      ),
      
      # Dot plot parameters
      dot_plot = shiny::reactiveValues(
        showCategory = 10,
        mode = "Both"
      ),
      
      # Ridge plot parameters
      ridge_plot = shiny::reactiveValues(
        showCategory = 30
      ),
      
      # CNET plot parameters
      cnet_plot = shiny::reactiveValues(
        showCategory = 3
      ),
      
      # eMap plot parameters
      emap_plot = shiny::reactiveValues(
        showCategory = 30
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
      rownames(volcano_table) = colnames(data_table)
      
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
                               keyType = self$params$global$feature_id_type,
                               ont = "ALL",
                               minGSSize = 3,
                               maxGSSize = 800, 
                               pvalueCutoff = 0.05, 
                               verbose = TRUE, 
                               OrgDb = "org.Hs.eg.db", 
                               pAdjustMethod = "none",
                               termsim_method = "JC",
                               termsim_showcat = 200) {
      
      print_time("GSEA started")
      
      gsea = clusterProfiler::gseGO(geneList=prot_list, 
                                    ont = ont, 
                                    keyType = keyType, 
                                    minGSSize = minGSSize, 
                                    maxGSSize = maxGSSize, 
                                    pvalueCutoff = pvalueCutoff, 
                                    verbose = TRUE, 
                                    OrgDb = "org.Hs.eg.db", 
                                    pAdjustMethod = pAdjustMethod)
      
      gsea = enrichplot::pairwise_termsim(gsea, method = termsim_method, semData = NULL, showCategory = termsim_showcat)
      
      
      print_time("GSEA finished")
      self$tables$gsea_object = gsea
      
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
      
      ncol_1 = ncol(data_table)
      data_table = data_table[,!is.na(colSums(data_table))]
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
    
    plot_dot_plot = function(object = self$tables$gsea_object,
                             x = "GeneRatio",
                             color = "p.adjust",
                             showCategory=10,
                             size = NULL,
                             split = ".sign",
                             orderBy="x",
                             mode = "Both",
                             width,
                             height){
      
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
      print_time("Dot plot completed")
      self$plots$dotplot = fig
    },
    
    plot_cnet_plot = function(x = self$tables$gsea_object,
                              showCategory = 3) {

      if (is.na(showCategory)) {
        base::warning("Invalid showCategory, setting to 3 by default")
        showCategory = 3
      }
      
      geneSets <- enrichplot:::extract_geneSets(x, showCategory)
      
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
      node_table$color = c(rep("#FFD800", length(main_nodes)),
                           rep("#20D9D6", length(secondary_nodes)))
      node_table$shape = rep("circle", nrow(node_table))
      
      
      
      
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
      
      self$plots$cnetplot = visNetwork::visNetwork(node_table, edge_table)
    },
    
    plot_ridge_plot = function(x = self$tables$gsea_object,
                               showCategory = 30,
                               fill="p.adjust",
                               core_enrichment = TRUE,
                               orderBy = "NES",
                               decreasing = FALSE,
                               width,
                               height) {
      
      print_time("Ridgeplot initiated")
      
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
                 ))
      
      self$plots$ridgeplot = p
    },
    
    plot_emap_plot = function(x = self$tables$gsea_object,
                              showCategory = 30) {
      
      print_time("Emapplot initiated")
      
      layout = NULL                    # removed
      coords = NULL                   # removed
      color = "p.adjust" 
      min_edge = 0.2              # removed
      cex_label_category  = 1       # removed
      cex_category = 1              # removed
      cex_line = 1                  # removed
      shadowtext = TRUE     
      label_style = "shadowtext"              # removed
      repel = FALSE                     # removed       
      node_label  = "category"     
      with_edge = TRUE                 # removed
      group_category = FALSE            # removed
      group_legend = FALSE              # removed                      
      cex_label_group = 1           # removed
      nWords = 4                   # removed
      label_format = 30              # removed
      clusterFunction = stats::kmeans           # removed
      nCluster = NULL                 # removed
      layout.params = list(
        layout = NULL, 
        coords = NULL             
      )
      
      edge.params = list(
        show = TRUE,              
        min = 0.2                           
      )
      cex.params = list(
        category_node = 1,        
        category_label = 1,       
        line = 1                  
      )
      hilight.params = list(
        category = NULL,          
        alpha_hilight = 1,        
        alpha_no_hilight = 0.3    
      )
      cluster.params = list(
        cluster = FALSE,          
        method = stats::kmeans,   
        n = NULL,                               
        legend = FALSE,           
        label_style = "shadowtext",
        label_words_n = 4,        
        label_format = 30         
      )
      
      
      
      enrichplot:::has_pairsim(x)
      label_size_category <- 5
      label_group <- 3
      
      # change parameter name
      ##############################################################
      params_df <- as.data.frame(rbind(
        c("layout", "layout.params", "layout"),
        c("coords", "layout.params", "coords"),
        
        c("with_edge", "edge.params", "show"),
        c("min_edge", "edge.params", "min"),
        
        c("cex_category", "cex.params", "category_node"),
        c("cex_label_category", "cex.params", "category_label"),
        c("cex_line", "cex.params", "line"),
        
        c("group_category", "cluster.params", "cluster"),
        c("clusterFunction", "cluster.params", "method"),
        c("nCluster", "cluster.params", "n"),
        c("group_legend", "cluster.params", "legend"),
        c("label_style", "cluster.params", "label_style"),
        c("nWords", "cluster.params", "label_words_n"),
        c("label_format", "cluster.params", "label_format"))
      )
      colnames(params_df) <- c("original", "listname", "present")
      rownames(params_df) <- params_df$original
      
      
      default.layout.params <- list(
        layout = NULL,                       
        coords = NULL            
      )
      
      default.edge.params <- list(
        show = TRUE,                  
        min = 0.2                             
      )
      
      default.cex.params <- list(
        category_node = 1,          
        category_label = 1,         
        line = 1                                       
      )
      
      default.cluster.params <- list(
        cluster = FALSE,            
        method = stats::kmeans,     
        n = NULL,                             
        legend = FALSE,             
        label_style = "shadowtext", 
        label_words_n = 4,          
        label_format = 30                          
      )
      
      default.hilight.params <- list(
        category = NULL,
        alpha_hilight = 1,
        alpha_no_hilight = 0.3
      )
      layout.params <- modifyList(default.layout.params, layout.params)
      edge.params <- modifyList(default.edge.params, edge.params)
      cex.params <- modifyList(default.cex.params, cex.params)
      cluster.params <- modifyList(default.cluster.params, cluster.params)
      hilight.params <- modifyList(default.hilight.params, hilight.params)
      params_list <- list(x = x,
                          showCategory = showCategory, 
                          layout = layout,                    
                          coords = coords,                    
                          color = color,     
                          min_edge = min_edge,                  
                          cex_label_category = cex_label_category,        
                          cex_category = cex_category,              
                          cex_line = cex_line,                  
                          shadowtext = shadowtext,      
                          label_style = label_style,               
                          repel = repel,                            
                          node_label  = node_label,     
                          with_edge = with_edge,                 
                          group_category = group_category,            
                          group_legend = group_legend,                                    
                          cex_label_group = cex_label_group,           
                          nWords = nWords,                    
                          label_format = label_format,              
                          clusterFunction = clusterFunction,           
                          nCluster = nCluster,                  
                          layout.params = layout.params,                    
                          edge.params = edge.params,
                          cex.params = cex.params,
                          hilight.params = hilight.params,  
                          cluster.params = cluster.params
      )
      # get all parameters value
      args <- as.list(match.call())
      removed_params <- intersect(params_df$original, names(args))
      if (length(removed_params) > 0) {
        for (i in removed_params) {
          params_list[[params_df[i, 2]]][[params_df[i, 3]]] <- get(i)
          warn <- get_param_change_message(i, params_df)
          warning(warn)
        }
      }
      
      layout.params <- params_list[["layout.params"]]
      edge.params <- params_list[["edge.params"]]
      cex.params <- params_list[["cex.params"]]
      cluster.params <- params_list[["cluster.params"]]
      hilight.params <- params_list[["hilight.params"]]
      
      layout <- layout.params[["layout"]]
      coords <- layout.params[["coords"]]
      with_edge <- edge.params[["show"]]
      min_edge <- edge.params[["min"]]
      cex_category <- cex.params[["category_node"]]
      cex_label_category <- cex.params[["category_label"]]
      cex_line <- cex.params[["line"]]
      group_category <- cluster.params[["cluster"]]
      clusterFunction <- cluster.params[["method"]]
      nCluster <- cluster.params[["n"]]
      group_legend <- cluster.params[["legend"]]
      label_style <- cluster.params[["label_style"]]
      nWords <- cluster.params[["label_words_n"]]
      label_format <- cluster.params[["label_format"]]
      hilight_category <- hilight.params[["category"]]
      alpha_hilight <- hilight.params[["alpha_hilight"]]
      alpha_nohilight <- hilight.params[["alpha_no_hilight"]]
      
      n <- enrichplot:::update_n(x, showCategory)
      y <- as.data.frame(x)
      ## get graph.data.frame() object
      g <- enrichplot:::get_igraph(x=x, nCategory=n, color=color, cex_line=cex_line,
                                   min_edge=min_edge)
      
      igraph::V(g)$size = igraph::V(g)$size/3
      
      print_time("Emapplot finished")
      self$plots$emapplot = visNetwork::visIgraph(g)
    }
    #------------------------------------------------------------------ END ----
  )
)
