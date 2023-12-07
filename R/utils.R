# Utility functions
base::source('./R/complex_functions/pca.R')
base::source('./R/complex_functions/volcano.R')
#--------------------------------------------------------- Switch functions ----

gene_ontology_switch = function(selection) {
  switch (EXPR = selection,
    'Gene ontology (ALL)' = 'ALL',
    'Gene ontology (BP)'= 'BP',
    'Gene ontology (MF)' = 'MF',
    'Gene ontology (CC)' = 'CC'
  )
}

experiment_switch = function(selection) {
  switch(EXPR = selection,
         'Lipidomics' = 'lips',
         'Proteomics' = 'prot',
         'Transcriptomics' = 'trns'
  )
}

adjustment_switch = function(selection){
  switch(EXPR = selection,
         "None" = "minus_log10_p_value",
         "Benjamini-Hochberg" = "minus_log10_p_value_bh_adj"
  )
}

adjustment_title_switch = function(selection) {
  switch(EXPR = selection,
         "minus_log10_p_value" = "-Log10(p-value)",
         "minus_log10_p_value_bh_adj" = "-Log10(BH(p-value))"
  )
}

feature_table_cols_switch = function(col) {
  switch(EXPR = col,
         'Lipid class' = 'lipid_class',
         'Double bonds (chain 1)' = 'unsat_1',
         'Carbon count (chain 1)' = 'carbons_1',
         'Double bonds (chain 2)' = 'unsat_2',
         'Carbon count (chain 2)' = 'carbons_2',
         'Double bonds (sum)' = 'unsat_sum',
         'Carbon count (sum)' = 'carbons_sum'
  )
}

r6_switch = function(exp_type, name, id, slot){
  switch(EXPR = exp_type,
         "Lipidomics" = Lips_exp$new(name = name, id = id, slot = slot),
         "Proteomics" = Prot_exp$new(name = name, id = id, slot = slot),
         "Transcriptomics" = Trns_exp$new(name = name, id = id, slot = slot)

  )
}

table_switch = function(table_name, r6) {
  switch(EXPR = table_name,
         'Imported metadata table' = r6$tables$imp_meta,
         'Raw metadata table' = r6$tables$raw_meta,
         'Imported data table' = r6$tables$imp_data,
         'Raw data table' = r6$tables$raw_data,
         'Imported feature table' = r6$tables$imp_feature_table,
         'Feature table' = r6$tables$feature_table,
         'Blank table' = r6$tables$blank_table,
         'Class normalized table' = r6$tables$class_norm_data,
         'Total normalized table' = r6$tables$total_norm_data,
         'Z-scored table' = r6$tables$z_scored_data,
         'Z-scored class normalized table' = r6$tables$z_scored_class_norm_data,
         'Z-scored total normalized table' = r6$tables$z_scored_total_norm_data,
         'Class table' = r6$tables$class_table,
         'Class table z-scored' = r6$tables$class_table_z_scored,
         'Class table total normalized' = r6$tables$class_table_total_norm,
         'Class table z-scored total normalized' = r6$tables$class_table_z_scored_total_norm,
         'Species summary table' = r6$tables$summary_species_table,
         'Class summary table' = r6$tables$summary_class_table,
         'GSEA prot list' = r6$tables$prot_list
         )
}

table_name_switch = function(table_name) {
  switch(EXPR = table_name,
         'Imported metadata table' = 'imp_meta',
         'Raw metadata table' = 'raw_meta',
         'Imported data table' = 'imp_data',
         'Raw data table' = 'raw_data',
         'Imported feature table' = 'imp_feature_table',
         'Feature table' = 'feature_table',
         'Blank table' = 'blank_table',
         'Class normalized table' = 'class_norm_data',
         'Total normalized table' = 'total_norm_data',
         'Z-scored table' = 'z_scored_data',
         'Z-scored class normalized table' = 'z_scored_class_norm_data',
         'Z-scored total normalized table' = 'z_scored_total_norm_data',
         'Class table' = 'class_table',
         'Class table z-scored' = 'class_table_z_scored',
         'Class table total normalized' = 'class_table_total_norm',
         'Class table z-scored total normalized' = 'class_table_z_scored_total_norm',
         'Species summary table' = 'summary_species_table',
         'Class summary table' = 'summary_class_table',
         'GSEA prot list' = 'prot_list'
  )
}

method_switch = function(method) {
  switch(EXPR = method,
         'minimum' = base::min,
         'mean' = base::mean,
         'median' = stats::median,
         'maximum' = base::max
  )
}

#--------------------------------------------------------------- Plot lists ----
lipidomics_plot_list = function() {
  plot_list = c("Class distribution" = "select_class_distribution",
                "Class comparison" = "select_class_comparison",
                "Volcano plot" = "select_volcano_plot",
                "Heatmap" = "select_heatmap",
                "PCA" = "select_pca",
                "Double bond plot" = "select_double_bond_plot",
                "Saturation index" = "select_satindex_plot"
  )
  return(plot_list)
}

proteomics_plot_list = function() {
  plot_list = c("Volcano plot" = "select_volcano_plot",
                "Heatmap" = "select_heatmap",
                "PCA" = "select_pca"
  )
  return(plot_list)
}

gsea_plot_list = function() {
  plot_list = c("Dot plot" = "select_dot_plot",
                "Ridge plot" = "select_ridge_plot",
                "CNET plot" = "select_cnet_plot",
                "eMap plot" = "select_emap_plot"
  )
  return(plot_list)
}

or_plot_list = function() {
  plot_list = c("Dot plot" = "select_dot_plot",
                "Bar plot" = "select_bar_plot",
                "CNET plot" = "select_cnet_plot",
                "eMap plot" = "select_emap_plot"
  )
  return(plot_list)
}

get_mofa_plot_list = function() {
  plot_list = c("Explained variance" = "select_explained_variance",
                "Factor plot" = "select_factor_plot",
                "Combined factors" = "select_combined_factors",
                "Feature weights" = "select_feature_weights",
                "Feature top weights" = "select_feature_top_weights",
                "MOFA Heatmap" = "select_mofa_heatmap",
                "Scatterplot" = "select_scatterplot"
  )
  return(plot_list)
}

get_snf_plot_list = function() {
  plot_list = c("Clusters heatmap 1 " = "select_clusters_heatmap_1",
                "Clusters heatmap 2 " = "select_clusters_heatmap_2",
                'Similarity network 1' = 'select_similarity_network_1',
                'Similarity network 2' = 'select_similarity_network_2',
                "Fusion heatmap" = "select_fusion_heatmap",
                'Similarity network fusion' = 'select_similarity_network_fusion'
  )
  return(plot_list)
}


#---------------------------------------------------------- Purge functions ----
purge_module_inputs = function(id, input_object) {
  base::invisible(
    lapply(grep(id, names(input_object), value = TRUE), function(i) {
      .subset2(input_object, "impl")$.values$remove(i)
    })
  )
}


#--------------------------------------------------------- Import functions ----

find_delim = function(path) {
  probe = paste(readLines(con = path, n = 10), collapse = "")
  sep = c("\t" = lengths(regmatches(probe, gregexpr("\t", probe))),
          "," = lengths(regmatches(probe, gregexpr(",", probe))),
          ";" = lengths(regmatches(probe, gregexpr(";", probe))))
  return(names(which.max(sep)))
}

soda_read_table = function(file_path, sep = NA, first_column_as_index = FALSE) {

  if (is.na(sep)) {
    if (stringr::str_sub(file_path, -4, -1) == ".tsv") {
      sep = '\t'
    }
  }

  if (first_column_as_index) {
    index = 1
  } else {
    index = NULL
  }

  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table = as.data.frame(readxl::read_xlsx(file_path))
  } else {
    if (is.na(sep)) {
      sep = find_delim(path = file_path)
      data_table = read.csv(file_path,
                            header = T,
                            sep = sep,
                            check.names = FALSE)
    } else {
      data_table = read.csv(file_path,
                            header = T,
                            sep = sep,
                            check.names = FALSE)
    }

  }

  if (!is.null(index)) {
    duplicates = duplicated(data_table[,index])
    if (sum(duplicates) > 0) {
      print(paste0('Removed ', sum(duplicates), ' duplicated features'))
      data_table = data_table[!duplicates,]
      rownames(data_table) = data_table[,1]
      data_table[,1] = NULL
    }
  }

  original_count = ncol(data_table)
  if (original_count > 1) {
    data_table = data_table[,!base::duplicated(colnames(data_table))]
    final_count = ncol(data_table)
    if(original_count != final_count) {
      print(paste0('Removed ', original_count - final_count, ' duplicated columns'))
    }
  }
  return(data_table)
}

augment_feature_table = function(feature_table, external_table_name, external_feature_table) {
  feature_table$merge_on = rownames(feature_table)
  external_feature_table$merge_on = rownames(external_feature_table)

  feature_table = base::merge(feature_table, external_feature_table, by = 'merge_on', all.x = TRUE, suffixes = c('', paste0('_', external_table_name)))

  rownames(feature_table) = feature_table$merge_on
  feature_table$merge_on = NULL
  return(feature_table)
}

annotate_go = function(feature_names,
                       keyType,
                       ont,
                       pvalueCutoff) {
  # Checks
  if (!(ont %in% c('ALL', 'BP', 'MF', 'CC'))) {
    print('ont should be one of [ALL, BP, MF, CC]')
    return()
  }

  # Get GO annotations
  go_enrich_data = clusterProfiler::enrichGO(gene = feature_names,
                                             OrgDb = 'org.Hs.eg.db',
                                             keyType = keyType,
                                             ont = ont,
                                             pvalueCutoff = pvalueCutoff)

  # Extract the GO table & filter
  go_table = go_enrich_data@result
  go_table = go_table[go_table$p.adjust < pvalueCutoff,]

  if (nrow(go_table) == 0) {
    return(NULL)
  }

  if (ont != 'ALL') {
    go_table$ONTOLOGY = ont
  }

  # Split geneIDs by '/'
  feature_id = strsplit(go_table$geneID, "/")

  # Repeat GO terms based on the number of feature_id it corresponds to
  goIDs_rep = rep(go_table$ID, sapply(feature_id, length))

  # Convert feature_id list to dataframe
  feature_table = data.frame(feature_id = unlist(feature_id), ID = goIDs_rep, stringsAsFactors = FALSE)

  # Group by gene and concatenate GO terms
  feature_table = aggregate(ID ~ feature_id, data = feature_table, FUN = function(x) paste(unique(x), collapse = "|"))
  rownames(feature_table) = feature_table$feature_id
  feature_table$feature_id = NULL

  # Add features not associates with go terms
  missing_features = feature_names[!(feature_names %in% rownames(feature_table))]
  missing_features = data.frame(feature_id = missing_features,
                                ID = NA)
  rownames(missing_features) = missing_features$feature_id
  missing_features$feature_id = NULL
  feature_table = rbind(feature_table, missing_features)


  return(list(
    go_table = go_table[,c('Description', 'ONTOLOGY')],
    feature_table = feature_table
  ))

}


#-------------------------------------------------------- General utilities ----

is_coercible_to_numeric = function(vector) {
  numeric_values = suppressWarnings(as.numeric(vector))
  if (any(is.na(numeric_values))) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

unique_na_rm = function(vector) {
  vector = vector[!is.na(vector)]
  vector = unique(vector)
  return(vector)
}

drop_rows = function(data_table, rows) {
  return(data_table[!(row.names(data_table) %in% rows),])
}

keep_rows = function(data_table, rows) {
  return(data_table[(row.names(data_table) %in% rows),])
}

drop_cols = function(data_table, cols) {
  return(data_table[,!(colnames(data_table) %in% cols)])
}

remove_empty_cols = function(table) {
  # filter out columns which are only NA
  del_cols = c()
  for (col in colnames(table)) {
    if (sum(is.na(table[,col])) == length(rownames(table))){
      del_cols = c(del_cols, col)
    }
  }
  if (!is.null(del_cols)) {
    table = table[,-which(colnames(table) %in% del_cols)]
  }
  return(table)
}

is_num_coercible = function(x) {
  all(grepl('^(?=.)([+-]?([0-9]*)(\\.([0-9]+))?)$', x, perl = TRUE))
}

#----------------------------------------------------- Print time functions ----

get_time = function() {
  return(format(Sys.time(), "%H:%M:%S"))
}

get_time_code = function() {
  return(format(Sys.time(), "%H%M%S"))
}

timestamped_name = function(file_name) {
  return(paste0(get_time_code(), '_', file_name))
}

print_t = function(in_print) {
  print(paste0(get_time(), " - ", in_print))
}

print_tm = function(m, in_print) {
  print(paste0(get_time(), " ", m, " - ", in_print))
}

#---------------------------------------------------------------- Plotboxes ----
# Plotly plotbox
get_plotly_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    plotly::plotlyOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}

# NetworkD3 plotbox
get_networkd3_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    networkD3::simpleNetworkOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}


# Visnet plotbox (for networks)
get_visnet_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    visNetwork::visNetworkOutput(outputId = ns(paste0(id, "_plot")),
                                 width = dimensions_obj$xpx * dimensions_obj$x_plot,
                                 height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}

# Regular plotbox (static plots)
get_plot_box = function(id, label, dimensions_obj, session) {

  ns = session$ns

  bs4Dash::box(
    id = ns(paste0(id, "_plotbox")),
    title = label,
    width = dimensions_obj$xbs,
    height = dimensions_obj$ypx * dimensions_obj$y_box,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "gray",
    sidebar = bs4Dash::boxSidebar(
      id = ns(paste0(id, "_sidebar")),
      width = 40,
      shiny::uiOutput(
        outputId = ns(paste0(id, "_sidebar_ui"))
      )
    ),
    shiny::plotOutput(
      outputId = ns(paste0(id, "_plot")),
      width = dimensions_obj$xpx * dimensions_obj$x_plot,
      height = dimensions_obj$ypx * dimensions_obj$y_plot
    )
  )
}

#----------------------------------------------------- Lipidomics functions ----

get_group_median_table = function(data_table,
                                  meta_table,
                                  group_col) {
  unique_groups = unique(meta_table[,group_col])
  out_table = as.data.frame(matrix(data = NA,
                                   nrow = length(unique_groups),
                                   ncol = ncol(data_table)))
  colnames(out_table) = colnames(data_table)
  rownames(out_table) = unique_groups

  for (group in unique_groups) {
    idx = rownames(meta_table)[which(meta_table[,group_col] == group)]

    group_table = data_table[idx,]

    if (length(idx) == 1) {
      group_values = group_table
    } else {
      group_values = apply(group_table,2,median, na.rm = TRUE)
    }

    # group_values[is.na(group_values)] = 0.0
    group_values[group_values == 0] = NA
    out_table[group,] = group_values
  }
  return(out_table)
}

get_lipid_class_table = function(table){

  # Get unique lipid classes
  classes = get_lipid_classes(feature_list = colnames(table), uniques = TRUE)

  # Get a column vector to find easily which columns belong to each lipid group
  col_vector = get_lipid_classes(feature_list = colnames(table), uniques = FALSE)

  # Fill the table
  out_table = sapply(X = classes,
                     FUN = function(x) {
                       col_list = which(col_vector == x)
                       if (length(col_list) > 1) {
                         rowSums(table[,col_list], na.rm = T)
                       } else {
                         table[,col_list]
                       }
                     }
  )

  return(out_table)
}

normalise_lipid_class = function(lips_table) {
  # Get classes and unique classes for the lipid features
  classes = get_lipid_classes(feature_list = as.character(colnames(lips_table)), uniques = FALSE)
  classes_unique = get_lipid_classes(feature_list = as.character(colnames(lips_table)), uniques = TRUE)

  # For each unique lipid class...
  for (lip_class in classes_unique){

    # Get columns from that class...
    cols = which(classes == lip_class)
    if (length(cols) > 1) {
      class_row_sums = rowSums(lips_table[, cols], na.rm = T)
    } else {
      class_row_sums = lips_table[, cols]
      class_row_sums[is.na(class_row_sums)] = 0
    }
    class_row_sums[class_row_sums == 0] = 1
    lips_table[, cols] = lips_table[, cols] / class_row_sums
  }
  return(lips_table)
}

z_score_normalisation = function(data_table) {

  data_table = apply(data_table, 2, function(col) {
    centered_row = col - base::mean(col, na.rm = T)
    scaled_row = centered_row / sd(centered_row, na.rm = T)
    return(scaled_row)
  })
  return(data_table)
}

impute_na = function(method, data_table, meta_table, group_col, sample_rownames, val_threshold) {
  imputation_func = method_switch(method)
  data_table = remove_empty_cols(data_table)
  groups = unique(meta_table[sample_rownames, group_col])
  for (g in groups) {
    g_rows = rownames(meta_table)[meta_table[, group_col] == g]
    g_rows = intersect(g_rows, sample_rownames)
    samples_total = length(g_rows)
    selected_features = colnames(data_table)[(colSums(!is.na(data_table[g_rows,])) / samples_total) >= val_threshold]
    for (f in selected_features){
      g_vector = data_table[g_rows,f]
      names(g_vector) = g_rows
      imputation_value = imputation_func(g_vector, na.rm = T)
      na_rownames = names(g_vector)[is.na(data_table[g_rows,f])]
      data_table[na_rownames,f] = imputation_value
    }
  }
  return(data_table)
}


get_lipid_classes = function(feature_list, uniques = TRUE){
  classes = sapply(feature_list, function(x)
    strsplit(x = x,
             split = " ",
             fixed = TRUE)[[1]][1])
  classes = as.vector(classes)
  if (uniques) {
    return(unique(classes))}
  else{
    return(classes)
  }
}

get_feature_metadata = function(data_table, dtype) {

  if (!(dtype %in% c('lipidomics','proteomics', 'transcriptomics', 'genomics'))) {
    print('Error: dtype should be one of [lipidomics, proteomics, transcriptomics, genomics]')
    return()
  }

  if (dtype == 'lipidomics') {
    feature_table = data.frame(row.names = sort(colnames(data_table)))
    feature_table$lipid_class = get_lipid_classes(feature_list = rownames(feature_table),
                                                  uniques = FALSE)
    # Collect carbon and unsaturation counts
    c_count_1 = c() # Main carbon count / total carbon count (TGs)
    s_count_1 = c() # Main saturation count
    c_count_2 = c() # Secondary carbon count (asyl groups or TGs)
    s_count_2 = c() # Secondary saturation (asyl groups or TGs)
    for (c in unique(feature_table$lipid_class)) {
      idx = rownames(feature_table)[feature_table$lipid_class == c]

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
          c_count_2 = c(c_count_2, 0)
          s_count_1 = c(s_count_1, i[3])
          s_count_2 = c(s_count_2, 0)
        }
      }
    }

    feature_table$carbons_1 = as.numeric(c_count_1)
    feature_table$carbons_2 = as.numeric(c_count_2)
    feature_table$carbons_sum = feature_table$carbons_1 + feature_table$carbons_2
    feature_table$unsat_1 = as.numeric(s_count_1)
    feature_table$unsat_2 = as.numeric(s_count_2)
    feature_table$unsat_sum = feature_table$unsat_1 + feature_table$unsat_2

  } else if (dtype %in% c('proteomics', 'transcriptomics', 'genomics')) {

    features = colnames(data_table)
    feature_table = data.frame(row.names = features)
  }


  return(feature_table)
}

get_col_means = function(data_table) {
  means = colMeans(data_table, na.rm = TRUE)
  means[is.na(means)] = 0
  return(means)
}

blank_filter = function(data_table, blank_table, blank_multiplier, sample_threshold, saved_cols = FALSE) {
  # Find features / columns below threshold
  blank_means = get_col_means(data_table = blank_table)
  del_cols = c()
  total_samples = nrow(data_table)
  for (col in colnames(data_table)){
    threshold = blank_multiplier * blank_means[col]
    above_threshold = sum(data_table[, col] >= threshold, na.rm = T)
    if ((above_threshold/total_samples) < sample_threshold) {
      del_cols = c(del_cols, col)
    }
  }
  if (saved_cols) {
    del_cols = setdiff(colnames(data_table), del_cols)
  }
  return(del_cols)
}

# Implementation of blank filtering methods with r6 object
lips_get_del_cols = function(data_table,
                             blank_table,
                             imp_meta,
                             raw_meta,
                             idx_blanks,
                             idx_samples,
                             id_col_meta,
                             group_col,
                             batch_col,
                             blank_multiplier,
                             sample_threshold,
                             group_threshold) {

  # Blank filtering
  del_cols = c()
  all_batches = unique(imp_meta[, batch_col])
  for (b in all_batches) {
    batch_idx = which(imp_meta[, batch_col] == b)
    batch_blanks = base::intersect(batch_idx, idx_blanks)
    batch_samples = base::intersect(batch_idx, idx_samples)

    # Get rownames
    batch_blanks = imp_meta[batch_blanks, id_col_meta]
    batch_samples = imp_meta[batch_samples, id_col_meta]
    batch_samples = base::intersect(rownames(data_table), batch_samples)

    del_cols = c(del_cols, blank_filter(data_table = data_table[batch_samples,],
                                        blank_table = blank_table[as.character(batch_blanks),],
                                        blank_multiplier = blank_multiplier,
                                        sample_threshold = sample_threshold))
  }

  del_cols = unique(del_cols)
  del_cols = sort(del_cols)

  if (is.null(del_cols)) {
    return(del_cols)
  }

  # Group filtering
  saved_cols = c()
  for (g in unique(raw_meta[,group_col])) {
    group_idx = which(imp_meta[, group_col] == g)
    above_threshold = rep(0, length(del_cols))
    names(above_threshold) = del_cols
    for (b in unique(imp_meta[group_idx, batch_col])) {

      batch_idx = which(imp_meta[, batch_col] == b)
      batch_blanks = base::intersect(batch_idx, idx_blanks)
      batch_samples = base::intersect(batch_idx, group_idx)

      # Get rownames
      batch_blanks = imp_meta[batch_blanks, id_col_meta]
      batch_samples = imp_meta[batch_samples, id_col_meta]
      batch_samples = base::intersect(rownames(data_table), batch_samples)

      # get batch blank means
      blank_means = get_col_means(data_table = blank_table[as.character(batch_blanks),])
      threshold = blank_multiplier * blank_means

      # Find features / columns below threshold
      for (col in del_cols) {
        above_threshold[col] = above_threshold[col] + sum(data_table[batch_samples,col] >= threshold[col], na.rm = T)
      }
    }
    above_threshold = above_threshold / length(group_idx) >= group_threshold
    saved_cols = c(saved_cols, names(above_threshold)[above_threshold])
  }

  saved_cols = unique(saved_cols)
  saved_cols = sort(saved_cols)

  del_cols = setdiff(del_cols, saved_cols)

  return(del_cols)
}

#------------------------------------------------------- Plotting functions ----

pca_plot_scores = function(x, y, meta_table, group_col, width, height, colour_list){
  groups = unique(meta_table[,group_col])
  fig = plotly::plot_ly(colors = colour_list, width = width, height = height)
  i = 1
  for (grp in groups) {
    idx = rownames(meta_table)[which(meta_table[,group_col] == grp)]
    fig = fig %>% add_trace(x = x[idx], y = y[idx],
                            name = grp, color = colour_list[i],
                            type  = "scatter", mode = "markers",
                            text = idx,
                            hoverinfo = "text",
                            legendgroup=grp)
    i = i + 1
  }
  fig = fig %>% layout(shapes = list(hline(0),
                                     vline(0),
                                     circle(x, y)),
                       legend = list(title=list(text = paste0('<b>', group_col, ': </b>'))))
  return(fig)
}

pca_plot_loadings = function(x, y, feature_list, width, height, colour_list){
  fig = plotly::plot_ly(colors = colour_list, width = width, height = height)
  fig = fig %>% add_trace(x = x, y = y,
                          type = "scatter", mode = "text", text = feature_list,
                          textposition = 'middle right', showlegend = F)

  shape_list = list(
    hline(0),
    vline(0)
  )


  for (i in 1:length(feature_list)) {
    feature = feature_list[i]
    new_line = list(
      type = "line",
      line = list(color = "pink"),
      xref = "x",
      yref = "y",
      x0 = 0,
      y0 = 0,
      x1 = x[i],
      y1 = y[i]
    )
    shape_list[[length(shape_list) + 1]] = new_line
  }

  fig = fig %>% layout(shapes = shape_list)

  return(fig)
}

hline = function(y = 0, color = "black", dash = NULL) {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color, dash=dash)
  )
}

vline <- function(x = 0, color = "black", dash = NULL) {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color, dash=dash)
  )
}

#' @title Calculate Hoteling T2
#'
#' @description Calculate Hoteling T2 for the scores plot
#'
#' @param x numeric vector with x values
#' @param y numeric vector with y values
#' @param alpha numeric(1), confidence interval
#' @param len numeric(1), number of points to create the ellipse
#'
#' @return A list is returned to be used in a plotly graph.
#'
#' @details This is a helper function which is used to create a confidence (Hotelling T2) interval for a
#'     PCA score plot.
#'
#' @importFrom stats var qf
#'
#' @noRd
#'
#' @author Damien Olivier
circle = function(x, y, alpha = 0.95, len = 200){
  N = length(x)
  mypi = seq(0, 2 * pi, length = len)
  r1 = sqrt(stats::var(x) * stats::qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  r2 = sqrt(stats::var(y) * stats::qf(alpha, 2, N - 2) * (2*(N^2 - 1)/(N * (N - 2))))
  list(
    type = "circle",
    xref = "x",
    x0= -r1,
    x1 = r1,
    yref = "y",
    y0 = -r2,
    y1 = r2,
    line = "black",
    opacity = 0.2
  )
}

lipidomics_summary_plot = function(r6, data_table) {
  groups = get_lipid_classes(colnames(r6$tables$imp_data)[2:length(colnames(r6$tables$imp_data))], uniques = T)

  plot_table = data.frame(table(base::factor((get_lipid_classes(colnames(data_table), uniques = F)), levels = groups)))
  names(plot_table) = c("class", "raw")
  plot_table$imported = table(base::factor((get_lipid_classes(colnames(r6$tables$imp_data)[2:length(colnames(r6$tables$imp_data))], uniques = F)), levels = groups))
  plot_table$removed = plot_table$imported - plot_table$raw

  absolute_counts = as.data.frame(base::matrix(nrow = 2*nrow(plot_table)))
  absolute_counts$classes = c(plot_table$class, plot_table$class)
  absolute_counts$values = c(plot_table$removed, plot_table$raw)
  absolute_counts$status = c(rep('kept', nrow(plot_table)), rep('removed', nrow(plot_table)))
  absolute_counts$V1 = NULL

  relative_counts = absolute_counts
  relative_counts$values = round(100*(relative_counts$values/c(plot_table$imported, plot_table$imported)))

  plot_1 = ggplot(absolute_counts ,
                  aes(
                    fill=status ,
                    y=values ,
                    x=classes ,
                    label = values )) +
    ggtitle("Absolute compound count")+
    geom_bar(position="stack",
             stat="identity",
             show.legend = FALSE) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent')
    ) +
    scale_fill_manual(values = c("#D2E8F5", "#007bff"))+
    coord_flip() +
    scale_y_reverse(limits = c(max(plot_table$imported), 0))


  plot_2 = ggplot(relative_counts,
                  aes(
                    fill=status ,
                    y=values ,
                    x=classes ,
                    label = values )) +
    ggtitle("Relative compound count (%)")+
    geom_bar(position="stack",
             stat="identity",
             show.legend = FALSE) +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme(
      axis.title.y=element_blank(),
      axis.title.x=element_blank(),
      axis.text.y=element_text(hjust = 0.4),
      axis.ticks.y=element_blank(),
      axis.ticks.x=element_blank(),
      plot.title = element_text(hjust = 0.5),
      panel.background = element_rect(fill='transparent'),
      plot.background = element_rect(fill='transparent', color=NA),
      legend.background = element_rect(fill='transparent')
    ) +
    scale_fill_manual(values = c("#D2E8F5", "#007bff"))+
    coord_flip()

  return(gridExtra::grid.arrange(plot_1, plot_2, ncol=2))
}



#--------------------------------------------------------------- Statistics ----

get_pca_data = function(data_table){

  pca_data = pcaMethods::pca(object = data_table,
                             nPcs = 2,
                             scale = "none",
                             cv = "q2",
                             completeObs = T)

  return(pca_data)
}

apply_discriminant_analysis = function(data_table, group_list, nlambda = 100, alpha = 0.8) {

  ncol_1 = ncol(data_table)
  data_table = data_table[,!is.na(colSums(data_table))]
  ncol_2 = ncol(data_table)
  if(ncol_2 != ncol_1) {
    print(paste0("Discriminant analysis : dropped ", ncol_1 - ncol_2, " features with no signal variation."))
  }

  count = table(group_list)
  if (any(count < 3)) {
    dead_groups = names(count)[count < 3]
    print(paste0("Warning: ", length(dead_groups), " groups with fewer than 3 samples, dropped from analysis."))
    data_table = data_table[!(group_list %in% dead_groups),]
    group_list = group_list[!(group_list %in% dead_groups)]
  }

  if (length(unique(group_list) > 2)) {
    family = "multinomial"
  } else {
    family = "binomial"
  }

  coef = NULL
  attempt_count = 1
  while(is.null(coef)) {
    print(paste0("Discriminant analysis: attempt ", attempt_count))
    if (attempt_count == 5) {break}
    attempt_count = attempt_count + 1
    base::tryCatch(
      {
        coef = glmnet::cv.glmnet(data_table,
                                 group_list,
                                 nlambda = nlambda,
                                 alpha = alpha,
                                 family = family,
                                 type.multinomial = "grouped")

      },error=function(e){
      },finally={}
    )
  }

  coef = stats::coef(coef, s = "lambda.min")
  keep_cols = as.matrix(coef[[1]])

  keep_cols = rownames(keep_cols)[which(keep_cols != 0)]
  keep_cols = keep_cols[2:length(keep_cols)]
  data_table = data_table[,keep_cols]
  return(data_table)
}


get_fold_changes = function(data_table, idx_group_1, idx_group_2, used_function, impute_inf = T) {

  if (used_function == "median") {
    av_function = function(x) {return(median(x, na.rm = T))}
  } else {
    av_function = function(x) {return(mean(x, na.rm = T))}
  }


  fold_changes = apply(data_table, 2, function(column) {
    mean_group1 = base::mean(column[idx_group_1], na.rm = T)
    mean_group2 = base::mean(column[idx_group_2], na.rm = T)

    # Impute NA means with 0
    if (is.na(mean_group1)) mean_group1 = 0
    if (is.na(mean_group2)) mean_group2 = 0

    fold_change = mean_group2 / mean_group1

    return(fold_change)
  })

  if (impute_inf) {
    # Impute infinite (x/0)
    if (length(which(fold_changes == Inf)) > 0) {
      fold_changes[which(fold_changes == Inf)] = max(fold_changes[which(fold_changes != Inf)]) * 1.01
    }

    # Impute zeros (0/x)
    if (length(which(fold_changes == 0)) > 0) {
      fold_changes[which(fold_changes == 0)] = min(fold_changes[which(fold_changes > 0)]) * 0.99
    }
  }

  # Impute NaNs (0/0)
  if (length(which(is.nan(fold_changes)) > 0)) {
    fold_changes[which(is.nan(fold_changes))] = 1
  }

  return(fold_changes)

}

get_p_val = function(data_table, idx_group_1, idx_group_2, used_function, impute_na = T) {

  if (used_function == "Wilcoxon") {
    test_function=function(x,y){

      if(all(x==mean(x, na.rm = T))&all(y==mean(y, na.rm = T))) {
        return(1)
      } else{
        return(stats::wilcox.test(x, y)$p.value)
      }
    }
  } else if (used_function == "t-Test") {
    test_function=function(x,y){

      if(all(x==mean(x, na.rm = T))&all(y==mean(y, na.rm = T))) {
        return(1)
      } else{
        return(stats::t.test(x, y)$p.value)
      }
    }

  }

  p_values = apply(data_table, 2, function(column) {
    group1 = column[idx_group_1]
    group2 = column[idx_group_2]

    # Check if there are enough non-NA values to conduct a t-test
    if (sum(!is.na(group1)) < 2 || sum(!is.na(group2)) < 2) {
      return(NA)  # Return NA if not enough data
    }

    test_result = test_function(group1, group2)  # Assuming equal variance
    return(test_result)
  })


  if ((length(which(is.na(p_values))) > 0) & impute_na){
    p_values[which(is.na(p_values))] = min(p_values, na.rm = T) * 0.99
  }


  return(p_values)
}



get_fc_and_pval = function(data_table, idx_group_1, idx_group_2, used_function, test){

  if (used_function == "median") {
    av_function = function(x) {return(median(x, na.rm = T))}
  } else {
    av_function = function(x) {return(mean(x, na.rm = T))}
  }

  if (test == "Wilcoxon") {
    test_function=function(x,y){

      if(all(x==mean(x, na.rm = T))&all(y==mean(y, na.rm = T))) {
        return(1)
      } else{
        return(stats::wilcox.test(x, y)$p.value)
      }
    }
  } else if (test == "t-Test") {
    test_function=function(x,y){

      if(all(x==mean(x, na.rm = T))&all(y==mean(y, na.rm = T))) {
        return(1)
      } else{
        return(stats::t.test(x, y)$p.value)
      }
    }

  }

  # Collect fold change and p-values
  fold_change = c()
  p_value = c()

  sorted_cols = sort(colnames(data_table))

  for (col in sorted_cols) {

    # If both groups contain data
    if (length(na.exclude(data_table[idx_group_1, col])) > 0 & length(na.exclude(data_table[idx_group_2, col])) > 0) {

      # If at least one of the groups contains only one value
      if ((length(na.exclude(data_table[idx_group_1, col])) == 1) | (length(na.exclude(data_table[idx_group_2, col])) == 1)) {
        fold_change = c(fold_change, av_function(data_table[idx_group_2, col]) / av_function(data_table[idx_group_1, col]))
        p_value = c(p_value, NA)
      } else {

        # If there is actual comparable data
        fold_change = c(fold_change, av_function(data_table[idx_group_2, col]) / av_function(data_table[idx_group_1, col]))
        p_value = c(p_value, test_function(data_table[idx_group_1, col], data_table[idx_group_2, col]))
      }

    } else {
      # If at least one of the groups is full NA, default values
      p_value = c(p_value, 666)
      # For fold changes, if it is the denominator
      if (length(na.exclude(data_table[idx_group_1, col])) == 0) {
        fold_change = c(fold_change, 777)
      } else {
        # If it is the numerator
        fold_change = c(fold_change, 666)
      }
    }
  }

  # Imputation of Inf for when denominator average is 0
  fold_change[fold_change == Inf] = 1.01*max(fold_change[!(fold_change == 777) & !(fold_change == 666) & !(fold_change == Inf)], na.rm = T)

  # Imputation of 0 for when numerator average is 0
  fold_change[fold_change == 0] = 0.99*min(fold_change[!(fold_change == 0)], na.rm = T)

  # Imputation of NAs for denominator FC with a value slightly above max FC
  fold_change[fold_change == 777] = 1.01*max(fold_change[!(fold_change == 777) & !(fold_change == 666) & !(fold_change == Inf)], na.rm = T)

  # Imputation of NAs for nominator FC with a value slightly below min FC
  fold_change[fold_change == 666] = 0.99*min(fold_change[!(fold_change == 0)], na.rm = T)

  # Imputation of NAs for when both numerators and denominator medians are 0
  fold_change[is.na(fold_change)] = 1

  # Imputation of NAs for p-values to be the min p-val
  p_value[p_value == 666] = 0.99*min(p_value, na.rm = T)

  # Adjust p-value
  p_value_bh_adj = p.adjust(p_value, method = "BH")

  return(list("fold_change" = fold_change,
              "p_value" = p_value,
              "p_value_bh_adj" = p_value_bh_adj))
}



#--------------------------------------------------------- Example datasets ----
example_lipidomics = function(name, id = NA, slot = NA, data = './examples/multiomics/lipidomics.csv', meta = './examples/multiomics/lipidomics_metadata.csv') {

  lips_data = soda_read_table(data)
  meta_data = soda_read_table(meta)

  r6 = Lips_exp$new(name = name, id = id, slot = slot, preloaded = T)

  r6$tables$imp_meta = meta_data
  r6$tables$imp_data = lips_data

  r6$indices$id_col_meta = 'ID'
  r6$indices$id_col_data = 'ID'

  r6$indices$group_col = 'Group_type'
  r6$indices$batch_col = 'Batch'
  r6$set_raw_meta()

  type_vector = r6$tables$imp_meta[, 'Sample_type']
  blank_idx = grep(pattern = 'blank',
                   x = type_vector,
                   ignore.case = TRUE)
  qc_idx = grep(pattern = 'Quality',
                x = type_vector,
                ignore.case = TRUE)
  pool_idx = grep(pattern = 'Pool',
                  x = type_vector,
                  ignore.case = TRUE)

  sample_idx = 1:nrow(r6$tables$imp_meta)
  sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

  r6$indices$idx_blanks = blank_idx
  r6$indices$idx_qcs = qc_idx
  r6$indices$idx_pools = pool_idx
  r6$indices$idx_samples = sample_idx

  r6$indices$rownames_blanks = r6$tables$imp_meta[blank_idx, r6$indices$id_col_meta]
  r6$indices$rownames_qcs = r6$tables$imp_meta[qc_idx, r6$indices$id_col_meta]
  r6$indices$rownames_pools = r6$tables$imp_meta[pool_idx, r6$indices$id_col_meta]
  r6$indices$rownames_samples = r6$tables$imp_meta[sample_idx, r6$indices$id_col_meta]

  r6$tables$raw_meta = r6$tables$raw_meta[r6$indices$rownames_samples,]

  r6$get_blank_table()

  r6$set_raw_data(apply_imputation = F,
                  impute_before = F,
                  apply_filtering = T,
                  imputation_function = 'minimum',
                  val_threshold = 0.6,
                  blank_multiplier = 2,
                  sample_threshold = 0.8,
                  group_threshold = 0.8,
                  norm_col = '')

  r6$derive_data_tables()

  return(r6)
}

example_proteomics = function(name = 'prot_example', id = NA, slot = NA, data = './examples/multiomics/proteomics_2.tsv', meta = './examples/multiomics/metadata.csv') {
  prot_data = soda_read_table(data)
  meta_data = soda_read_table(meta)

  r6 = Prot_exp$new(name = name, id = id, slot = slot, preloaded = T)

  r6$tables$imp_meta = meta_data
  r6$tables$imp_data = prot_data

  r6$indices$id_col_meta = 'ID'
  r6$indices$id_col_data = 'ID'

  r6$indices$group_col = 'Group_type'
  r6$indices$batch_col = 'Batch'
  r6$set_raw_meta()

  type_vector = r6$tables$imp_meta[, 'Sample_type']
  blank_idx = grep(pattern = 'blank',
                   x = type_vector,
                   ignore.case = TRUE)
  qc_idx = grep(pattern = 'QC',
                x = type_vector,
                ignore.case = TRUE)
  pool_idx = grep(pattern = 'Pool',
                  x = type_vector,
                  ignore.case = TRUE)

  sample_idx = 1:nrow(r6$tables$imp_meta)
  sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

  r6$indices$idx_blanks = blank_idx
  r6$indices$idx_qcs = qc_idx
  r6$indices$idx_pools = pool_idx
  r6$indices$idx_samples = sample_idx

  r6$indices$rownames_blanks = r6$tables$imp_meta[blank_idx, r6$indices$id_col_meta]
  r6$indices$rownames_qcs = r6$tables$imp_meta[qc_idx, r6$indices$id_col_meta]
  r6$indices$rownames_pools = r6$tables$imp_meta[pool_idx, r6$indices$id_col_meta]
  r6$indices$rownames_samples = r6$tables$imp_meta[sample_idx, r6$indices$id_col_meta]

  r6$tables$raw_meta = r6$tables$raw_meta[r6$indices$rownames_samples,]

  r6$get_blank_table()

  r6$set_raw_data(apply_imputation = F,
                      impute_before = F,
                      apply_filtering = F,
                      imputation_function = 'minimum',
                      val_threshold = 0.6,
                      blank_multiplier = 2,
                      sample_threshold = 0.8,
                      group_threshold = 0.8,
                      norm_col = '')

  r6$derive_data_tables()

  # r6$get_prot_list()
  # r6$get_gsea_object()

  return(r6)
}

example_transcriptomics = function(name = 'trns_example', id = NA, slot = NA, data = './examples/multiomics/transcriptomics_2_genename_test.tsv', meta = './examples/multiomics/metadata.csv') {
  trns_data = soda_read_table(data)
  meta_data = soda_read_table(meta)

  r6 = Trns_exp$new(name = name, id = id, slot = slot, preloaded = T)

  r6$tables$imp_meta = meta_data
  r6$tables$imp_data = trns_data

  r6$indices$id_col_meta = 'ID'
  r6$indices$id_col_data = 'ID'

  r6$indices$group_col = 'Group_type'
  r6$indices$batch_col = 'Batch'
  r6$set_raw_meta()

  type_vector = r6$tables$imp_meta[, 'Sample_type']
  blank_idx = grep(pattern = 'blank',
                   x = type_vector,
                   ignore.case = TRUE)
  qc_idx = grep(pattern = 'QC',
                x = type_vector,
                ignore.case = TRUE)
  pool_idx = grep(pattern = 'Pool',
                  x = type_vector,
                  ignore.case = TRUE)

  sample_idx = 1:nrow(r6$tables$imp_meta)
  sample_idx = setdiff(sample_idx, c(blank_idx, qc_idx, pool_idx))

  r6$indices$idx_blanks = blank_idx
  r6$indices$idx_qcs = qc_idx
  r6$indices$idx_pools = pool_idx
  r6$indices$idx_samples = sample_idx

  r6$indices$rownames_blanks = r6$tables$imp_meta[blank_idx, r6$indices$id_col_meta]
  r6$indices$rownames_qcs = r6$tables$imp_meta[qc_idx, r6$indices$id_col_meta]
  r6$indices$rownames_pools = r6$tables$imp_meta[pool_idx, r6$indices$id_col_meta]
  r6$indices$rownames_samples = r6$tables$imp_meta[sample_idx, r6$indices$id_col_meta]

  r6$tables$raw_meta = r6$tables$raw_meta[r6$indices$rownames_samples,]

  r6$get_blank_table()

  r6$set_raw_data(apply_imputation = F,
                  impute_before = F,
                  apply_filtering = F,
                  imputation_function = 'minimum',
                  val_threshold = 0.6,
                  blank_multiplier = 2,
                  sample_threshold = 0.8,
                  group_threshold = 0.8,
                  norm_col = '')

  r6$derive_data_tables()

  # r6$get_prot_list()
  # r6$get_gsea_object()

  return(r6)
}

#---------------------------------------------- Enrichment & GSEA utilities ----
get_sparse_matrix = function(features_go_table, all_go_terms, sep = '|') {
  go_list = vector("list", nrow(features_go_table))
  # Loop through each row and split the 'go_terms' column by '|'
  for (i in 1:nrow(features_go_table)) {
    if (is.na(features_go_table[i,1])) {
      go_list[[i]] = NA
    } else {
      go_list[[i]] = strsplit(as.character(features_go_table[i,1]), sep, fixed = TRUE)[[1]]
    }
  }


  # Initialize a list to store the one-hot encoded vectors
  one_hot_list = vector("list", nrow(features_go_table))

  # Loop through each gene and create a one-hot encoded vector
  for (i in seq_along(rownames(features_go_table))) {
    # Create a boolean vector for the presence of each GO term
    one_hot_vector = all_go_terms %in% go_list[[i]]
    # Add the vector to the list
    one_hot_list[[i]] = one_hot_vector
  }

  # Combine the one-hot encoded vectors into a matrix
  sparse_matrix = do.call(rbind, one_hot_list)

  # Convert the matrix to a sparse matrix
  sparse_matrix = Matrix::Matrix(sparse_matrix, sparse = TRUE)

  # Add row and column names to the sparse matrix
  rownames(sparse_matrix) = rownames(features_go_table)
  colnames(sparse_matrix) = all_go_terms
  return(sparse_matrix)
}

match_go_terms = function(terms_list, sparse_table) {
  if (length(terms_list) > 1) {
    matches = rowSums(sparse_table[,terms_list])
  } else {
    matches = sparse_table[,terms_list]
  }
  return(matches)
}

get_term2gene = function(feature_table, column, sep = "\\|") {
  term2gene=sapply(feature_table[,column], FUN = function(x) strsplit(x,sep)[[1]])
  names(term2gene)=rownames(feature_table)
  term2gene = utils::stack(term2gene)
  return(term2gene)
}

custom_ora = function(geneList, pvalueCutoff = 0.05, pAdjustMethod = "BH", qvalueCutoff = 0.2, minGSSize = 10, maxGSSize = 500, term2gene) {
  enricher_result = clusterProfiler::enricher(gene = geneList,
                                              pvalueCutoff = pvalueCutoff,
                                              pAdjustMethod = pAdjustMethod,
                                              qvalueCutoff = qvalueCutoff,
                                              minGSSize = minGSSize,
                                              maxGSSize  = maxGSSize,
                                              TERM2GENE=term2gene)
  return(enricher_result)
}
custom_gsea = function(geneList, minGSSize = 10, maxGSSize = 500, pvalueCutoff = 0.05, verbose = TRUE, pAdjustMethod = "BH", term2gene) {
  gsea_result = clusterProfiler::GSEA(geneList = geneList,
                                      minGSSize = minGSSize,
                                      maxGSSize = maxGSSize,
                                      pvalueCutoff = pvalueCutoff,
                                      verbose = verbose,
                                      pAdjustMethod = pAdjustMethod,
                                      TERM2GENE=term2gene)
  return(gsea_result)
}

get_cp_results = function(object, showCategory) {
  showCategory = min(nrow(object@result), showCategory)
  df = object@result[1:showCategory,]
  if (!is.null(df$GeneRatio)) {
    df$GeneRatio = sapply(strsplit(as.character(df$GeneRatio), "/"), function(x) as.numeric(x[1]) / as.numeric(x[2]))
  }
  if (!is.null(df$BgRatio)) {
    df$BgRatio = sapply(strsplit(as.character(df$BgRatio), "/"), function(x) as.numeric(x[1]) / as.numeric(x[2]))
  }
  return(df)
}

#------------------------------------------------------- Saturation index ------
# Here are some functions to calculate the saturation index in several different
# ways.

# use palmitate, stearate and oleate tails for the calculation of the SI index per lipid class
satindex_calc_ratio <- function(data_table = NULL,
                                feature_table = NULL,
                                sample_meta = NULL) {
  ## Initialize some stuff
  # the feature table doesn't contain a column lipids fix here
  feature_table$lipid <- rownames(feature_table)
  # get the unique lipid classes
  lipid_classes <- unique(feature_table$lipid_class)

  # define list with which FA tails to search for
  lipids <- list(
    palmitate = c("carbon" = 16,
                  "db" = 0),
    stearate = c("carbon" = 18,
                 "db" = 0),
    oleate = c("carbon" = 18,
               "db" = 1)
  )
  # get the names of the FA tails
  lipid_names <- names(lipids)

  # initialize result list
  tot_lipids <- vector(mode = "list",
                       length = length(lipid_classes))
  names(tot_lipids) <- lipid_classes
  tot_lipids <- lapply(tot_lipids, function(x) {
    setNames(vector(mode = "list",
                    length = length(lipid_names) + 1),
             c(lipid_names, "SI"))
  })

  for(a in lipid_classes) {
    for(b in lipid_names) {
      # select the lipids with the correct FA tails
      lipid_all <- feature_table$lipid[feature_table$lipid_class == a &
                                         ((feature_table$carbons_1 == lipids[[b]]["carbon"] & feature_table$unsat_1 == lipids[[b]]["db"]) |
                                            (feature_table$carbons_2 == lipids[[b]]["carbon"] & feature_table$unsat_2 == lipids[[b]]["db"]))]
      lipid_dbl <- feature_table$lipid[feature_table$lipid_class == a &
                                         (feature_table$carbons_1 == lipids[[b]]["carbon"] & feature_table$unsat_1 == lipids[[b]]["db"] &
                                            feature_table$carbons_2 == lipids[[b]]["carbon"] & feature_table$unsat_2 == lipids[[b]]["db"])]

      # get all data
      lipid_data <- data_table[, colnames(data_table) %in% feature_table$lipid[feature_table$lipid_class == a], drop = FALSE]
      # If doesn't contain any of the FA tail multiply by zero
      lipid_data[, !(colnames(lipid_data) %in% lipid_all)] <- lipid_data[, !(colnames(lipid_data) %in% lipid_all)] * 0
      # if contains 2x FA tail multiply by 2
      lipid_data[, colnames(lipid_data) %in% lipid_dbl] <- lipid_data[, colnames(lipid_data) %in% lipid_dbl] * 2

      # get the total
      tot_lipids[[a]][[b]] <- rowSums(lipid_data, na.rm = TRUE)
    }
    tot_lipids[[a]][["SI"]] <- (tot_lipids[[a]][["palmitate"]] + tot_lipids[[a]][["stearate"]]) / tot_lipids[[a]][["oleate"]]
  }

  # make data.frame
  tot_lipids <- do.call("cbind.data.frame", lapply(tot_lipids, function(x) {
    x["SI"]
  }))
  names(tot_lipids) <- lipid_classes

  return(tot_lipids)
}

# use all FA tails for the calculation of the SI index per lipid class
satindex_calc_all <- function(data_table = NULL,
                              feature_table = NULL,
                              sample_meta = NULL) {
  # initialize some stuff
  # the feature table doesn't contain a column lipids fix here
  feature_table$lipid <- rownames(feature_table)
  # get the unique lipid classes
  lipid_classes <- unique(feature_table$lipid_class)

  tot_lipids <- vector(mode = "list",
                       length = length(lipid_classes))
  names(tot_lipids) <- lipid_classes
  tot_lipids <- lapply(tot_lipids, function(x) {
    setNames(vector(mode = "list",
                    length = 3),
             c("tot_sat", "tot_unsat", "SI"))
  })

  for(a in lipid_classes) {
    # lipids with only one FA chain
    if(all(feature_table$carbons_2[feature_table$lipid_class == a] == 0) | a == "TG") {
      if(a == "TG") {
        sat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                           feature_table$unsat_2 == 0]
        unsat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                             feature_table$unsat_2 != 0]
        sat_lipid_dbl <- NULL
        unsat_lipid_dbl <- NULL
      } else {
        sat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                           feature_table$unsat_1 == 0]
        unsat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                             feature_table$unsat_1 != 0]
        sat_lipid_dbl <- NULL
        unsat_lipid_dbl <- NULL
      }
    } else {
      # lipids with 2 FA chains
      sat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                         (feature_table$unsat_1 == 0 |
                                            feature_table$unsat_2 == 0)]
      unsat_lipid <- feature_table$lipid[feature_table$lipid_class == a &
                                           (feature_table$unsat_1 != 0 |
                                              feature_table$unsat_2 != 0)]
      sat_lipid_dbl <- feature_table$lipid[feature_table$lipid_class == a &
                                             feature_table$unsat_1 == 0 &
                                             feature_table$unsat_2 == 0]
      unsat_lipid_dbl <- feature_table$lipid[feature_table$lipid_class == a &
                                               feature_table$unsat_1 != 0 &
                                               feature_table$unsat_2 != 0]
    }

    # get data per lipid class
    lipid_data <- data_table[, colnames(data_table) %in% feature_table$lipid[feature_table$lipid_class == a], drop = FALSE]
    # saturated
    lipid_data_sat <- lipid_data[, colnames(lipid_data) %in% c(sat_lipid, sat_lipid_dbl), drop = FALSE]
    lipid_data_sat[, colnames(lipid_data_sat) %in% sat_lipid_dbl] <- lipid_data_sat[, colnames(lipid_data_sat) %in% sat_lipid_dbl] * 2
    # # if contains 2x FA tail multiply by 2
    lipid_data_unsat <- lipid_data[, colnames(lipid_data) %in% c(unsat_lipid, unsat_lipid_dbl), drop = FALSE]
    lipid_data_unsat[, colnames(lipid_data_unsat) %in% unsat_lipid_dbl] <- lipid_data_unsat[, colnames(lipid_data_unsat) %in% unsat_lipid_dbl] * 2

    # calculate the SI index
    tot_lipids[[a]][["tot_sat"]] <- rowSums(lipid_data_sat, na.rm = TRUE)
    tot_lipids[[a]][["tot_unsat"]] <- rowSums(lipid_data_unsat, na.rm = TRUE)
    tot_lipids[[a]][["SI"]] <- tot_lipids[[a]][["tot_sat"]] / tot_lipids[[a]][["tot_unsat"]]

  }

  # make data.frame
  tot_lipids <- do.call("cbind.data.frame", lapply(tot_lipids, function(x) {
    x["SI"]
  }))
  names(tot_lipids) <- lipid_classes

  return(tot_lipids)
}

# use all FA tails for the calculation of the overall SI index
satindex_calc_overall <- function(data_table = NULL,
                                  feature_table = NULL,
                                  sample_meta = NULL) {
  # the feature table doesn't contain a column lipids fix here
  feature_table$lipid <- rownames(feature_table)

  # leave TG's and PA's out
  sat_lipid <- feature_table$lipid[!(feature_table$lipid_class %in% c("TG", "PA")) &
                                     (feature_table$unsat_1 == 0 |
                                        feature_table$unsat_2 == 0)]
  unsat_lipid <- feature_table$lipid[!(feature_table$lipid_class %in% c("TG", "PA")) &
                                       (feature_table$unsat_1 != 0 |
                                          feature_table$unsat_2 != 0)]
  sat_lipid_dbl <- feature_table$lipid[!(feature_table$lipid_class %in% c("TG", "PA")) &
                                         feature_table$unsat_1 == 0 &
                                         feature_table$unsat_2 == 0]
  unsat_lipid_dbl <- feature_table$lipid[!(feature_table$lipid_class %in% c("TG", "PA")) &
                                           feature_table$unsat_1 != 0 &
                                           feature_table$unsat_2 != 0]
  # with TG's
  sat_lipid_TG <- feature_table$lipid[feature_table$lipid_class == "TG" &
                                        feature_table$unsat_2 == 0]
  unsat_lipid_TG <- feature_table$lipid[feature_table$lipid_class == "TG" &
                                          feature_table$unsat_2 != 0]

  # with PA's
  sat_lipid_PA <- feature_table$lipid[feature_table$lipid_class == "PA" &
                                        feature_table$unsat_2 == 0]
  unsat_lipid_PA <- feature_table$lipid[feature_table$lipid_class == "PA" &
                                          feature_table$unsat_2 != 0]
  # saturated
  lipid_data_sat <- data_table[, colnames(data_table) %in% Reduce("union", list(sat_lipid, sat_lipid_dbl, sat_lipid_TG, sat_lipid_PA)), drop = FALSE]
  if(length(sat_lipid_dbl) > 0 ) {
    lipid_data_sat[, colnames(lipid_data_sat) %in% sat_lipid_dbl] <- lipid_data_sat[, colnames(lipid_data_sat) %in% sat_lipid_dbl, drop = FALSE] * 2
  }

  # unsaturated
  lipid_data_unsat <- data_table[, colnames(data_table) %in% Reduce("union", list(unsat_lipid, unsat_lipid_dbl, unsat_lipid_TG, unsat_lipid_PA)), drop = FALSE]
  if(length(unsat_lipid_dbl) > 0 ) {
    lipid_data_unsat[, colnames(lipid_data_unsat) %in% unsat_lipid_dbl] <- lipid_data_unsat[, colnames(lipid_data_unsat) %in% unsat_lipid_dbl, drop = FALSE] * 2
  }

  SI_index_overall <- rowSums(lipid_data_sat, na.rm = TRUE) / rowSums(lipid_data_unsat, na.rm = TRUE)

  tot_lipids <- data.frame(SI = SI_index_overall)

  return(tot_lipids)
}

satindex_calc_db <- function(data_table = NULL,
                             feature_table = NULL,
                             sample_meta = NULL,
                             group_col = NULL,
                             group_1 = NULL,
                             group_2 = NULL,
                             selected_lipid_class = NULL) {
  # get only the selected lipid class
  class_data <- feature_table[feature_table$lipid_class == selected_lipid_class, ]

  # which saturations are there
  saturation <- sort(unique(class_data$unsat_sum))

  groups <- c(group_1, group_2)

  # initialize some things
  db_data <- vector(mode = "list",
                    length = length(saturation))
  names(db_data) <- as.character(saturation)

  db_data <- lapply(db_data, function(x) {
    setNames(data.frame(group1 = rep(NA, 1),
                        group2 = rep(NA, 1),
                        doubleBond = rep(NA, 1),
                        foldChange = rep(NA, 1)),
             c(group_1, group_2, "doubleBond", "foldChange"))
  })

  # do the calculations
  for(a in saturation) {
    for(b in groups) {
      # get the correct lipids
      sel_lipids <- rownames(class_data)[class_data$unsat_sum == a]
      # get the correct samples
      sel_samples <- rownames(sample_meta)[sample_meta[, group_col] == b]
      # calculate the average over the samples after summing the lipid species
      db_data[[as.character(a)]][[b]] <- mean(rowSums(data_table[rownames(data_table) %in% sel_samples,
                                                                 colnames(data_table) %in% sel_lipids,
                                                                 drop = FALSE],
                                                      na.rm = TRUE),
                                              na.rm = TRUE)
    }
    db_data[[as.character(a)]][["foldChange"]] <- db_data[[as.character(a)]][[1]] / db_data[[as.character(a)]][[2]]
    db_data[[as.character(a)]][["doubleBond"]] <- a
  }

  # make one nice data.frame
  db_data <- do.call(rbind.data.frame, db_data)
  db_data$doubleBond <- as.factor(db_data$doubleBond)

  return(db_data)
}
