#--------------------------------------------------- Global table functions ----
find_delim = function(path) {
  probe = paste(readLines(con = path, n = 10), collapse = "")
  sep = c("\t" = lengths(regmatches(probe, gregexpr("\t", probe))),
          "," = lengths(regmatches(probe, gregexpr(",", probe))),
          ";" = lengths(regmatches(probe, gregexpr(";", probe))))
  return(names(which.max(sep)))
}

get_idx_by_pattern = function(table, col, pattern, row_names = T) {
  if (row_names) {
    out_idx = rownames(table)[grep(pattern = pattern, x = table[,col], ignore.case = TRUE)]
  }else{
    out_idx = grep(pattern = pattern, x = table[,col], ignore.case = TRUE)
  }
  return(out_idx)
}

set_index_col = function(data_table, idx) {
  rownames(data_table) = data_table[,idx]
  data_table[, idx] = NULL
  return(data_table)
}

get_rownames_from_idx = function(idx, id_col, data_table) {
  return(data_table[idx, id_col])
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

keep_rows = function(data_table, rows) {
  return(data_table[(row.names(data_table) %in% rows),])
}

drop_rows = function(data_table, rows) {
  return(data_table[!(row.names(data_table) %in% rows),])
}

convert_long = function(long_table, id_col, feature_col, values_col, meta_cols, remove_conflicts = TRUE) {
  print_time("Table convert: Converting table")
  out_table = long_table[,c(id_col, feature_col, values_col, meta_cols)]
  out_table = out_table[!is.na(out_table[,feature_col]),]
  out_table = reshape(out_table, v.names = values_col, timevar = feature_col, idvar = id_col, direction = "wide")
  colnames(out_table) = stringr::str_replace_all(colnames(out_table), paste0(values_col, "."), "")
  meta_table = out_table[,c(id_col, meta_cols)]
  data_table = out_table[,-which(colnames(out_table) %in% meta_cols)]
  data_table = data_table[,-c(which(colnames(data_table) == "NaN"))]
  if (remove_conflicts) {
    new_colnames = new_colnames = stringr::str_split_i(colnames(data_table), ";", 1)
    colnames(data_table) = new_colnames
  }
  return(list("meta_table" = meta_table,
              "data_table" = data_table))
}

convert_long_file = function(file, id_col, feature_col, values_col, meta_cols) {
  print_time("Table convert: Importing table")
  sep = find_delim(file)
  long_table = read.table(file,
                          quote = "",
                          sep = sep,
                          header = TRUE)
  out_tables =  convert_long(long_table,
                             id_col,
                             feature_col,
                             values_col,
                             meta_cols)
  print_time("Table convert: Exporting tables")
  write.table(out_tables$meta_table,
              file = stringr::str_replace(file, "\\..*","_meta.tsv"),
              sep = "\t",
              row.names = FALSE)
  write.table(out_tables$data_table,
              file = stringr::str_replace(file, "\\..*","_data.tsv"),
              sep = "\t",
              na = "",
              row.names = FALSE)
}

na_imputation = function(data_table, mask, imputation) {
  data_table[mask] = imputation
  return(data_table)
}

batch_na_imputation = function(data_table, mask, batch_list, imputation_factor) {
  
  # Return directly if imputation is NA
  if (is.na(imputation_factor)) {
    return(data_table)
  }
  
  # No batch requirements if imputation is fixed
  if (imputation_factor == 0) {
    data_table[mask] = 0.0
    return(data_table)
  }
  
  for (b in unique(batch_list)) {
    batch_mask = mask
    batch_mask[batch_list != b,] = FALSE
    
    
    imputation = data_table[batch_list == b,2:ncol(data_table)]
    imputation_mask = batch_mask[batch_list == b,2:ncol(batch_mask)]
    imputation = imputation[!imputation_mask]
    imputation = min(imputation)
    imputation = imputation*imputation_factor
    data_table = na_imputation(data_table = data_table,
                               mask = batch_mask,
                               imputation = imputation)
  }
  return(data_table)
}

meta_normalise = function(data_table, norm_list) {
  for (i in 1:nrow(data_table)){
    data_table[i,] = data_table[i,]/norm_list[i]
  }
  return(data_table)
}

#----------------------------------------------------------- Table switches ----
table_switch = function(selection, r6){
  switch(EXPR = selection,
         "Filtered data table" = r6$tables$data_filtered,
         "Class normalised data table" = r6$tables$data_class_norm,
         "Total normalised data table" = r6$tables$data_total_norm,
         "Raw class table" = r6$tables$data_class_table_raw,
         "Total normalised class table" = r6$tables$data_class_table_total_norm,
         "Z-scored data table" = r6$tables$data_z_scored,
         "Z-scored class normalised data table" = r6$tables$data_class_norm_z_scored,
         "Z-scored total normalised data table" = r6$tables$data_total_norm_z_scored,
         "Z-scored total normalised class table" = r6$tables$data_class_table_z_scored,
         "Raw feature table" = r6$tables$feat_raw,
         "Filtered feature table" = r6$tables$feat_filtered,
         "Group summary species" = r6$tables$group_species,
         "Group summary classes" = r6$tables$group_classes,
         
         # Proteomics, Transcriptomics specific
         "GO components raw" = r6$tables$go_components_raw,
         "GO functions raw" = r6$tables$go_functions_raw,
         "GO processes raw" = r6$tables$go_processes_raw,
         "GO components filtered" = r6$tables$go_components_filtered,
         "GO functions filtered" = r6$tables$go_functions_filtered,
         "GO processes filtered" = r6$tables$go_processes_filtered
  )
}

#-------------------------------------------------- P-val adjustment switch ----


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

#----------------------------------------------------------- Summary tables ----

get_group_median_table = function(data_table,
                                  meta_table,
                                  col_group) {
  unique_groups = unique(meta_table[,col_group])
  out_table = as.data.frame(matrix(data = 0.0,
                                   nrow = length(unique_groups),
                                   ncol = ncol(data_table)))
  colnames(out_table) = colnames(data_table)
  rownames(out_table) = unique_groups
  for (group in unique_groups) {
    idx = rownames(meta_table)[which(meta_table[,col_group] == group)]
    group_table = data_table[idx,]
    group_values = apply(group_table,2,median, na.rm = TRUE)
    group_values[is.na(group_values)] = 0.0
    out_table[group,] = group_values
  }
  return(out_table)
}

#------------------------------------------------------ Filtering functions ----
get_col_means = function(data_table) {
  means = colMeans(data_table, na.rm = TRUE)
  means[is.na(means)] = 0
  return(means)
}

blank_filter = function(data_table, blank_table, blank_multiplier, sample_threshold, saved_cols = FALSE) {
  # Find features / columns below threshold
  blank_means = get_col_means(data_table = blank_table)
  del_cols = c()
  data_table[is.na(data_table)] = 0
  total_samples = nrow(data_table)
  for (col in colnames(data_table)){
    threshold = blank_multiplier * blank_means[col]
    above_threshold = sum(data_table[, col] >= threshold)
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
                             meta_table_raw,
                             meta_table_filtered,
                             idx_blanks = r6$indices$idx_blanks,
                             idx_samples = r6$indices$idx_samples,
                             col_id_meta = r6$texts$col_id_meta,
                             col_group,
                             col_batch,
                             blank_multiplier,
                             sample_threshold,
                             group_threshold) {
  
  # Blank filtering
  data_table[is.na(data_table)] = 0.0
  del_cols = c()
  all_batches = unique(meta_table_raw[, col_batch])
  for (b in all_batches) {
    batch_idx = which(meta_table_raw[, col_batch] == b)
    batch_blanks = base::intersect(batch_idx, idx_blanks)
    batch_samples = base::intersect(batch_idx, idx_samples)
    
    # Get rownames
    batch_blanks = meta_table_raw[batch_blanks, col_id_meta]
    batch_samples = meta_table_raw[batch_samples, col_id_meta]
    batch_samples = base::intersect(rownames(data_table), batch_samples)
    
    del_cols = c(del_cols, blank_filter(data_table = data_table[batch_samples,],
                                        blank_table = blank_table[batch_blanks,],
                                        blank_multiplier = blank_multiplier,
                                        sample_threshold = sample_threshold))
  }
  
  del_cols = unique(del_cols)
  del_cols = sort(del_cols)
  
  if (is.null(del_cols)) {
    print(del_cols)
    return(del_cols)
  }

  # Group filtering
  saved_cols = c()
  for (g in unique(meta_table_filtered[,col_group])) {
    group_idx = which(meta_table_raw[, col_group] == g)
    above_threshold = rep(0, length(del_cols))
    names(above_threshold) = del_cols
    for (b in unique(meta_table_raw[group_idx, col_batch])) {
      
      batch_idx = which(meta_table_raw[, col_batch] == b)
      batch_blanks = base::intersect(batch_idx, idx_blanks)
      batch_samples = base::intersect(batch_idx, group_idx)
      
      # Get rownames
      batch_blanks = meta_table_raw[batch_blanks, col_id_meta]
      batch_samples = meta_table_raw[batch_samples, col_id_meta]
      batch_samples = base::intersect(rownames(data_table), batch_samples)
      
      # get batch blank means
      blank_means = get_col_means(data_table = blank_table[batch_blanks,])
      threshold = blank_multiplier * blank_means
      
      # Find features / columns below threshold
      for (col in del_cols) {
        above_threshold[col] = above_threshold[col] + sum(data_table[batch_samples,col] >= threshold[col])
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

#-------------------------------------------------- Normalisation functions ----

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

normalise_lipid_class = function(lips_table) {
  # Get classes and unique classes for the lipid features
  classes = get_lipid_classes(feature_list = as.character(colnames(lips_table)), uniques = FALSE)
  classes_unique = get_lipid_classes(feature_list = as.character(colnames(lips_table)), uniques = TRUE)
  lips_table[is.na(lips_table)] = 0
  
  # For each unique lipid class...
  for (lip_class in classes_unique){
    
    # Get columns from that class...
    cols = which(classes == lip_class)
    if (length(cols) > 1) {
      class_row_sums = rowSums(lips_table[, cols])
    } else {
      class_row_sums = lips_table[, cols]
    }
    class_row_sums[class_row_sums == 0] = 1
    lips_table[, cols] = lips_table[, cols] / class_row_sums
  }
  return(lips_table)
}

z_score_normalisation = function(data_table, impute) {
  # Impute (or not) and scale (z-score) the data
  if (is.na(impute)) {
    data_table = scale(data_table)
  } else {
    data_table[is.na(data_table)] = impute
    data_table = scale(data_table)
  }
  return(data_table)
}



#---------------------------------------------------- Class table functions ----
get_lipid_class_table = function(table){
  
  # Replace NA by 0s
  table[is.na(table)] = 0
  
  # Get unique lipid classes
  classes = get_lipid_classes(feature_list = colnames(table), uniques = TRUE)
  
  # Get a column vector to find easily which columns belong to each lipid group
  col_vector = get_lipid_classes(feature_list = colnames(table), uniques = FALSE)
  
  # Fill the table
  out_table = sapply(X = classes,
                     FUN = function(x) {
                       col_list = which(col_vector == x)
                       if (length(col_list) > 1) {
                         rowSums(table[,col_list])
                       } else {
                         table[,col_list]
                       }
                     }
  )
  
  return(out_table)
}

#----------------------------------------------- Class comparison functions ----
get_subplot_dim = function(class_list){
  return(ceiling(sqrt(length(class_list))))}

get_subplot_titles = function(class_list){
  dim = get_subplot_dim(class_list)
  step = 1/dim
  x = step/2
  y = 0.97 - step
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
    x = x + step
    if (x >= 1) {
      x = step/2
      y = y - step}}
  annotations[[i]] = list(x = -0.08, y = 0.5, text = "Concentration",
                          font = list(size = 10),
                          textangle = 270, showarrow = FALSE, xref='paper',
                          yref='paper')
  return(annotations)}



#----------------------------------------- Lipid upload class preview plots ----

preview_class_plot = function(data_table, del_cols, feat_raw){
  
  total_values = table(feat_raw$lipid_class)
  filtered_values_1 = rep(0,each=length(total_values))
  names(filtered_values_1) = names(total_values)
  
  if (!is.null(del_cols)){
    
    filtered_values_2 = table(get_lipid_classes(feature_list = colnames(data_table)[!(colnames(data_table) %in% del_cols)],
                                                uniques = F))
  }else{
    filtered_values_2 = table(get_lipid_classes(feature_list = colnames(data_table),
                                                uniques = F))
  }
  
  for (n in names(filtered_values_2)){
    filtered_values_1[n] = filtered_values_2[n]
  }
  
  lip_class = c(names(total_values), names(total_values))
  d_type = c(rep("kept", length(total_values)), rep("lost", length(total_values)))
  
  lip_val = c(rep(100, length(total_values)) - round(100*(filtered_values_1/total_values),1), round(100*(filtered_values_1/total_values),1))
  class_df = data.frame(lip_class, d_type, lip_val)
  class_df[, "lip_val"] = round(class_df[, "lip_val"],1)
  
  lip_val_abs = c(total_values - filtered_values_1, filtered_values_1)
  class_df_abs = data.frame(lip_class, d_type, lip_val_abs)
  
  
  plot_1 = ggplot(class_df_abs ,
                  aes(
                    fill=d_type ,
                    y=lip_val_abs,
                    x=lip_class,
                    label = lip_val_abs)) +
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
    scale_y_reverse(limits = c(max(total_values), 0))
  
  plot_2 = ggplot(class_df,
                  aes(
                    fill=d_type ,
                    y=lip_val,
                    x=lip_class,
                    label = lip_val)) +
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
  
  return(grid.arrange(plot_1, plot_2, ncol=2))
}




#--------------------------------------------------------------- Statistics ----
get_pca_data = function(data_table){
  
  if (sum(is.na(data_table)) > 0) {
    complete_obs = F
  }else{
    complete_obs = T
  }
  
  pca_data = pcaMethods::pca(object = data_table,
                             nPcs = 2,
                             scale = "none",
                             cv = "q2",
                             completeObs = complete_obs)
  
  return(pca_data)
}

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


get_fc_and_pval = function(data_table, idx_group_1, idx_group_2, used_function, test){
  
  if (used_function == "median") {
    av_function = function(x) {return(median(x, na.rm = T))}
  } else {
    av_function = function(x) {return(mean(x, na.rm = T))}
  }
  
  if (test == "Wilcoxon") {
    test_function = stats::wilcox.test
    print_time("Wilcoxon selected")
  } else if (test == "T-test") {
    test_function = stats::t.test
    print_time("T-test selected")
  }
  
  # Collect fold change and p-values
  fold_change = c()
  p_value = c()
  
  for (col in colnames(data_table)) {
    
    # If both groups contain data
    if (length(na.exclude(data_table[idx_group_1, col])) > 0 & length(na.exclude(data_table[idx_group_2, col])) > 0) {
      
      # If at least one of the groups contains only one value
      if ((length(na.exclude(data_table[idx_group_1, col])) == 1) | (length(na.exclude(data_table[idx_group_2, col])) == 1)) {
        fold_change = c(fold_change, av_function(data_table[idx_group_2, col]) / av_function(data_table[idx_group_1, col]))
        p_value = c(p_value, NA)
      } else {
        
        # If there is actual comparable data
        fold_change = c(fold_change, av_function(data_table[idx_group_2, col]) / av_function(data_table[idx_group_1, col]))
        p_value = c(p_value, test_function(data_table[idx_group_1, col], data_table[idx_group_2, col])$p.value)
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


apply_discriminant_analysis = function(data_table, group_list, nlambda = 100, alpha = 0.8) {
  
  count = table(group_list)
  if (any(count < 3)) {
    dead_groups = names(count)[count < 3]
    print_time(paste0("Warning: ", length(dead_groups), " groups with fewer than 3 samples, dropped from analysis."))
    data_table = data_table[!(group_list %in% dead_groups),]
    group_list = group_list[!(group_list %in% dead_groups)]
  }
  
  if (length(unique(group_list) > 2)) {
    family = "multinomial"
  } else {
    family = "binomial"
  }
  
  coef = glmnet::cv.glmnet(data_table,
                           group_list,
                           nlambda = nlambda,
                           alpha = alpha,
                           family = family,
                           type.multinomial = "grouped")
  
  coef = stats::coef(coef, s = "lambda.min")
  keep_cols = as.matrix(coef[[1]])
  
  keep_cols = rownames(keep_cols)[which(keep_cols != 0)]
  keep_cols = keep_cols[2:length(keep_cols)]
  data_table = data_table[,keep_cols]
  return(data_table)
}




#------------------------------------------------------- Plotting functions ----

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


#-------------------------------------------------- Feature table functions ----

feature_switch = function(feature_col){
  out_list = c()
  for (col in feature_col) {
    out_list = c(out_list, switch(EXPR = col,
                                  "Class" = "lipid_class",
                                  "Carbon count" = "carbons_1",
                                  "Unsaturation count" = "unsat_1"
    )
    )
  }
  return(out_list)
}


get_feature_metadata = function(data_table) {
  # Initialise table
  feature_table = data.frame(row.names = sort(colnames(data_table)))
  
  # Add lipid classes
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
        c_count_2 = c(c_count_2, i[2])
        s_count_1 = c(s_count_1, i[3])
        s_count_2 = c(s_count_2, i[3])
      }
    }
  }
  
  feature_table$carbons_1 = as.numeric(c_count_1)
  feature_table$carbons_2 = as.numeric(c_count_2)
  feature_table$unsat_1 = as.numeric(s_count_1)
  feature_table$unsat_2 = as.numeric(s_count_2)
  return(feature_table)
}

#------------------------------------------------------------- UI functions ----
soda_get_col_ui = function(label = "Column selection", desc = "Description"){
  shiny::tagList(
    shiny::strong(label),
    shiny::br(),
    shiny::helpText(desc)
  )
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

print_time = function(in_print) {
  print(paste0(get_time(), " - ", in_print))
}

#--------------------------------------------------------- String functions ----

detect_null_str = function(x) {
  if (is.null(x)) {
    return("")
  } else {
    return(x)
  }
}


#----------------------------------------------------- proteomics functions ----


cleanup_prot_list = function(prot_list) {
  out_list = c()
  for (prot in prot_list) {
    out_list = c(out_list, base::strsplit(prot, ";")[[1]])
  }
  out_list = base::unique(out_list)
  out_list = base::sort(out_list)
  return(out_list)
}

#prot_list = c("A0A0B4J2F0", "A0AV96", "A0AVT1")
prots_get_feature_table = function(prot_list, main_go) {
  
  feature_table = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(feature_table) = c("accession", "id", "proteinExistence", "protein", "gene", "go_components", "go_functions", "go_processes")
  
  while (length(prot_list) > 0) {
    print_time(paste0("Uniprot query: ", length(prot_list), " proteins remaining."))
    end = min(length(prot_list), 100)
    prot_sublist = prot_list[1:end]
    prot_list = prot_list[-c(1:end)]
    
    prot_query = paste(prot_sublist, collapse = "%2C")
    request_url = paste0("https://www.ebi.ac.uk/proteins/api/proteins?offset=0&size=100&accession=", prot_query)
    
    
    r = httr::GET(request_url, accept("application/json"))
    
    fail = base::try(httr::stop_for_status(r))
    if (base::inherits(fail, "try-error")) {
      
      for (prot in prot_sublist) {
        
        request_url = paste0("https://www.ebi.ac.uk/proteins/api/proteins?offset=0&size=100&accession=", prot)
        r = httr::GET(request_url, accept("application/json"))
        
        local_fail = base::try(httr::stop_for_status(r))
        
        if (base::inherits(local_fail, "try-error")) {
          next
        } else {
          uniprot_data = rjson::toJSON(content(r))
          uniprot_data = rjson::fromJSON(uniprot_data)
          
          for (i in 1:length(uniprot_data)) {
            
            go_values = c()
            for (j in 1:length(uniprot_data[[i]]$dbReferences)) {
              if (uniprot_data[[i]]$dbReferences[[j]]$type == "GO") {
                go_values = c(go_values, uniprot_data[[i]]$dbReferences[[j]]$properties$term)
              }
            }
            
            go_components = go_values[stringr::str_detect(go_values, pattern = "C:")]
            go_functions = go_values[stringr::str_detect(go_values, pattern = "F:")]
            go_processes = go_values[stringr::str_detect(go_values, pattern = "P:")]
            
            go_components = stringr::str_replace_all(go_components, "C:", "")
            go_functions = stringr::str_replace_all(go_functions, "F:", "")
            go_processes = stringr::str_replace_all(go_processes, "P:", "")
            
            go_components = paste(go_components, collapse = "|")
            go_functions = paste(go_functions, collapse = "|")
            go_processes = paste(go_processes, collapse = "|")
            
            new_row = c(uniprot_data[[i]]$accession,
                        uniprot_data[[i]]$id,
                        uniprot_data[[i]]$proteinExistence,
                        uniprot_data[[i]]$protein$recommendedName$fullName$value,
                        detect_null_str(uniprot_data[[i]]$gene[[1]]$name$value),
                        go_components,
                        go_functions,
                        go_processes
            )
            feature_table[nrow(feature_table) + 1,] = new_row
          }
          
        }
        
      }
    } else {
      
      uniprot_data = rjson::toJSON(content(r))
      uniprot_data = rjson::fromJSON(uniprot_data)
      
      
      for (i in 1:length(uniprot_data)) {
        
        go_values = c()
        for (j in 1:length(uniprot_data[[i]]$dbReferences)) {
          if (uniprot_data[[i]]$dbReferences[[j]]$type == "GO") {
            go_values = c(go_values, uniprot_data[[i]]$dbReferences[[j]]$properties$term)
          }
        }
        
        go_components = go_values[stringr::str_detect(go_values, pattern = "C:")]
        go_functions = go_values[stringr::str_detect(go_values, pattern = "F:")]
        go_processes = go_values[stringr::str_detect(go_values, pattern = "P:")]
        
        go_components = stringr::str_replace_all(go_components, "C:", "")
        go_functions = stringr::str_replace_all(go_functions, "F:", "")
        go_processes = stringr::str_replace_all(go_processes, "P:", "")
        
        go_components = paste(go_components, collapse = "|")
        go_functions = paste(go_functions, collapse = "|")
        go_processes = paste(go_processes, collapse = "|")
        
        new_row = c(uniprot_data[[i]]$accession,
                    uniprot_data[[i]]$id,
                    uniprot_data[[i]]$proteinExistence,
                    uniprot_data[[i]]$protein$recommendedName$fullName$value,
                    detect_null_str(uniprot_data[[i]]$gene[[1]]$name$value),
                    go_components,
                    go_functions,
                    go_processes
        )
        feature_table[nrow(feature_table) + 1,] = new_row
      }
    }
  }
  
  all_go_components = unlist(stringr::str_split(feature_table[,"go_components"], "\\|"), recursive = TRUE, use.names = FALSE)
  all_go_functions = unlist(stringr::str_split(feature_table[,"go_functions"], "\\|"), recursive = TRUE, use.names = FALSE)
  all_go_processes = unlist(stringr::str_split(feature_table[,"go_processes"], "\\|"), recursive = TRUE, use.names = FALSE)
  
  all_go_components = sort(unique(all_go_components))
  all_go_functions = sort(unique(all_go_functions))
  all_go_processes = sort(unique(all_go_processes))
  
  if (any(all_go_components == "")){
    all_go_components = all_go_components[-which(all_go_components == "")]
  }
  
  if (any(all_go_functions == "")){
    all_go_functions = all_go_functions[-which(all_go_functions == "")]
  }
  
  if (any(all_go_processes == "")){
    all_go_processes = all_go_processes[-which(all_go_processes == "")]
  }
  
  go_components_table = data.frame(matrix(data = FALSE, ncol = length(all_go_components), nrow = nrow(feature_table)))
  colnames(go_components_table) = all_go_components
  rownames(go_components_table) = feature_table$accession
  
  go_functions_table = data.frame(matrix(data = FALSE, ncol = length(all_go_functions), nrow = nrow(feature_table)))
  colnames(go_functions_table) = all_go_functions
  rownames(go_functions_table) = feature_table$accession
  
  go_processes_table = data.frame(matrix(data = FALSE, ncol = length(all_go_processes), nrow = nrow(feature_table)))
  colnames(go_processes_table) = all_go_processes
  rownames(go_processes_table) = feature_table$accession
  
  print_time("Filling Gene Ontology tables")
  for (i in 1:nrow(feature_table)) {
    go_components_i = stringr::str_split(feature_table[i, "go_components"], "\\|")[[1]]
    go_functions_i = stringr::str_split(feature_table[i, "go_functions"], "\\|")[[1]]
    go_processes_i = stringr::str_split(feature_table[i, "go_processes"], "\\|")[[1]]
    
    if (any(go_components_i == "")){
      go_components_i = go_components_i[-which(go_components_i == "")]
    }
    if (any(go_functions_i == "")){
      go_functions_i = go_functions_i[-which(go_functions_i == "")]
    }
    if (any(go_processes_i == "")){
      go_processes_i = go_processes_i[-which(all_go_processes == "")]
    }
    
    go_components_table[i, go_components_i] = TRUE
    go_functions_table[i, go_functions_i] = TRUE
    go_processes_table[i, go_processes_i] = TRUE
    
  }
  
  if (main_go) {
    
    main_components = c("nuclear chromosome", "extracellular region", "extracellular space", "cell wall",
                        "nucleus", "nuclear envelope", "nucleoplasm", "chromosome", "nucleolus",
                        "mitochondrion", "lysosome", "endosome", "vacuole", "peroxisome", "endoplasmic reticulum",
                        "golgi apparatus", "lipid droplet", "microtubule organizing center", "cytosol",
                        "ribosome", "cytoskeleton", "plasma membrane", "cilium", "plastid", "thylakoid",
                        "external encapsulating structure", "extracellular matrix", "cytoplasmic vesicle",
                        "organelle")
    
    main_functions = c("virus receptor activity", "DNA binding", "RNA binding", "cytoskeletal motor activity",
                       "catalytic activity", "GTPase activity", "structural molecule activity",
                       "transporter activity", "cytoskeletal protein binding", "lipid binding",
                       "cyclase activity", "antioxidant activity", "oxidoreductase activity",
                       "transferase activity", "hydrolase activity", "lyase activity", "isomerase activity",
                       "ligase activity", "protein tag", "cargo receptor activity", "histone binding",
                       "protein folding chaperone", "translation regulator activity", "nutrient reservoir activity",
                       "receptor ligand activity", "molecular transducer activity", "molecular adaptor activity",
                       "toxin activity", "cell adhesion mediator activity", "molecular function regulator activity",
                       "virus coreceptor activity", "catalytic activity, acting on a protein",
                       "catalytic activity, acting on dna", "catalytic activity, acting on rna",
                       "molecular carrier activity", "transcription regulator activity", "general transcription initiation factor activity",
                       "small molecule sensor activity", "molecular sequestering activity", "atp-dependent activity")
    
    main_processes = c("mitotic cell cycle", "cytokinesis", "cytoplasmic translation", "immune system process",
                       "muscle system process", "circulatory system process", "renal system process",
                       "respiratory system process", "carbohydrate metabolic process", "generation of precursor metabolites and energy",
                       "dna replication", "dna repair", "dna recombination", "chromatin organization",
                       "dna-templated transcription", "regulation of dna-templated transcription",
                       "trna metabolic process", "protein folding", "protein glycosylation", "amino acid metabolic process",
                       "cellular modified amino acid metabolic process", "lipid metabolic process",
                       "vitamin metabolic process", "sulfur compound metabolic process", "intracellular protein transport",
                       "nucleocytoplasmic transport", "autophagy", "inflammatory response", "mitochondrion organization",
                       "cytoskeleton organization", "microtubule-based movement", "peroxisome organization", "lysosome organization",
                       "chromosome segregation", "cell adhesion", "establishment or maintenance of cell polarity",
                       "programmed cell death", "photosynthesis", "mrna metabolic process", "snrna metabolic process", "vesicle-mediated transport",
                       "reproductive process", "digestive system process", "signaling", "cell differentiation", "protein catabolic process",
                       "extracellular matrix organization", "rna-mediated gene silencing", "telomere organization", "cell junction organization",
                       "wound healing", "ribosome biogenesis", "cilium organization", "anatomical structure development",
                       "cell motility", "nervous system process", "endocrine process", "protein maturation",
                       "transmembrane transport", "nucleobase-containing small molecule metabolic process",
                       "hepaticobiliary system process", "membrane organization", "protein-containing complex assembly",
                       "cell wall organization or biogenesis", "nitrogen cycle metabolic process", "protein localization to plasma membrane",
                       "defense response to other organism", "detoxification", "meiotic nuclear division",
                       "mitotic nuclear division", "mitochondrial gene expression", "carbohydrate derivative metabolic process")
    
    kept_components = which(colnames(go_components_table) %in% main_components)
    go_components_table = go_components_table[,kept_components]
    
    kept_functions = which(colnames(go_functions_table) %in% main_functions)
    go_functions_table = go_functions_table[,kept_functions]
    
    kept_processes = which(colnames(go_processes_table) %in% main_processes)
    go_processes_table = go_processes_table[,kept_processes]
    
  }
  
  
  feature_table[,"go_components"] = NULL
  feature_table[,"go_functions"] = NULL
  feature_table[,"go_processes"] = NULL
  
  return(list("feature_table" = feature_table,
              "go_components" = go_components_table,
              "go_functions" = go_functions_table,
              "go_processes" = go_processes_table))
}

#------------------------------------------------ General settings function ----
get_color_plot = function(color_palette) {
  color_df = data.frame(color=color_palette,
                        int=rep(1,length(color_palette)))
  color_df$color = factor(color_df$color, levels = color_df$color)
  color_plot = ggplot(color_df, aes(x=color, y=int)) + 
    geom_bar(stat="identity", fill=color_df$color, width=1) +
    theme(axis.title.y=element_blank(),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill='transparent', color=NA),
          panel.background = element_rect(fill='transparent')) + 
    coord_flip()
  return(color_plot)
}





