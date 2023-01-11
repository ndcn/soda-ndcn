#--------------------------------------------------- Global table functions ----

#' @title get_idx_by_pattern
#' @description returns the index of values matching a pattern in a given col
#' for a given table.
#' @param table data.frame, Input dataframe.
#' @param col character, colomn to be searched.
#' @param pattern character, pattern to be searched in col values.
#' @return
get_idx_by_pattern = function(table, col, pattern) {
  return(grep(pattern = pattern, x = table[,col], ignore.case = TRUE))
}

#------------------------------------------------------ Filtering functions ----
get_col_means = function(table, row_ids) {
  means = colMeans(table[row_ids,], na.rm = TRUE)
  means[is.na(means)] = 0
  return(means)
}


blank_filter = function(data_table, idx_blanks, blank_multiplier, sample_threshold) {
  # Find features / columns below threshold
  blank_means = get_col_means(table = data_table, row_ids = idx_blanks)
  del_cols = c()
  data_table[is.na(data_table)] = 0
  total_samples = length(rownames(data_table))
  for (col in 1:length(data_table)){
    threshold = blank_multiplier * blank_means[col]
    above_threshold = sum(data_table[, col] > threshold)
    if ((above_threshold/total_samples) < sample_threshold) {
      del_cols = c(del_cols, col)
    }
  }
  return(del_cols)
}


group_filter = function(data_table, meta_table, del_cols, idx_samples, idx_blanks, col_group, blank_multiplier, group_threshold){
  # Salvage some of the features with a group filtering (same as above but applied to groups)
  blank_means = get_col_means(table = data_table, row_ids = idx_blanks)
  groups_total = table(meta_table[idx_samples, col_group])
  saved_cols = c()
  for (col in 1:length(del_cols)) {
    threshold = blank_multiplier * blank_means[col]
    ratio = which(data_table[idx_samples, col] > threshold)
    ratio = table(meta_table[idx_samples, col_group][ratio])
    ratio = ratio/groups_total[names(ratio)]
    if (any(ratio >= group_threshold)) {
      saved_cols = c(saved_cols, col)
    }
  }
  return(saved_cols)
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
  
  # For each unique lipid class...
  for (lip_class in classes_unique){
    
    # Get columns from that class...
    cols = which(classes == lip_class)
    
    # and for each sample / row...
    for (i in c(1:nrow(lips_table))){
      
      # Get sum of that samples lipid values
      sum_val = sum(lips_table[i, cols], na.rm = T)
      
      # Normalise row to sum_val
      if (sum_val > 0) {
        lips_table[i, cols] = lips_table[i, cols]/sum_val}}}
  
  return(lips_table)
}

#---------------------------------------------------- Class table functions ----
get_lipid_class_table = function(table){
  
  # Get unique lipid classes
  classes = get_lipid_classes(feature_list = colnames(table), uniques = TRUE)
  
  # Get a column vector to find easily which columns belong to each lipid group
  col_vector = get_lipid_classes(feature_list = colnames(table), uniques = FALSE)
  
  # Initialise the output table
  out_table = data.frame(matrix(nrow = nrow(table), ncol = 0), row.names = rownames(table))
  
  # Replace NA by 0s
  table[is.na(table)] = 0
  
  # Fill the table
  for (lipid in classes) {
    new_col = c()
    col_list = which(col_vector == lipid)
    for (i in rownames(table)) {
      new_col = c(new_col, sum(table[i, col_list], na.rm = TRUE))
    }
    out_table[,lipid] = new_col
  }
  
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
  annotations[[i]] = list(x = -0.08, y = 0.5, text = "Percentage of total lipid",
                          font = list(size = 10),
                          textangle = 270, showarrow = FALSE, xref='paper',
                          yref='paper')
  return(annotations)}

