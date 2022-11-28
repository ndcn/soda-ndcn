#' @title get_idx
#' @description returns the index of values matching a pattern in a given col
#' for a given table.
#' @param table data.frame, Input dataframe.
#' @param col character, colomn to be searched.
#' @param pattern character, pattern to be searched in col values.
#' @return
get_idx = function(table, col, pattern) {
  return(grep(pattern = pattern, x = table[,col], ignore.case = TRUE))
}

set_index = function(table, col) {
  row.names(table) = table[,col]
  table[,col] = NULL
  return(table)
}

table_to_numeric = function(table){
  # Convert lipid_data to num only values
  for (col in colnames(table)) {
    table[,col] = as.numeric(table[,col])}

  # Replace NAs by 0s
  table[is.na(table)] = 0

  return(table)
}


get_lipid_classes = function(feature_list, uniques = TRUE){
  classes = sapply(feature_list, function(x)
    strsplit(x = x,
             split = " ",
             fixed = TRUE)[[1]][1])
  if (uniques) {
    return(unique(classes))}
  else{
    return(classes)
  }
}

get_samp_table = function(samp_table,
                          pattern_blank,
                          pattern_qc,
                          type_col) {

  # Get blank, qc and samples indexes
  idx_blanks = get_idx(table = samp_table, col = type_col, pattern = pattern_blank)
  idx_qcs = get_idx(samp_table, type_col, pattern_qc)
  idx_samples = c(1:length(samp_table[,type_col]))[-c(idx_blanks, idx_qcs)]

  # Filter samples table to keep only sample rows
  samp_table = samp_table[idx_samples,]

  return(samp_table)
}

blank_filter = function(table, idx_blanks, idx_samples, blank_thr){
  # Mean value for blanks for each lipid species
  blank_mean = colMeans(table[idx_blanks,])

  # get the frequency at which is col is below threshold for each sample
  del_cols_freq = which(t(t(table[idx_samples,]) <= blank_thr * blank_mean),
                        arr.ind = TRUE)
  return(list("blank" = blank_mean, "idx" = del_cols_freq[,2]))
}

sample_count_filter = function(del_cols_freq, idx_samples, blank_thr_nr_of_samples) {
  # Frequency table for each feature when they are below threshold in a sample
  count_table = table(del_cols_freq)

  # Get features that are below threshold for 80% of the samples (default blank_thr_nr_of_samples)
  del_cols = which((count_table / length(idx_samples)) > blank_thr_nr_of_samples)
  del_cols = names(count_table[del_cols])
  return(del_cols)
}

blank_and_group_filter = function(lips_table, samp_table, idx_blanks, idx_samples,
                                  blank_thr, blank_thr_nr_of_samples, group_thr_nr_of_samples,
                                  group_col){
  # Get the mean values from each feature in blanks
  # del_cols is a list of features for which the values were below blank threshold in some samples
  del_cols_freq = blank_filter(lips_table, idx_blanks, idx_samples, blank_thr)
  blank_mean = del_cols_freq$blank
  del_cols_freq = del_cols_freq$idx

  # Get the col / feature names for which 80% (default) of the samples are below blank threshold
  del_cols = sample_count_filter(del_cols_freq, idx_samples, blank_thr_nr_of_samples)


  # Search for features in del_cols that can be spared from filtering by considering groups
  save_idx = c()
  groups_total = table(samp_table[,group_col])

  # which proportion of samples from one group is above the threshold
  for (i in c(1:length(del_cols))){
    current_feature = as.numeric(del_cols[i])
    local_threshold = blank_thr * blank_mean[current_feature]

    # Consider now the groups : for current feature, how many of them are above threshold
    groups_above_threshold = which(lips_table[idx_samples, current_feature] > local_threshold)
    groups_above_threshold = table(samp_table[,group_col][groups_above_threshold])

    # get the ratio of samples above threshold in each group to the total sample count in each group
    ratio = groups_above_threshold / groups_total

    if (any(ratio >= group_thr_nr_of_samples))
      save_idx <- c(save_idx, i)}


  # If there are features to be saved, remove them from del_cols
  if (length(save_idx) > 0){
    del_cols = del_cols[-save_idx]}

  # If there are features to remove, delete the columns from the data table
  if (length(del_cols) > 0){
    lips_table <- lips_table[, -as.numeric(del_cols)]}

  return(lips_table[idx_samples,])
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
      sum_val = sum(lips_table[i, cols])

      # Normalise row to sum_val
      if (sum_val > 0) {
        lips_table[i, cols] = lips_table[i, cols]/sum_val}}}

  return(lips_table)
}

normalise_lips = function(samp_table,
                          lips_table,
                          blank_thr,
                          blank_thr_nr_of_samples,
                          group_thr_nr_of_samples,
                          type_col,
                          group_col,
                          pattern_blank,
                          pattern_qc) {


  # Get blank indexes
  idx_blanks = get_idx(table = samp_table, col = type_col, pattern = pattern_blank)
  idx_qcs = get_idx(samp_table, type_col, pattern_qc)
  idx_samples = c(1:length(samp_table[,type_col]))[-c(idx_blanks, idx_qcs)]

  # Filter out QCs and Blanks from the sample table
  samp_table = get_samp_table(samp_table,
                              pattern_blank = pattern_blank,
                              pattern_qc = pattern_qc,
                              type_col = type_col)


  # Set group column to factor for frequency calculations during filters
  samp_table[,group_col] = as.factor(samp_table[,group_col])

  # Blank filter and remove blanks & QCs from table
  lips_table = blank_and_group_filter(lips_table, samp_table, idx_blanks, idx_samples,
                                      blank_thr, blank_thr_nr_of_samples, group_thr_nr_of_samples,
                                      group_col)

  # Normalise to lipid classes
  lips_table_norm_class = normalise_lipid_class(lips_table)

  # Normalise to all lipids
  lips_table_norm_total = lips_table/rowSums(lips_table)

  return(list("class" = lips_table_norm_class, "total" = lips_table_norm_total))

}



table_to_long = function(table,
                         samp_table,
                         pattern_blank,
                         pattern_qc,
                         type_col){

  # Filter out QCs and Blanks from the sample table
  samp_table = get_samp_table(samp_table,
                              pattern_blank = pattern_blank,
                              pattern_qc = pattern_qc,
                              type_col = type_col)

  # Set sample IDs as a separate column
  samp_table$ID = rownames(samp_table)

  table = tidyr::pivot_longer(data = cbind(samp_table, table),
                              cols = -colnames(samp_table),
                              names_to = "variable",
                              values_to = "value")
  return(table)
}

get_lipid_class_table = function(table, samp_table, pattern_blank, pattern_qc, type_col){

  # Get unique lipid classes
  classes = get_lipid_classes(feature_list = colnames(table), uniques = TRUE)

  # Get a column vector to find easily which columns belong to each lipid group
  col_vector = get_lipid_classes(feature_list = colnames(table), uniques = FALSE)

  # Initialise the output table
  out_table = data.frame(matrix(nrow = nrow(table), ncol = 0), row.names = rownames(table))

  # Fill the table
  for (lipid in classes) {
    new_col = c()
    col_list = which(col_vector == lipid)
    for (i in rownames(table)) {
      new_col = c(new_col, sum(table[i, col_list]))
    }
    out_table[,lipid] = new_col
  }

  return(out_table)
}
get_class_plot_table = function(table, samp_table, group_col){
  samp_list = rownames(table)
  class_list = colnames(table)
  group_list = unique(samp_table[,group_col])

  out_table = data.frame(matrix(data = 0.0,
                                nrow = length(class_list),
                                ncol = length(group_list)))
  rownames(out_table) = class_list
  colnames(out_table) = group_list

  for (c in class_list) {
    for (g in group_list){
      s = rownames(samp_table)[samp_table[,group_col] == g]
      m = mean(as.matrix(table[s, c]))
      out_table[c,g] = m
    }
  }

  return(out_table)
}


bundle_lips = function(samp_table,
                       lips_table,
                       blank_thr,
                       blank_thr_nr_of_samples,
                       group_thr_nr_of_samples,
                       type_col,
                       group_col,
                       pattern_blank,
                       pattern_qc){


  # Get lipid values normalised to total lipids and lipid classes
  lips_classnorm = normalise_lips(samp_table,
                                  lips_table,
                                  blank_thr,
                                  blank_thr_nr_of_samples,
                                  group_thr_nr_of_samples,
                                  type_col,
                                  group_col,
                                  pattern_blank,
                                  pattern_qc)
  lips_totalnorm = lips_classnorm[["total"]]
  lips_classnorm = lips_classnorm[["class"]]

  # Get other variations of the same data
  lips_class = get_lipid_class_table(lips_classnorm, samp_table, pattern_blank, pattern_qc, type_col)
  lips_total = get_lipid_class_table(lips_totalnorm, samp_table, pattern_blank, pattern_qc, type_col)

  # Remove blanks and QCs from sample table (redundant)
  samp_table = get_samp_table(samp_table,
                              pattern_blank,
                              pattern_qc,
                              type_col)

  bundled_tables = list(
    samp_table = samp_table,
    data_classNorm = lips_classnorm,
    data_totLipidNorm = lips_totalnorm,
    data_classNorm_melt = lips_class,
    classData_totLipidNorm = lips_total
  )
  return(bundled_tables)

}






# test_func = function(samp_table,
#                        lips_table,
#                        blank_thr,
#                        blank_thr_nr_of_samples,
#                        group_thr_nr_of_samples,
#                        type_col,
#                        group_col,
#                        pattern_blank,
#                        pattern_qc){
#   print(type_col)
#   print(group_col)
#   print(pattern_blank)
#   print(pattern_qc)
#   return("Truffles")
# }




