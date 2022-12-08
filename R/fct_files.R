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
  classes = as.vector(classes)
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


get_vistables_lips = function(samp_table,
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

  vistables_lips = list(
    samp_table = samp_table,
    data_classNorm = lips_classnorm,
    data_totLipidNorm = lips_totalnorm,
    data_classNorm_melt = lips_class,
    classData_totLipidNorm = lips_total
  )
  return(vistables_lips)

}


################################################### Specific to saturation plot
# Get a class and columns DF for a list of lipids
get_lips_df = function(table){
  species = colnames(table)
  class_list = get_lipid_classes(feature_list = colnames(table),
                                 uniques = F)
  lips_df = data.frame(matrix(data = NA, nrow = length(class_list), ncol = 0))
  lips_df[,"class"] = class_list
  lips_df[,"species"] = species
  return(lips_df)
}

# Get lipids metadata by evaluating the lipid type (TGs, simple, complex)
get_lips_metadata = function(lips_list){
  if (length(grep("TG",lips_list))>0){
    cs_count = lips_selector_1(lips_list) # TG lipids
  } else if (length(grep("/|_",lips_list)) > 0) {
    cs_count = lips_selector_2(lips_list) # Complex lipids
  }else{
    cs_count = lips_selector_3(lips_list) # Simple lipids
  }
  return(cs_count)
}


# Lipid data selection for TGs
lips_selector_1 = function(lips_list){

  # Get carbon count
  c_count = as.numeric(str_sub(lips_list,4,5))

  # Get saturation count
  s_count = as.numeric(str_sub(lips_list,7,-8))
  return(list(
    carbons = c_count,
    saturations = s_count
  ))
}


# Lipid data selection for Complex lipids
lips_selector_2 = function(lips_list) {
  main_data=sapply(lips_list,function(x){
    gsub("^.+?\\(","",strsplit(x,"/|_")[[1]][1])
  })
  c_count=sapply(main_data,function(x){strsplit(x,":",fixed = T)[[1]][1]})
  c_count=as.numeric(gsub(".*?([0-9]+).*", "\\1",c_count))
  s_count=sapply(main_data,function(x){as.numeric(strsplit(x,":",fixed = T)[[1]][2])})
  s_count = as.vector(s_count)
  return(list(
    carbons = c_count,
    saturations = s_count
  ))
}


# Lipid data selection for simple lipids
lips_selector_3 = function(lips_list){

  # Get carbon count
  c_count=sapply(lips_list,function(x){
    gsub("^.+?\\(","",strsplit(x,":",fixed = T)[[1]][1])
  })
  c_count=sapply(c_count,function(x){
    strsplit(x," ",fixed = T)[[1]][2]
  })
  c_count = as.vector(as.numeric(c_count))

  # Get saturation count
  s_count=sapply(lips_list,function(x){
    gsub("\\)","",strsplit(x,":",fixed = T)[[1]][2])
  })
  s_count = as.vector(as.numeric(s_count))

  return(list(
    carbons = c_count,
    saturations = s_count
  ))
}


# Get double bond / saturation data for lipid species contained in a lipidomics table
get_unsaturation_data = function(table){
  lips_df = get_lips_df(table)
  unsaturation_data = list()
  for (c in unique(lips_df[, "class"])) {

    # Get lipid species list
    lips_list = which(lips_df[, "class"] == c)
    lips_list = lips_df[lips_list, "species"]

    # Determine lipid class
    cs_count = get_lips_metadata(lips_list)

    class_data = data.frame(matrix(data = NA, nrow = length(lips_list), ncol = 0))
    class_data[,"species"] = lips_list
    class_data[,"carbons"] = cs_count$carbons
    class_data[,"saturation"] = cs_count$saturations

    unsaturation_data[[c]] = class_data

  }
  return(unsaturation_data)
}

# Merge outputs from get_unsaturation_data and get_wilcoxon_table to create the staplot_data for saturation plots
get_unsatplot_data = function(unsaturation_data, wilcoxon_table){
  unsatplot_data = list()
  for (lips_class in names(unsaturation_data)) {
    species_list = unsaturation_data[[lips_class]]$species
    class_data = wilcoxon_table[species_list,]
    class_data$carbons = unsaturation_data[[lips_class]]$carbons
    class_data$saturation = unsaturation_data[[lips_class]]$saturation
    unsatplot_data[[lips_class]] = class_data
  }
  return(unsatplot_data)
}


################################################  Statistical tests

# Wilcoxon test + BH adjust on lipids table
get_wilcoxon_table = function(lips_table, samp_table, selected_group, groups) {

  # data for group 1
  samples_1 = samp_table[,selected_group] == groups[1]
  samples_1 = lips_table[samples_1,]
  samples_1 = as.matrix(samples_1)

  # data for group 2
  samples_2 = samp_table[,selected_group] == groups[2]
  samples_2 = lips_table[samples_2,]
  samples_2 = as.matrix(samples_2)

  # Find columns with missing values
  nacols_1 = unique(which(is.na(samples_1), arr.ind = T)[,2])
  nacols_2 = unique(which(is.na(samples_2), arr.ind = T)[,2])
  nacols = unique(c(nacols_1, nacols_2))

  # Do wilcoxon test on each colum
  pval_list = c()
  fchange_list = c()

  for (i in c(1:ncol(samples_1))) {

    # If column has no NA values in any of the datasets
    if (!i %in% nacols){

      # Wilcoxon test
      p_val = wilcox.test(samples_1[,i], samples_2[,i])$p.value
      pval_list = c(pval_list, p_val)
      fchange = median(samples_2[,i],na.rm = T)/median(samples_1[,i],na.rm = T)
      fchange_list = c(fchange_list, fchange)

      # If column has NA values in one of both datasets
    } else {
      pval_list = c(pval_list, 1)
      fchange_list = c(fchange_list, 1)
    }
  }

  # Woops I divided by 0 part 1 : If some of the values in fchange_list are 0
  if(any(fchange_list==0)) {
    tmp_min_fchange = min(fchange_list[-which(fchange==0)],
                          1/fchange_list[-which(fchange==0)], na.rm = T)
    fchange_list[which(fchange_list==0)]=2^(log2(tmp_min_fchange)-1)
  }

  # Woops I divided by 0 part 2 : If some of the values in fchange_list are inf
  if(any(is.infinite(fchange_list)))
  {
    tmp_max_fchange=max(fchange_list)
    fchange_list[which(is.infinite(fchange_list))]=2^(log2(tmp_max_fchange)+1)
  }

  # Woops I divided by 0 part 3 : No idea what this is
  if(any(is.nan(fchange_list)))
  {
    fchange_list[which(is.nan(fchange_list))]=1
  }

  # p adjust using Benjamini-Hochberg
  pval_list_adjusted = p.adjust(pval_list,method = "BH")

  # Do -log10() on the adjusted pval for some reason. And round it, because reasons.
  log_pval_list_adjusted = round(-log10(pval_list_adjusted), 1)

  # Now do a random logFC, just do it. Also round and abs it, I don't make the rules
  log_fc = abs(round(log2(fchange_list), 1))

  # Set the names of the lists to the names of the features / columns
  names(pval_list) = names(fchange_list) = names(pval_list_adjusted) = colnames(lips_table)
  names(log_pval_list_adjusted) = names(log_fc) = colnames(lips_table)

  wilcoxon_table = list(p_values = pval_list,
                        foldchange = fchange_list,
                        BH_adj_p = pval_list_adjusted,
                        logP = log_pval_list_adjusted,
                        logFC = log_fc)

  wilcoxon_table = as.data.frame(wilcoxon_table)

  return(wilcoxon_table)
}


