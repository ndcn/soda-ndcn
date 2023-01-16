#--------------------------------------------------- Global table functions ----

get_idx_by_pattern = function(table, col, pattern, row_names = T) {
  if (row_names) {
    out_idx = rownames(table)[grep(pattern = pattern, x = table[,col], ignore.case = TRUE)]
  }else{
    out_idx = grep(pattern = pattern, x = table[,col], ignore.case = TRUE)
  }
  return(out_idx)
}

#------------------------------------------------------ Filtering functions ----
get_col_means = function(data_table) {
  means = colMeans(data_table, na.rm = TRUE)
  means[is.na(means)] = 0
  return(means)
}


blank_filter = function(data_table, blank_table, blank_multiplier, sample_threshold) {
  # Find features / columns below threshold
  blank_means = get_col_means(data_table = blank_table)
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


group_filter = function(data_table, blank_table, meta_table, del_cols, col_group, blank_multiplier, group_threshold){
  # Salvage some of the features with a group filtering (same as above but applied to groups)
  blank_means = get_col_means(data_table = blank_table)
  groups_total = table(meta_table[, col_group])
  saved_cols = c()
  for (col in 1:length(del_cols)) {
    threshold = blank_multiplier * blank_means[col]
    ratio = which(data_table[, col] > threshold)
    ratio = table(meta_table[, col_group][ratio])
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



#----------------------------------------- Lipid upload class preview plots ----

preview_class_plot = function(r6, total_cols, del_cols){

  total_values = table(get_lipid_classes(feature_list = colnames(r6$data_filtered),
                                         uniques = F))
  filtered_values_1 = rep(0,each=length(total_values))
  names(filtered_values_1) = names(total_values)
  
  if (!is.null(del_cols)){
    filtered_values_2 = table(get_lipid_classes(feature_list = colnames(r6$data_filtered[,-del_cols]),
                                                uniques = F))
  }else{
    filtered_values_2 = table(get_lipid_classes(feature_list = colnames(r6$data_filtered),
                                                uniques = F))
  }

  for (n in names(filtered_values_2)){
    filtered_values_1[n] = filtered_values_2[n]
  }
  
  lip_class = c(names(total_values), names(total_values))
  d_type = c(rep("kept", length(total_values)), rep("lost", length(total_values)))
  # lip_val = c(round(100*(filtered_values_1/total_values),1), rep(100, length(total_values)) - round(100*(filtered_values_1/total_values),1))
  lip_val = c(rep(100, length(total_values)) - round(100*(filtered_values_1/total_values),1), round(100*(filtered_values_1/total_values),1))
  class_df = data.frame(lip_class, d_type, lip_val)
  
  filtered_values_2 = data.frame(filtered_values_2)
  
  plot_1 = ggplot(filtered_values_2,
                  aes(x=Var1,
                      y=Freq)) +
    ggtitle("Total compound count")+
    geom_bar(stat="identity",
             fill = "#337ab7",
             show.legend = FALSE) +
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
    coord_flip() + 
    scale_y_reverse(limits = c(max(total_values), 0))
  
  
  
  plot_2 = ggplot(class_df,
                  aes(
                    fill=d_type ,
                    y=lip_val,
                    x=lip_class )) + 
    ggtitle("Relative compound count")+
    geom_bar(position="stack",
             stat="identity",
             show.legend = FALSE) + 
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
    scale_fill_manual(values = c("#f5f5f5", "#337ab7"))+
    coord_flip()
  
  return(grid.arrange(plot_1, plot_2, ncol=2))
}
  


