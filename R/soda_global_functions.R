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




blank_filter = function(data_table, blank_table, blank_multiplier, sample_threshold) {
  # Find features / columns below threshold
  blank_means = get_col_means(data_table = blank_table)
  del_cols = c()
  data_table[is.na(data_table)] = 0
  total_samples = nrow(data_table)
  for (col in colnames(data_table)){
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
  for (col in del_cols) {
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

# Combination of both the above functions
bland_and_group_filter = function(data_table,
                                  blank_table,
                                  meta_table,
                                  col_group,
                                  blank_multiplier,
                                  sample_threshold,
                                  group_threshold
                                  ) {
  del_cols = blank_filter(data_table, blank_table, blank_multiplier, sample_threshold)
  if (!is.null(del_cols)) {
    saved_cols = group_filter(data_table, blank_table, meta_table, del_cols, col_group, blank_multiplier, group_threshold)
    del_cols = setdiff(del_cols,saved_cols)
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
                             group_threshold,
                             drop_method
                             ) {
  
  all_del_cols = c()
  if (drop_method == "flatten") {
    
    # Blank and group filter
    del_cols = bland_and_group_filter(data_table = data_table,
                                      blank_table = blank_table,
                                      meta_table = meta_table_filtered,
                                      col_group = col_group,
                                      blank_multiplier = blank_multiplier,
                                      sample_threshold = sample_threshold,
                                      group_threshold = group_threshold
                                      )
    
  } else {
    
    
    all_batches = unique(meta_table_raw[, col_batch])
    
    for (b in all_batches) {
      # Get indices
      batch_idx = which(meta_table_raw[, col_batch] == b)
      batch_blanks = base::intersect(batch_idx, idx_blanks)
      batch_samples = base::intersect(batch_idx, idx_samples)
      
      # Get rownames
      batch_blanks = meta_table_raw[batch_blanks, col_id_meta]
      batch_samples = meta_table_raw[batch_samples, col_id_meta]
      
      # Blank and group filter
      del_cols = bland_and_group_filter(data_table = data_table[batch_samples,],
                                        blank_table = blank_table[batch_blanks,],
                                        meta_table = meta_table_filtered,
                                        col_group = col_group,
                                        blank_multiplier = blank_multiplier,
                                        sample_threshold = sample_threshold,
                                        group_threshold = group_threshold)
      
      all_del_cols[[length(all_del_cols) + 1]] = del_cols
    }
    
    
    if (drop_method == "union") {
      del_cols = base::unlist(x = all_del_cols,
                              recursive = TRUE,
                              use.names = FALSE)
      del_cols = sort(unique(del_cols))
    } else if (drop_method == "intersect") {
      del_cols = all_del_cols[[1]]
      for (i in 1:length(all_del_cols)){
        del_cols = base::intersect(del_cols, all_del_cols[[i]])
      }
    }
  }
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
    print("Wilcoxon selected")
  } else if (test == "T-test") {
    test_function = stats::t.test
    print("T-test selected")
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
  return(keep_cols)
}




#------------------------------------------------------- Plotting functions ----

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