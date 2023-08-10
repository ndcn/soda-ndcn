# Utility functions

#--------------------------------------------------------- Switch functions ----

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
         'Class summary table' = r6$tables$summary_class_table
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

soda_read_table = function(file_path) {
  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table = as.data.frame(readxl::read_xlsx(file_path))
  } else {
    sep = find_delim(path = file_path)
    data_table = read.csv(file_path,
                             header = T,
                             sep = sep,
                             check.names = FALSE)
  }
  return(data_table)
}

#-------------------------------------------------------- General utilities ----

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

lipidomics_plot_list = function() {
  plot_list = c("Class distribution" = "select_class_distribution",
                "Class comparison" = "select_class_comparison",
                "Volcano plot" = "select_volcano_plot",
                "Heatmap" = "select_heatmap",
                "PCA" = "select_pca",
                "Double bond plot" = "select_double_bond_plot"
  )
  return(plot_list)
}

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

get_feature_metadata = function(data_table) {
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

  plot_table = data.frame(table(base::factor((get_lipid_classes(colnames(data_table)[2:length(colnames(data_table))], uniques = F)), levels = groups)))
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


