#' plotting
#'
#' @title Bar plot with lipid class distribution
#'
#' @description Bar plot with lipid class distribution.
#'
#' @return ggplot2 object
#'
#' @author Yassene Mohammed
#' @author Rico Derks
#'
#' @importFrom reshape melt
#' @importFrom ggplot2 ggplot aes_string geom_col ylab theme_bw
#' @import plotly
#' @import tidyr
#' @import RColorBrewer
#'
#' @noRd
#'
plot_class_distribution <- function(table,
                         samp_table,
                         group_col) {
  plot_data = get_class_plot_table(table, samp_table, group_col)
  fig = plot_ly()
  for (col in colnames(plot_data)) {
    fig = fig %>% add_trace(x = rownames(plot_data), y = plot_data[,col], name = col, type  = "bar")
    fig = fig %>% layout(legend = list(orientation = 'h', xanchor = "center", x = 0.5))
  }
  fig

  return(fig)
}


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

plot_class_comparison = function(table,
                                 samp_table,
                                 group_col){
  default_cols = c('#1f77b4', '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f', '#bcbd22', '#17becf')
  groups = unique(samp_table[,group_col])
  class_list = colnames(table)
  annotations = get_subplot_titles(class_list)
  dims = get_subplot_dim(class_list)
  plot_list = c()
  cleared_groups = c()
  j = 1
  for (c in class_list) {
    i = 1
    subplot = plot_ly()
    for (g in groups){
      if (g %in% cleared_groups) {
        first_bool = FALSE
      }else{
        first_bool = TRUE
        cleared_groups = c(cleared_groups, g)
      }
      s = rownames(samp_table)[samp_table[, group_col] == g]
      d = table[s, c]
      m = mean(d)
      subplot = subplot %>% add_trace(x = g, y = m, type  = "bar", name = g,
                                      color = default_cols[i], alpha = 1,
                                      legendgroup=i, showlegend = first_bool)
      subplot = subplot %>% add_trace(x = g, y = d, type  = "box", boxpoints = "all",
                                      pointpos = 0, name = g, color = default_cols[i],
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
  return(fig)
}


#' @title Volcano plot
#'
#' @description Interactive volcano plot.
#'
#' @param all_cls data
#'
#' @return a plotly object.
#'
#' @author Yassene Mohammed
#' @author Rico Derks
#'
#' @importFrom plotly plot_ly add_markers layout
#' @importFrom grDevices rainbow
#'
#' @noRd
#'
plot_class_volcano <- function(all_cls) {

  # find infinite values and impute
  ind_inf <- which(is.infinite(all_cls$logFC))
  if(length(ind_inf) > 0) {
    impute_inf <- max(abs(all_cls$logFC[-ind_inf]))+1
    all_cls$logFC[ind_inf[which(all_cls$logFC[ind_inf]<0)]] <- -impute_inf
    all_cls$logFC[ind_inf[which(all_cls$logFC[ind_inf]>0)]] <- impute_inf
  }

  ply1 <- plotly::plot_ly(all_cls,
                          x = ~logFC,
                          y = ~logP,
                          colors = grDevices::rainbow(n = 100),
                          text = ~paste(lipid_name, "<br>logP:", round(logP, 2), "<br>logFC:", round(logFC, 2))) |>
    plotly::add_markers(color = ~lipid_class) |>
    plotly::layout(xaxis = list(title = "log2(fold change)"),
                   yaxis = list(title = "-log10(p-value)"),
                   shapes = list(vline(-1),
                                 vline(1),
                                 hline(-log10(0.05))))

  return(ply1)
}


#' @title Vertical line
#'
#' @description Create vertical line in plotly plot.
#'
#' @param x intersection with x-axis
#' @param color color of the line
#'
#' @return a plotly object.
#'
#' @author Yassene Mohammed
#' @author Rico Derks
#'
#' @noRd
#'
vline <- function(x = 0, color = "blue") {
  list(
    type = "line",
    y0 = 0,
    y1 = 1,
    yref = "paper",
    x0 = x,
    x1 = x,
    line = list(color = color,
                width = 1,
                dash = "dash")
  )
}


#' @title Horizontal line
#'
#' @description Create horizontal line in plotly plot.
#'
#' @param y intersection with y-axis
#' @param color color of the line
#'
#' @return a plotly object.
#'
#' @author Yassene Mohammed
#' @author Rico Derks
#'
#' @noRd
#'
hline <- function(y = 0, color = "blue") {
  list(
    type = "line",
    x0 = 0,
    x1 = 1,
    xref = "paper",
    y0 = y,
    y1 = y,
    line = list(color = color,
                width = 1,
                dash = "dash")
  )
}

