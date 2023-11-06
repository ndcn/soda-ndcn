
volcano_main = function(fc_vals = volcano_table$fold_change,
                        p_vals = volcano_table$q_val_bh,
                        names = rownames(volcano_table),
                        y_label = '-Log10(p-value)',
                        groups = NULL,
                        displayed_plot = 'main',
                        p_val_threshold = 0.05,
                        fc_threshold = 2,
                        marker_size = 6,
                        opacity = 1) {

  # Checks
  if (!(displayed_plot %in% c('main', 'all', 'left', 'right', 'top'))) {
    stop("displayed_plot should be one of ['main', 'all', 'left', 'right', 'top']")
  }

  data = data.frame(
    fold_change = fc_vals,
    p_values = p_vals,
    names = names
  )

  # Format data
  data$log2_fold_change = log2(data$fold_change)
  data$log10_p_values = -log10(data$p_values)

  data$groups = 'Not significant'
  data$groups[data$p_values < p_val_threshold & data$log2_fold_change > log2(fc_threshold)] = "Overexpressed"
  data$groups[data$p_values < p_val_threshold & data$log2_fold_change < -log2(fc_threshold)] = "Underexpressed"

  data$opacity = opacity
  data$color = "gray47"
  data$color[data$p_values < p_val_threshold & data$log2_fold_change > log2(fc_threshold)] = "red"
  data$color[data$p_values < p_val_threshold & data$log2_fold_change < -log2(fc_threshold)] = "blue"
  data$color[data$p_values > p_val_threshold & data$log2_fold_change > -log2(fc_threshold) & data$log2_fold_change < log2(fc_threshold)] = "grey"


  # Produce the data tables & plots
  if (length(which(is.na(data$log10_p_values))) > 0) { # Top violin
    top_data = data[which(is.na(data$log10_p_values)),]
    data = data[-which(is.na(data$log10_p_values)),]
    inf_idx = which(base::is.infinite(top_data$log2_fold_change))
    top_data = top_data[-inf_idx,]
    print(paste0('Dropped ', length(inf_idx), ' features with no FC nor p-values.'))
    top_data$color[top_data$log2_fold_change > log2(fc_threshold)] = "red"
    top_data$color[top_data$log2_fold_change < -log2(fc_threshold)] = "blue"

    top_violin = plot_volcano_violin(data = top_data$log2_fold_change,
                                     threshold = log2(fc_threshold),
                                     names = top_data$names,
                                     side = 'top',
                                     opacity = opacity,
                                     marker_size = marker_size,
                                     show_legend = F)

  } else {top_data = NULL}

  if (length(which(data$log2_fold_change == -Inf)) > 0) { # Left violin
    left_data = data[which(data$log2_fold_change == -Inf),]
    data = data[-which(data$log2_fold_change == -Inf),]

    left_violin = plot_volcano_violin(data = left_data$log10_p_values,
                                      threshold = -log10(p_val_threshold),
                                      names = left_data$names,
                                      side = 'left',
                                      opacity = opacity,
                                      marker_size = marker_size,
                                      show_legend = F)

  } else {left_data = NULL}

  if (length(which(data$log2_fold_change == Inf)) > 0) { # right violin
    right_data = data[which(data$log2_fold_change == Inf),]
    data = data[-which(data$log2_fold_change == Inf),]

    right_violin = plot_volcano_violin(data = right_data$log10_p_values,
                                       threshold = -log10(p_val_threshold),
                                       names = right_data$names,
                                       side = 'right',
                                       opacity = opacity,
                                       marker_size = marker_size,
                                       show_legend = F)

  } else {right_data = NULL}

  # Main plot y_label
  main_plot = plot_volcano(data = data,
                           marker_size = marker_size,
                           p_val_threshold = p_val_threshold,
                           fc_threshold = fc_threshold,
                           y_axis_title = y_label,
                           show_y_title = F)

  # Blank plot
  blank_plot = plot_ly(type = 'scatter', mode = 'markers')
  blank_plot = plotly::layout(blank_plot,
                              xaxis = list(zeroline = F,
                                           showticklabels = F,
                                           showgrid = F),
                              yaxis = list(zeroline = F,
                                           showticklabels = F,
                                           showgrid = F))



  if (is.null(left_data) & is.null(right_data) & is.null(top_data) | (displayed_plot == 'main')) { # Only main
    out_plot = main_plot
    out_plot = plotly::layout(out_plot,
                              yaxis = list(title = y_label))
  } else if (!is.null(top_data) & (displayed_plot == 'top')) { # Export top violin

    out_plot = top_violin

  } else if (!is.null(left_data) & (displayed_plot == 'left')) { # Export left violin

    out_plot = left_violin

  } else if (!is.null(right_data) & (displayed_plot == 'right')) { # Export right violin

    out_plot = right_violin

  } else if (is.null(left_data) & is.null(right_data) & !is.null(top_data) & (displayed_plot == 'all')) { # Top only

    out_plot = plotly::subplot(
      top_violin,
      main_plot,
      nrows = 2,
      shareX = TRUE,
      heights = c(0.1, 0.9)
    )

  } else if (!is.null(left_data) & is.null(right_data) & is.null(top_data) & (displayed_plot == 'all')) { # Left only

    out_plot = plotly::subplot(
      list(left_violin, main_plot),
      shareX = TRUE,
      shareY = TRUE,
      titleX = T,
      titleY = F,
      widths = c(0.1, 0.9)
    )

  } else if (is.null(left_data) & !is.null(right_data) & is.null(top_data) & (displayed_plot == 'all')) { # Right only

    out_plot = plotly::subplot(
      list(main_plot, right_violin),
      shareX = TRUE,
      shareY = TRUE,
      titleX = T,
      titleY = F,
      widths = c(0.9, 0.1)
    )

  } else if (!is.null(left_data) & !is.null(right_data) & is.null(top_data) & (displayed_plot == 'all')) { # Left and Right

    out_plot = plotly::subplot(
      matrix(list(left_violin, main_plot, right_violin),
             ncol = 3, byrow = TRUE),
      shareX = TRUE,
      shareY = TRUE,
      titleX = T,
      titleY = F,
      widths = c(0.1, 0.8, 0.1)
    )

    out_plot = plotly::layout(out_plot,
                                yaxis = list(title = y_label))
  } else if (!is.null(left_data) & !is.null(right_data) & !is.null(top_data) & (displayed_plot == 'all')) { # All violins

    out_plot = plotly::subplot(
      list(blank_plot, top_violin, blank_plot,
                  left_violin, main_plot, right_violin),
      nrows = 2,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.02,
      widths = c(0.1, 0.8, 0.1),
      heights = c(0.1, 0.9)
    )
  } else if (!is.null(left_data) & is.null(right_data) & !is.null(top_data) & (displayed_plot == 'all')) { # Top and left

    out_plot = plotly::subplot(
      list(blank_plot, top_violin,
           left_violin, main_plot),
      nrows = 2,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.02,
      widths = c(0.1, 0.9),
      heights = c(0.1, 0.9)
    )

  } else if (is.null(left_data) & !is.null(right_data) & !is.null(top_data) & (displayed_plot == 'all')) { # Top and right

    out_plot = plotly::subplot(
      list(top_violin, blank_plot,
           main_plot, right_violin),
      nrows = 2,
      shareX = TRUE,
      shareY = TRUE,
      titleX = TRUE,
      titleY = TRUE,
      margin = 0.02,
      widths = c(0.9, 0.1),
      heights = c(0.1, 0.9)
    )
  } else {
    warning('Selected plot unavailable, returning blank.')
    out_plot = blank_plot
  }

  return(out_plot)

}

plot_volcano_violin = function(data, threshold, names = NULL, side, opacity = 1, marker_size = 6, show_legend = F) {

  if (!(side %in% c('left', 'right', 'top'))) {
    stop('side must be in [left, right, top]')
  }

  if (side == 'left') {
    col_line = 'blue'
    col_fill = 'lightblue'
    title = 'Underexpressed'
    x_val = 'Left only'
  } else if (side == 'right') {
    col_line = 'red'
    col_fill = 'pink'
    title = 'Overexpressed'
    x_val = 'Right only'
  } else if (side == 'top') {
    return(plot_volcano_violin_top(data = data,
                                   threshold = threshold,
                                   names = names,
                                   opacity = opacity,
                                   marker_size = marker_size,
                                   show_legend = show_legend))
  }

  colors = rep('gray', length(data))
  colors[which(data >= threshold)] = col_line

  p = plotly::plot_ly()

  p = plotly::add_trace(p, y = data[which(data >= threshold)], x = x_val, type = "violin",
                        box = list(visible = FALSE),
                        line = list(color = col_line),
                        fillcolor = col_fill,
                        meanline = list(visible = F),
                        opacity = opacity,
                        points = FALSE,
                        name = title,
                        legendgroup = title,
                        hoverinfo = 'none',
                        showlegend = F)

  p = plotly::add_trace(p, y = data[which(data < threshold)], x = x_val, type = "violin",
                        box = list(visible = FALSE),
                        line = list(color = 'darkgray'),
                        fillcolor = 'lightgray',
                        meanline = list(visible = F),
                        opacity = opacity,
                        points = FALSE,
                        name = 'Not significant',
                        legendgroup = 'Not significant',
                        hoverinfo = 'none',
                        showlegend = F)

  if (length(data[which(data >= threshold)]) > 0) {
    p = plotly::add_trace(p,
                          type = "scatter", mode = "markers",
                          y = data[which(data >= threshold)], x = x_val,
                          marker = list(size = marker_size, color = col_line, opacity = opacity, line = list(width = 0.5, color = 'white')),
                          name = title,
                          legendgroup = title,
                          text = names[which(data >= threshold)],
                          hoverinfo = 'text',
                          showlegend = show_legend)
  }

  if (length(data[which(data < threshold)]) > 0) {
    p = plotly::add_trace(p,
                          type = "scatter", mode = "markers",
                          y = data[which(data < threshold)], x = x_val,
                          marker = list(size = marker_size, color = 'gray', opacity = opacity, line = list(width = 0.5, color = 'white')),
                          name = 'Not significant',
                          legendgroup = 'Not significant',
                          text = names[which(data < threshold)],
                          hoverinfo = 'text',
                          showlegend = show_legend)
  }


  p = plotly::layout(p,
                     shapes = list(
                       list(
                         type = "line",
                         x0 = -0.3,
                         x1 = 0.3,
                         y0 = threshold,
                         y1 = threshold,
                         line = list(color = "black", width = 1, dash = "dot")
                         )
                       )
                     )




  return(p)

}

plot_volcano_violin_top = function(data,
                                   threshold,
                                   names,
                                   opacity,
                                   marker_size,
                                   show_legend) {
  p = plotly::plot_ly()

  p = plotly::add_trace(p,
                        y = 'No p-value',
                        x = data,
                        type = "violin",
                        box = list(visible = FALSE),
                        line = list(color = 'darkgray'),
                        fillcolor = 'ghostwhite',
                        meanline = list(visible = F),
                        opacity = opacity,
                        points = FALSE,
                        name = 'Not significant',
                        legendgroup = 'Not significant',
                        hoverinfo = 'none',
                        showlegend = F,
                        orientation = 'h')

  if (length(data[which(data >= threshold)]) > 0) {
    p = plotly::add_trace(p,
                          type = "scatter",
                          mode = "markers",
                          y = 'No p-value',
                          x = data[which(data >= threshold)],
                          marker = list(size = marker_size, color = 'red', opacity = opacity, line = list(width = 0.5, color = 'white')),
                          name = 'Overexpressed',
                          legendgroup = 'Overexpressed',
                          text = names[which(data >= threshold)],
                          hoverinfo = 'text',
                          showlegend = show_legend)
  }


  if (length(data[which(data <= -threshold)]) > 0) {
    p = plotly::add_trace(p,
                          type = "scatter",
                          mode = "markers",
                          y = 'No p-value',
                          x = data[which(data <= -threshold)],
                          marker = list(size = marker_size, color = 'blue', opacity = opacity, line = list(width = 0.5, color = 'white')),
                          name = 'Underexpressed',
                          legendgroup = 'Underexpressed',
                          text = names[which(data <= -threshold)],
                          hoverinfo = 'text',
                          showlegend = show_legend)
  }


  if (length(data[which((data < threshold) & (data > -threshold))]) > 0) {
    p = plotly::add_trace(p,
                          type = "scatter",
                          mode = "markers",
                          y = 'No p-value',
                          x = data[which((data < threshold) & (data > -threshold))],
                          marker = list(size = marker_size, color = 'darkgray', opacity = opacity, line = list(width = 0.5, color = 'white')),
                          name = 'Not significant',
                          legendgroup = 'Not significant',
                          text = names[which((data < threshold) & (data > -threshold))],
                          hoverinfo = 'text',
                          showlegend = show_legend)
  }

  p = plotly::layout(p,
                     xaxis = list(title = "Log2(Fold Change)"
                     ),
                     shapes = list(
                       # Vertical line at x = -1
                       list(
                         type = "line",
                         x0 = -threshold,
                         x1 = -threshold,
                         y0 = -0.3,
                         y1 = 0.3,
                         line = list(color = "black", width = 1, dash = "dot")
                       ),
                       # Vertical line at x = 1
                       list(
                         type = "line",
                         x0 = threshold,
                         x1 = threshold,
                         y0 = -0.3,
                         y1 = 0.3,
                         line = list(color = "black", width = 1, dash = "dot")
                       )
                     ))
  return(p)
}

plot_volcano = function(data, marker_size, p_val_threshold = 0.05, fc_threshold = 2, show_y_title = F , y_axis_title = '-Log10(p-value)') {

  if (!show_y_title){
    y_axis_title = NULL
  }

  main_plot = plotly::plot_ly()

  for (group in unique(data$groups)) {
    subset_data = data[data$groups == group, ]
    main_plot = plotly::add_trace(
      main_plot,
      x = subset_data$log2_fold_change,
      y = subset_data$log10_p_values,
      text = subset_data$names,
      color = I(subset_data$color),
      type = "scatter",
      mode = "markers",
      marker = list(size = marker_size,
                    opacity = subset_data$opacity,
                    line = list(width = 0.5, color = 'white')),
      legendgroup = group,
      name = group,
      showlegend = TRUE,
      hoverinfo = 'text'
    )
  }

  main_plot = plotly::layout(main_plot,
                             xaxis = list(title = "Log2(Fold Change)",
                                          zeroline = T,
                                          range = c(-round(max(abs(data$log2_fold_change))),
                                                    round(max(abs(data$log2_fold_change)))
                                          )
                             ),
                             yaxis = list(title = y_axis_title),
                             hovermode = "closest",
                             shapes = list(
                               # Vertical line at x = -1
                               list(
                                 type = "line",
                                 x0 = -log2(fc_threshold),
                                 x1 = -log2(fc_threshold),
                                 y0 = 0,
                                 y1 = max(data$log10_p_values),
                                 line = list(color = "black", width = 1, dash = "dot")
                               ),
                               # Vertical line at x = 1
                               list(
                                 type = "line",
                                 x0 = log2(fc_threshold),
                                 x1 = log2(fc_threshold),
                                 y0 = 0,
                                 y1 = max(data$log10_p_values),
                                 line = list(color = "black", width = 1, dash = "dot")
                               ),
                               # Horizontal line at y = -log10(0.05)
                               list(
                                 type = "line",
                                 x0 = -round(max(abs(data$log2_fold_change))),
                                 x1 = round(max(abs(data$log2_fold_change))),
                                 y0 = -log10(p_val_threshold),
                                 y1 = -log10(p_val_threshold),
                                 line = list(color = "black", width = 1, dash = "dot")
                               )
                             ))
  return(main_plot)
}
