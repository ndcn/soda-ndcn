pca_main = function(data_table, sample_groups = NULL, feature_groups = NULL, nPcs = 2, displayed_pc_1 = 1, displayed_pc_2 = 2, pca_method = 'svd', completeObs = T, displayed_plots = "both", colors_palette = "Spectral", return_data = FALSE, width = NULL, height = NULL) {

  # Check if arguments are valid
  if (!(pca_method %in% pcaMethods::listPcaMethods())){
    print(paste0('Invalid pca_method: must be one of [', paste(pcaMethods::listPcaMethods(), collapse = ', '), '], defaulting to svd'))
    pca_method = 'svd'
  }

  if ((pca_method %in% c('robustPca', 'nlpca', 'llsImpute'))){
    print(paste0(pca_method, ' is not currently supported, defaulting to svd'))
    pca_method = 'svd'
  }

  if (!(displayed_plots %in% c('both', 'loadings', 'scores', 'variance'))){
    print('Error: displayed_plots must be in both, loadings or scores')
    return()
  }

  if (max(c(displayed_pc_1, displayed_pc_2)) > nPcs) {
    print('At least one displayed PC outside of nPcs range, adjusting nPcs')
    nPcs = max(c(displayed_pc_1, displayed_pc_2))
  }

  if (displayed_pc_1 == displayed_pc_2) {
    print('displayed PCs are the same, defaulting to PC1 and PC2')
    displayed_pc_1 = 1
    displayed_pc_2 = 2
  }

  # Apply PCA
  pca_data = pcaMethods::pca(object = data_table,
                             method = pca_method,
                             nPcs = nPcs,
                             scale = "none",
                             cv = "q2",
                             completeObs = completeObs)

  # Plot depending on the type requested
  if (displayed_plots == 'both') {
    fig = c()
    fig[[1]] = plot_pca(x = pca_data@scores[,paste0('PC', displayed_pc_1)],
                        y = pca_data@scores[,paste0('PC', displayed_pc_2)],
                        label_1 = paste0('PC', displayed_pc_1),
                        label_2 = paste0('PC', displayed_pc_2),
                        weight_1 = round(pca_data@R2[displayed_pc_1], 3),
                        weight_2 = round(pca_data@R2[displayed_pc_2], 3),
                        names = rownames(data_table),
                        type = 'scores',
                        groups = sample_groups,
                        colors = colors_palette,
                        marker_size = 5,
                        width = width,
                        height = height)


    fig[[2]] = plot_pca(x = pca_data@loadings[, paste0('PC', displayed_pc_1)],
                        y = pca_data@loadings[, paste0('PC', displayed_pc_2)],
                        label_1 = paste0('PC', displayed_pc_1),
                        label_2 = paste0('PC', displayed_pc_2),
                        weight_1 = round(pca_data@R2[displayed_pc_1], 3),
                        weight_2 = round(pca_data@R2[displayed_pc_2], 3),
                        names = colnames(data_table),
                        type = 'loadings',
                        groups = feature_groups,
                        colors = colors_palette,
                        marker_size = 5,
                        width = width,
                        height = height)

    fig = plotly::subplot(fig, nrows = 1, margin = 0.035, titleX = T, titleY = T)

    fig = plotly::layout(fig, title = "PCA scores and loadings")


  } else if (displayed_plots == 'loadings'){

    fig = plot_pca(x = pca_data@loadings[, paste0('PC', displayed_pc_1)],
                   y = pca_data@loadings[, paste0('PC', displayed_pc_2)],
                   label_1 = paste0('PC', displayed_pc_1),
                   label_2 = paste0('PC', displayed_pc_2),
                   weight_1 = round(pca_data@R2[displayed_pc_1], 3),
                   weight_2 = round(pca_data@R2[displayed_pc_2], 3),
                   names = colnames(data_table),
                   type = 'loadings',
                   groups = feature_groups,
                   colors = colors_palette,
                   marker_size = 5,
                   width = width,
                   height = height)

  } else if (displayed_plots == 'scores') {

    fig = plot_pca(x = pca_data@scores[,paste0('PC', displayed_pc_1)],
                   y = pca_data@scores[,paste0('PC', displayed_pc_2)],
                   label_1 = paste0('PC', displayed_pc_1),
                   label_2 = paste0('PC', displayed_pc_2),
                   weight_1 = round(pca_data@R2[displayed_pc_1], 3),
                   weight_2 = round(pca_data@R2[displayed_pc_2], 3),
                   names = rownames(data_table),
                   type = 'scores',
                   groups = sample_groups,
                   colors = colors_palette,
                   marker_size = 5,
                   width = width,
                   height = height)
  } else if (displayed_plots == 'variance') {

    fig = plot_explained_variance(variance_explained = pca_data@R2,
                                  width = width,
                                  height = height)
  }

  if (return_data) {
    return(list(
      pca_data = pca_data,
      fig = fig))
  } else {
    return(fig)
  }


}


plot_pca = function(x, y, label_1, label_2, weight_1, weight_2, names, type, groups = NULL, colors = "Spectral", marker_size = 5, width = NULL, height = NULL) {

  if (!(type %in% c("scores", "loadings"))) {
    print('Error: type must either be scores or loadings.')
    return()
  }

  if (is.null(groups) & (type == "scores")) { # Score plot without groups (should not exist)
    data_table = data.frame(
      x = x,
      y = y,
      names = names
    )

    conf_ellipse = ellipse::ellipse(x = stats::cov(cbind(data_table$x, data_table$y)),
                                    centre = c(mean(data_table$x), mean(data_table$y)),
                                    level = 0.95)

    plot = plotly::plot_ly(data = data_table, width = width, height = height) %>%

      add_markers(
        x = ~x,
        y = ~y,
        text = ~names,
        mode = "markers+text",
        marker = list(size = marker_size),
      ) %>%

      add_trace(
        data = as.data.frame(conf_ellipse),
        x = ~x,
        y = ~y,
        mode = "lines",
        line = list(color = 'gray',
                    width = 1),
        inherit = FALSE,
        name = 'Hotelling',
        showlegend = TRUE,
        type = "scatter"
      ) %>%

      layout(
        title = "PCA Scores Plot",
        xaxis = list(title = paste0(label_1, ' (', weight_1 * 100, '%)'), zeroline = TRUE),
        yaxis = list(title = paste0(label_2, ' (', weight_2 * 100, '%)'), zeroline = TRUE)
      )

    return(plot)

  } else if (is.null(groups) & (type == "loadings")) { # Loadings plot without groups

    data_table = data.frame(
      x = x,
      y = y,
      names = names
    )

    plot = plot_ly(data = data_table, width = width, height = height) %>%

      add_segments(
        x = 0,
        y = 0,
        xend = ~x,
        yend = ~y,
        line = list(dash = "solid"),
        showlegend = FALSE
      ) %>%

      add_markers(
        x = ~x,
        y = ~y,
        text = ~names,
        mode = "markers+text",
        marker = list(size = marker_size),
        showlegend = FALSE
      ) %>%

      layout(
        title = "PCA Loadings Plot",
        xaxis = list(title = paste0(label_1, ' (', weight_1 * 100, '%)'), zeroline = TRUE),
        yaxis = list(title = paste0(label_2, ' (', weight_2 * 100, '%)'), zeroline = TRUE)
      )

    return(plot)

  } else if (!is.null(groups) & (type == "scores")) { # Score plot with groups

    data_table = data.frame(
      x = x,
      y = y,
      names = names,
      groups = as.factor(groups)
    )

    conf_ellipse = ellipse::ellipse(x = stats::cov(cbind(data_table$x, data_table$y)),
                                    centre = c(mean(data_table$x), mean(data_table$y)),
                                    level = 0.95)

    plot = plotly::plot_ly(data = data_table, width = width, height = height) %>%

    add_markers(
        x = ~x,
        y = ~y,
        text = ~names,
        mode = "markers+text",
        marker = list(size = marker_size),
        color = ~groups,
        colors = colors,
        legendgroup = ~groups,
        showlegend = TRUE
      ) %>%

      add_trace(
        data = as.data.frame(conf_ellipse),
        x = ~x,
        y = ~y,
        mode = "lines",
        line = list(color = 'gray',
                    width = 1),
        inherit = FALSE,
        name = 'Hotelling',
        showlegend = TRUE,
        type = "scatter"
      )%>%

      layout(
        title = "PCA Scores Plot",
        xaxis = list(title = paste0(label_1, ' (', weight_1 * 100, '%)'), zeroline = TRUE),
        yaxis = list(title = paste0(label_2, ' (', weight_2 * 100, '%)'), zeroline = TRUE)
      )

    return(plot)

  } else if (!is.null(groups) & (type == "loadings")) {

    data_table = data.frame(
      x = x,
      y = y,
      names = names,
      groups = as.factor(groups)
    )

    plot = plot_ly(data = data_table, width = width, height = height) %>%

      add_segments(
        x = 0,
        y = 0,
        xend = ~x,
        yend = ~y,
        line = list(dash = "solid"),
        color = ~groups,
        colors = colors,
        legendgroup = ~groups,
        showlegend = FALSE
      ) %>%

      add_markers(
        x = ~x,
        y = ~y,
        text = ~names,
        mode = "markers+text",
        marker = list(size = marker_size),
        color = ~groups,
        colors = colors,
        legendgroup = ~groups,
        showlegend = TRUE
      ) %>%

      layout(
        title = "PCA Loadings Plot",
        xaxis = list(title = paste0(label_1, ' (', weight_1 * 100, '%)'), zeroline = TRUE),
        yaxis = list(title = paste0(label_2, ' (', weight_2 * 100, '%)'), zeroline = TRUE)
      )

    return(plot)

  }
}

plot_explained_variance = function(variance_explained, width, height) {

  if (variance_explained[1] < 1) {
    variance_explained = variance_explained * 100
  }

  cumulative_variance = base::cumsum(variance_explained)

  plot = plotly::plot_ly(x = 1:length(variance_explained),
                         y = variance_explained,
                         type = 'bar',
                         name = 'Variance Explained',
                         marker = list(color = 'lightblue'),
                         width = width,
                         height = height)

  # Add line for cumulative variance
  plot = plot %>%
    add_trace(x = 1:length(variance_explained),
              y = cumulative_variance,
              type = 'scatter',
              mode = 'lines+markers',
              name = 'Cumulative Variance',
              line = list(color = 'red'),
              marker = list(color = 'red'))

  # Customize the layout
  plot = plot %>%
    layout(title = "Variance Explained by Each PC",
           xaxis = list(title = "Principal Component"),
           yaxis = list(title = "Variance Explained (%)", rangemode = "tozero"),
           barmode = 'overlay')

  return(plot)
}