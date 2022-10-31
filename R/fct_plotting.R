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
#'
#' @noRd
#'
plot_classes <- function(l3 = NULL,
                         meta_s = NULL,
                         fill1 = "Genotype") {

  plot_data <- aggregate(x = l3$value,
                         by = list(l3$class, l3[, fill1]),
                         # correct the sum by the number of samples per group
                         FUN = function(x) mean(x, na.rm = TRUE),
                         drop = FALSE)

  colnames(plot_data) <- c("class", fill1, "value")

  ggbar_lipid_class_per_genotype_per_cellnumber <- ggplot2::ggplot(
    data = plot_data,
    ggplot2::aes_string(x = "class",
                        y = "value",
                        fill = fill1)
  ) +
    ggplot2::geom_col(position = "dodge") +
    # ggplot2::geom_bar(position = "dodge",
    #                   stat = "summary") +
    ggplot2::ylab("")+
    ggplot2::theme_bw()

  return(ggbar_lipid_class_per_genotype_per_cellnumber)
}



#' @title Facetted bar plot showing the class distribution and samples
#'
#' @description Facetted bar plot showing the class distribution and samples.
#'
#' @param l4_2 data
#' @param class_norm type of normalisation
#' @param meta_s the meta data
#' @param compare1 what comparison
#'
#' @return a ggplot2 object.
#'
#' @author Yassene Mohammed
#' @author Rico Derks
#'
#' @importFrom ggplot2 ggplot aes_string geom_bar geom_point facet_wrap ylab
#'     theme_bw theme element_blank unit element_text
#'
#' @noRd
#'
plot_compare_classes <- function(l4_2 = NULL,
                                 meta_s = NULL,
                                 compare1 = "Genotype") {


  ggbar_class <- ggplot2::ggplot(data = l4_2,
                                 ggplot2::aes_string(x = compare1,
                                                     y = "value",
                                                     fill = compare1)) +
    ggplot2::geom_bar(position = "dodge",
                      stat = "summary",
                      fun.y = "mean_se")+
    ggplot2::geom_boxplot(width = 0.2) +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(~ class,
                        ncol = 4,
                        scales = "free")+
    ggplot2::ylab("percentage of total lipid")+
    ggplot2::theme_bw()+
    ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   legend.key.size = ggplot2::unit(1, "cm"), #change legend key size
                   legend.key.height = ggplot2::unit(1, "cm"), #change legend key height
                   legend.key.width = ggplot2::unit(1, "cm"), #change legend key width
                   legend.title = ggplot2::element_text(size = 14), #change legend title font size
                   legend.text = ggplot2::element_text(size = 10), #change legend text font size
                   legend.position = "none")

  return(ggbar_class)
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

