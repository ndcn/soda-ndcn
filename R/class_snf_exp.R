#---------------------------- Class Snf_data ---------------------------------
Snf_data = R6::R6Class(
  "Snf_exp",
  public = list(

    initialize = function(name = NA){
      self$name = name
    },

    #--------------------------------------------------------------- Global ----
    name = NULL,
    type = NULL,

    #----------------------------------------------------------- Parameters ----
    params = list(

      clusters_heatmap_1 = list(
        data_table = NULL,
        K1 = 5,
        sigma = 0.5,
        K2 = 3,
        vertical_annotations = NULL,
        horizontal_annotations = NULL,
        img_format = 'png'
      ),
      clusters_heatmap_2 = list(
        data_table = NULL,
        K1 = 5,
        sigma = 0.5,
        K2 = 3,
        vertical_annotations = NULL,
        horizontal_annotations = NULL,
        img_format = 'png'
      ),

      similarity_network_1 = list(
        data_table = NULL,
        sample_groups = "Group_type",
        color_palette = "Spectral",
        K1 = 5,
        sigma = 0.5,
        K2 = 3,
        legend = TRUE
      ),

      similarity_network_2 = list(
        data_table = NULL,
        sample_groups = "Group_type",
        color_palette = "Spectral",
        K1 = 5,
        sigma = 0.5,
        K2 = 3,
        legend = TRUE
      ),

      fusion_heatmap = list(
        K1 = 5,
        sigma = 0.5,
        K2 =  3,
        Wall = NULL,
        K3 = 3,
        t = 5,
        vertical_annotations = NULL,
        horizontal_annotations = NULL,
        img_format = 'png'
      ),

      similarity_network_fusion = list(
        K1 = 5,
        sigma = 0.5,
        K2 = 3,
        Wall = NULL,
        K3 = 3,
        t = 5
      )

    ),

    #---------------------------------------------------- Parameter methods ----
    param_cluster_heatmap = function(data_table, K1, sigma, K2, vertical_annotations, horizontal_annotations, img_format, context) {
      self$params[[context]]$data_table = data_table
      self$params[[context]]$K1 = K1
      self$params[[context]]$sigma = sigma
      self$params[[context]]$K2 = K2
      self$params[[context]]$vertical_annotations = vertical_annotations
      self$params[[context]]$horizontal_annotations = horizontal_annotations
      self$params[[context]]$img_format = img_format

    },

    param_similarity_network = function(data_table, sample_groups, color_palette, K1, sigma, K2, legend, context) {
      self$params[[context]]$data_table = data_table
      self$params[[context]]$sample_groups = sample_groups
      self$params[[context]]$color_palette = color_palette
      self$params[[context]]$K1 = K1
      self$params[[context]]$sigma = sigma
      self$params[[context]]$K2 = K2
      self$params[[context]]$legend = legend
    },

    param_fusion_heatmap = function(K1, sigma, K2, Wall, K3, t, vertical_annotations, horizontal_annotations, img_format) {
      self$params$fusion_heatmap$K1 = K1
      self$params$fusion_heatmap$sigma = sigma
      self$params$fusion_heatmap$K2 = K2
      self$params$fusion_heatmap$Wall = Wall
      self$params$fusion_heatmap$K3 = K3
      self$params$fusion_heatmap$t = t
      self$params$fusion_heatmap$vertical_annotations = vertical_annotations
      self$params$fusion_heatmap$horizontal_annotations = horizontal_annotations
      self$params$fusion_heatmap$img_format = img_format
    },

    param_similarity_network_fusion = function(K1, sigma, K2, Wall, K3, t) {
      self$params$similarity_network_fusion$K1 = K1
      self$params$similarity_network_fusion$sigma = sigma
      self$params$similarity_network_fusion$K2 = K2
      self$params$similarity_network_fusion$Wall = Wall
      self$params$similarity_network_fusion$K3 = K3
      self$params$similarity_network_fusion$t = t
    },

    #---------------------------------------------------------------- Plots ----
    plots = list(
      clusters_heatmap_1 = NULL,
      clusters_heatmap_2 = NULL,
      similarity_network_1 = NULL,
      similarity_network_2 = NULL,
      fusion_heatmap = NULL,
      similarity_network_fusion = NULL
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(
      metadata = NULL,
      omics_tables = list(),
      clusters_heatmap_1 = NULL,
      clusters_heatmap_2 = NULL,
      similarity_network_1 = NULL,
      similarity_network_2 = NULL,
      fusion_heatmap = NULL,
      similarity_network_fusion = NULL
    ),

    #-------------------------------------------------------- Table methods ----

    add_data = function(name, data_table) {
      self$tables$omics_tables[[name]] = data_table
    },

    add_meta = function(meta_table) {
      self$tables$metadata = meta_table
    },

    #----------------------------------------------------- Plotting methods ----

    plot_clusters_heatmap = function(data_table = self$params$clusters_heatmap_1$data_table,
                                     meta_table = self$tables$metadata,
                                     K1 = 5,
                                     sigma = 0.5,
                                     K2 = 3,
                                     vertical_annotations = NULL,
                                     horizontal_annotations = NULL,
                                     context = 'clusters_heatmap_1') {

      if (is.null(data_table)) {
        print('ERROR: no default data table for clusters heatmaps')
        return()
      } else {
        data_table = self$tables$omics_tables[[data_table]]
        if (is.null(data_table)) {
          print('ERROR: undefined data table.')
          return()
        }
      }

      K1 = as.numeric(K1)
      K2 = as.numeric(K2)
      sigma = as.numeric(sigma)

      # Get the affinity matrix
      data_table = base::as.matrix(stats::dist(data_table))
      data_table = SNFtool::affinityMatrix(data_table, K = K1, sigma = sigma)

      # Set diagonal to 0
      base::diag(data_table) = NA
      base::diag(data_table) = min(data_table, na.rm = T)

      # Change samplenames if coercible to numeric (plotly requirement)
      if (is_coercible_to_numeric(rownames(data_table))) {
        rownames(data_table) = paste0('X',rownames(data_table))
        colnames(data_table) = paste0('X',colnames(data_table))
      }

      order_by_cluster = base::order(SNFtool::spectralClustering(data_table, K = K2))
      data_table = data_table[order_by_cluster,order_by_cluster]

      # Annotations
      meta_table = meta_table[order_by_cluster,]

      if (!is.null(vertical_annotations)) {
        if (length(vertical_annotations) > 1) {
          vertical_annotations = meta_table[, vertical_annotations]
          colnames(vertical_annotations) = stringr::str_replace_all(colnames(vertical_annotations), "_", " ")
        } else {
          row_names = vertical_annotations
          vertical_annotations = as.data.frame(meta_table[, vertical_annotations],
                                               row.names = rownames(meta_table))
          colnames(vertical_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      if (!is.null(horizontal_annotations)) {
        if (length(horizontal_annotations) > 1) {
          horizontal_annotations = meta_table[, horizontal_annotations]
          colnames(horizontal_annotations) = stringr::str_replace_all(colnames(horizontal_annotations), "_", " ")
        } else {
          row_names = horizontal_annotations
          horizontal_annotations = as.data.frame(meta_table[, horizontal_annotations],
                                                 row.names = rownames(meta_table))
          colnames(horizontal_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      fig = heatmaply::heatmaply(data_table,
                                 dendrogram = 'none',
                                 Rowv = F,
                                 Colv = F,
                                 trace = 'none',
                                 col_side_colors = vertical_annotations,
                                 row_side_colors = horizontal_annotations
                                 )

      self$tables[[context]] = data_table
      self$plots[[context]] = fig
    },

    plot_fusion_heatmap = function(meta_table = self$tables$metadata,
                        K1 = self$params$fusion_heatmap$K1,
                        sigma = self$params$fusion_heatmap$sigma,
                        K2 = self$params$fusion_heatmap$K2,
                        Wall = self$params$fusion_heatmap$Wall,
                        K3 = self$params$fusion_heatmap$K3,
                        t = self$params$fusion_heatmap$t,
                        vertical_annotations = self$params$fusion_heatmap$vertical_annotations,
                        horizontal_annotations = self$params$fusion_heatmap$horizontal_annotations
                        ) {

      if (is.null(Wall)) {
        Wall = names(self$tables$omics_tables)
      }
      K1 = as.numeric(K1)
      K2 = as.numeric(K2)
      K3 = as.numeric(K3)
      sigma = as.numeric(sigma)
      t = as.numeric(t)

      matrix_list = list()
      for (w in Wall){
        affinity_matrix = self$tables$omics_tables[[w]]
        affinity_matrix = base::as.matrix(stats::dist(affinity_matrix))
        affinity_matrix = SNFtool::affinityMatrix(affinity_matrix, K = K1, sigma = sigma)
        matrix_list[[w]] = affinity_matrix
      }
      data_table = SNFtool::SNF(Wall = matrix_list, K = K2, t = t)
      base::diag(data_table) = NA
      base::diag(data_table) = min(data_table, na.rm = T)
      clusters = SNFtool::spectralClustering(data_table, K = K3)
      order_by_cluster = base::order(clusters)
      data_table = data_table[order_by_cluster,order_by_cluster]

      # Annotations
      meta_table = meta_table[order_by_cluster,]

      if (!is.null(vertical_annotations)) {
        if (length(vertical_annotations) > 1) {
          vertical_annotations = meta_table[, vertical_annotations]
          colnames(vertical_annotations) = stringr::str_replace_all(colnames(vertical_annotations), "_", " ")
        } else {
          row_names = vertical_annotations
          vertical_annotations = as.data.frame(meta_table[, vertical_annotations],
                                               row.names = rownames(meta_table))
          colnames(vertical_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      if (!is.null(horizontal_annotations)) {
        if (length(horizontal_annotations) > 1) {
          horizontal_annotations = meta_table[, horizontal_annotations]
          colnames(horizontal_annotations) = stringr::str_replace_all(colnames(horizontal_annotations), "_", " ")
        } else {
          row_names = horizontal_annotations
          horizontal_annotations = as.data.frame(meta_table[, horizontal_annotations],
                                                 row.names = rownames(meta_table))
          colnames(horizontal_annotations) = stringr::str_replace_all(row_names, "_", " ")
        }
      }

      fig = heatmaply::heatmaply(data_table,
                                 dendrogram = 'none',
                                 Rowv = F,
                                 Colv = F,
                                 trace = 'none',
                                 col_side_colors = vertical_annotations,
                                 row_side_colors = horizontal_annotations
                                 )
      self$tables$fusion_heatmap = data_table
      self$plots$fusion_heatmap = fig
    },


    plot_similarity_network = function(meta_table = self$tables$metadata,
                                       context = 'similarity_network_1') {

      # Load parameters
      data_table = self$params[[context]]$data_table
      sample_groups = self$params[[context]]$sample_groups
      color_palette = self$params[[context]]$color_palette
      K1 = self$params[[context]]$K1
      sigma = self$params[[context]]$sigma
      K2 = self$params[[context]]$K2
      legend = self$params[[context]]$legend

      if (is.null(data_table)) {
        print('ERROR: no default data table for clusters heatmaps')
        return()
      } else {
        data_table = self$tables$omics_tables[[data_table]]
        if (is.null(data_table)) {
          print('ERROR: undefined data table.')
          return()
        }
      }

      K1 = as.numeric(K1)
      K2 = as.numeric(K2)
      sigma = as.numeric(sigma)

      # Get the affinity matrix
      data_table = base::as.matrix(stats::dist(data_table))
      data_table = SNFtool::affinityMatrix(data_table, K = K1, sigma = sigma)

      network_table = igraph::graph_from_adjacency_matrix(adjmatrix = data_table,
                                                          weighted= TRUE,
                                                          mode="undirected",
                                                          diag=F)
      network_table = igraph::simplify(network_table)
      network_table = igraph::mst(network_table)
      network_table = base::as.data.frame(igraph::get.edgelist(network_table))

      # Create the node table with coloring
      nodes = data.frame(name = unique(c(network_table$V1, network_table$V2)))
      nodes$group = meta_table[rownames(data_table), sample_groups]

      color_count = colors_switch(color_palette)
      color_palette = RColorBrewer::brewer.pal(color_count, color_palette)
      color_palette = colorRampPalette(color_palette)(length(unique(nodes$group)))
      color_mapping = setNames(color_palette, unique(nodes$group))

      # Format for forceNetwork
      links = network_table
      links$source = match(links$V1, nodes$name) - 1
      links$target = match(links$V2, nodes$name) - 1

      # JavaScript function for colourScale
      js_colourScale = base::sprintf("d3.scaleOrdinal().domain(%s).range(%s)",
                                     jsonlite::toJSON(names(color_mapping)),
                                     jsonlite::toJSON(color_mapping))

      fig = networkD3::forceNetwork(Links = links,
                                    Nodes = nodes,
                                    Source = "source",
                                    Target = "target",
                                    NodeID = "name",
                                    Group = "group",
                                    linkWidth = 1,
                                    opacity = 1,
                                    legend = legend,
                                    zoom = TRUE,
                                    opacityNoHover = 1,
                                    colourScale = networkD3::JS(js_colourScale))


      self$tables[[context]]$edge_table = links
      self$tables[[context]]$node_table = nodes
      self$plots[[context]] = fig
    },

    plot_similarity_network_fusion = function(meta_table = self$tables$metadata,
                                              K1 = self$params$similarity_network_fusion$K1,
                                              sigma = self$params$similarity_network_fusion$sigma,
                                              K2 = self$params$similarity_network_fusion$K2,
                                              Wall = self$params$similarity_network_fusion$Wall,
                                              K3 = self$params$similarity_network_fusion$K3,
                                              t = self$params$similarity_network_fusion$t
    ) {

      if (is.null(Wall)) {
        Wall = names(self$tables$omics_tables)
      }
      K1 = as.numeric(K1)
      K2 = as.numeric(K2)
      K3 = as.numeric(K3)
      sigma = as.numeric(sigma)
      t = as.numeric(t)

      matrix_list = list()
      for (w in Wall){
        affinity_matrix = self$tables$omics_tables[[w]]
        affinity_matrix = base::as.matrix(stats::dist(affinity_matrix))
        affinity_matrix = SNFtool::affinityMatrix(affinity_matrix, K = K1, sigma = sigma)
        matrix_list[[w]] = affinity_matrix
      }
      data_table = SNFtool::SNF(Wall = matrix_list, K = K2, t = t)

      network_table = igraph::graph_from_adjacency_matrix(adjmatrix = data_table,
                                                          weighted= TRUE,
                                                          mode="undirected",
                                                          diag=F)
      network_table = igraph::simplify(network_table)
      network_table = igraph::mst(network_table)
      network_table = base::as.data.frame(igraph::get.edgelist(network_table))
      fig = networkD3::simpleNetwork(network_table, zoom = T)

      self$tables$similarity_network_fusion = network_table
      self$plots$similarity_network_fusion = fig

    }

  )
)