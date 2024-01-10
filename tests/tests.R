base::source('./R/utils.R')
base::source('./R/class_omics_exp.R')
base::source('./R/class_mofa_exp.R')
base::source('./R/class_snf_exp.R')
base::source('./R/complex_functions/pca.R')
base::source('./R/complex_functions/volcano.R')

#---------------------------------------------------- LIPIDOMICS TEST APO-E ----

self = example_lipidomics(name = 'lips_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/lipidomics.csv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/lipidomics_metadata.csv',
                          param_file = './R/params/params_lipidomics.R')


self$derive_data_tables()


data_table = 'Class table z-scored'
impute = self$params$heatmap$impute
meta_table = self$tables$raw_meta
meta_table_features = self$tables$feature_table
cluster_rows = self$params$heatmap$cluster_samples
cluster_cols = self$params$heatmap$cluster_features
row_annotations = self$params$heatmap$map_sample_data
col_annotations = self$params$heatmap$map_feature_data
map_feature_terms = self$params$heatmap$map_feature_terms
apply_da = self$params$heatmap$apply_da
group_column_da = self$params$heatmap$group_column_da
alpha_da = self$params$heatmap$alpha_da
color_palette = self$params$heatmap$color_palette
reverse_palette = self$params$heatmap$reverse_palette
width = NULL
height = NULL


self$plot_samples_correlation(data_table = self$tables$z_scored_total_norm_data,
                              impute = self$params$samples_correlation$impute,
                              meta_table = self$tables$raw_meta,
                              correlation_method = self$params$samples_correlation$correlation_method,
                              use = self$params$samples_correlation$use,
                              cluster_cols = self$params$samples_correlation$cluster_cols,
                              cluster_rows = self$params$samples_correlation$cluster_rows,
                              row_annotations = self$params$samples_correlation$row_annotations,
                              col_annotations = self$params$samples_correlation$col_annotations,
                              color_palette = 'RdYlGn',
                              reverse_palette = T,
                              width = NULL,
                              height = NULL)
self$plots$samples_correlation

data_table = self$tables$z_scored_total_norm_data


samples_correlation = list(
  auto_refresh = T,
  dataset = 'Z-scored total normalized table',
  impute = T,
  correlation_method = "pearson",
  use = 'pairwise.complete.obs',
  cluster_cols = T,
  cluster_rows = T,
  row_annotations = 'Group_type',
  col_annotations = 'Group_type',
  color_palette = 'RdYlBu',
  reverse_palette = F,
  img_format = "png"
)

data_table = self$tables$z_scored_total_norm_data
meta_table = self$tables$raw_meta
auto_refresh = T
dataset = 'Z-scored total normalized table'
impute = T
correlation_method = "pearson"
use = 'pairwise.complete.obs'
cluster_cols = T
cluster_rows = T
row_annotations = NULL
col_annotations = NULL
color_palette = 'RdYlBu'
reverse_palette = F
img_format = "png"

## Sample correlation plot
plot_samples_correlation = function(data_table = self$tables$z_scored_total_norm_data,
                        impute = self$params$samples_correlation$impute,
                        meta_table = self$tables$raw_meta,
                        correlation_method = self$params$samples_correlation$correlation_method,
                        use = self$params$samples_correlation$use,
                        cluster_cols = self$params$samples_correlation$cluster_cols,
                        cluster_rows = self$params$samples_correlation$cluster_rows,
                        row_annotations = self$params$samples_correlation$row_annotations,
                        col_annotations = self$params$samples_correlation$col_annotations,
                        color_palette = self$params$samples_correlation$color_palette,
                        reverse_palette = self$params$samples_correlation$reverse_palette,
                        width = NULL,
                        height = NULL) {



  # Set the clustering
  if (cluster_rows & cluster_cols) {
    dendrogram_list = "both"
  } else if (cluster_rows) {
    dendrogram_list = "column" # Because of the transpose, rows => cols
  } else if (cluster_cols) {
    dendrogram_list = "row" # Because of the transpose, cols => rows
  } else {
    dendrogram_list = "none"
  }

  data_table = stats::cor(x = t(data_table),
                          y = NULL,
                          use = use,
                          method = correlation_method)

  # diag(data_table) = 0

  # Set zmax and zmin
  val_list = as.vector(data_table)
  val_list = na.omit(val_list)
  val_list = sort(val_list)

  zmax = min(c(abs(min(val_list)), max(val_list)))
  zmin = -zmax

  # Filter out the data
  data_table[data_table > zmax] = zmax
  data_table[data_table < zmin] = zmin

  # Annotations
  if (!is.null(row_annotations)) {
    if (length(row_annotations) > 1) {
      row_annotations = meta_table[, row_annotations]
      colnames(row_annotations) = stringr::str_replace_all(colnames(row_annotations), "_", " ")
    } else {
      row_names = row_annotations
      row_annotations = as.data.frame(meta_table[, row_annotations],
                                      row.names = rownames(meta_table))
      colnames(row_annotations) = stringr::str_replace_all(row_names, "_", " ")
    }
  }

  if (!is.null(col_annotations)) {
    if (length(col_annotations) > 1) {
      col_annotations = meta_table[, col_annotations]
      colnames(col_annotations) = stringr::str_replace_all(colnames(col_annotations), "_", " ")
    } else {
      row_names = col_annotations
      col_annotations = as.data.frame(meta_table[, col_annotations],
                                      row.names = rownames(meta_table))
      colnames(col_annotations) = stringr::str_replace_all(row_names, "_", " ")
    }
  }




  # Save table as heatmap table
  self$tables$samples_correlation = data_table

  # Get the color palette
  color_count = colors_switch(color_palette)
  color_palette = RColorBrewer::brewer.pal(color_count, color_palette)
  color_palette = c(color_palette[1], color_palette[round(color_count/2)] , color_palette[color_count])
  if (reverse_palette) {
    color_palette = base::rev(color_palette)
  }

  # Plot the data
  self$plots$samples_correlation = heatmaply::heatmaply(x = t(data_table),
                                                        scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                                                          low = color_palette[3],
                                                          mid = color_palette[2],
                                                          high = color_palette[1],
                                                          midpoint = 0,
                                                          limits = c(zmin, zmax)
                                                        ),
                                                        width = width,
                                                        height = height,
                                                        limits = c(zmin, zmax),
                                                        col_side_colors = row_annotations,
                                                        row_side_colors = col_annotations,
                                                        dendrogram = dendrogram_list)

},

roh_lim = 0.85
diag(data_table) = 0
max_abs_values = apply(data_table, 1, function(x) max(abs(x), na.rm = T))
roh_filter = unname(which(max_abs_values >= roh_lim))
length(roh_filter)
length(max_abs_values)
data_table = data_table[roh_filter, roh_filter]



correlation_method = c("pearson", "kendall", "spearman")[3]
use = 'pairwise.complete.obs' # c("everything", "all.obs", "complete.obs", "na.or.complete", or "pairwise.complete.obs")
roh_lim = 0.95

correlation_data = stats::cor(x = t(data_table),
                              y = NULL,
                              use = use,
                              method = correlation_method)

correlation_data = stats::cor(x = data_table,
                              y = NULL,
                              use = use,
                              method = correlation_method)
diag(correlation_data) = 0
max_abs_values = apply(correlation_data, 1, function(x) max(abs(x), na.rm = T))
roh_filter = unname(which(max_abs_values >= roh_lim))
length(roh_filter)
length(max_abs_values)
correlation_data = correlation_data[roh_filter, roh_filter]

library(corrplot)
library(pheatmap)
corrplot(correlation_data, method = "color")
pheatmap(correlation_data)




#---------------------------------------------------- PROTEOMICS TEST APO-E ----

self = example_proteomics(name = 'prot_1',
                          data = 'D:/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_2.tsv',
                          meta = 'D:/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/metadata.csv',
                          param_file = './R/params/params_gene_based_omics.R')


self$hardcoded_settings$volcano_plot$datasets

self$add_feature_table(name = 'feat_1',
                       feature_file = 'D:/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/231023_feature_tables/proteomics_feat_annotation_clean.tsv')



self$derive_data_tables()

self$hardcoded_settings$enrichment_analysis$terms
self$hardcoded_settings$enrichment_analysis$adjustment

#---- Save params method ----




names(params_list)
params_list$class_comparison
#---- Save params method ----

auto_refresh = T
data_table = self$tables$z_scored_total_norm_data
meta_table = self$tables$feature_table
dataset = 'Z-scored total normalized table'
map_feature_terms = NULL
correlation_method = "pearson"
use = 'pairwise.complete.obs'
cluster_cols = T
cluster_rows = T
row_annotations = NULL
col_annotations = NULL
roh_threshold = 0.80
top_features = 300
color_palette = 'RdYlBu'
reverse_palette = F
img_format = "png"

self$plot_feature_correlation(roh_threshold = 0.95)
self$plots$feature_correlation

self$plot_samples_correlation(data_table = self$tables$z_scored_total_norm_data,
                              impute = self$params$samples_correlation$impute,
                              meta_table = self$tables$raw_meta,
                              correlation_method = self$params$samples_correlation$correlation_method,
                              use = self$params$samples_correlation$use,
                              cluster_cols = self$params$samples_correlation$cluster_cols,
                              cluster_rows = self$params$samples_correlation$cluster_rows,
                              row_annotations = self$params$samples_correlation$row_annotations,
                              col_annotations = self$params$samples_correlation$col_annotations,
                              color_palette = 'RdYlGn',
                              reverse_palette = T,
                              width = NULL,
                              height = NULL)



self$plot_heatmap()
self$plots$heatmap

features = c('GREM1', 'MICAL2', 'RUNX2', 'BOP1', 'PYCR1', 'TBL3', 'CRMP1', 'IGFBP4', 'ITGA11',
             'SSSCA1', 'ARHGAP29', 'ALDH1B1', 'PUS7L', 'APOBEC3C', 'MAPK12', 'CPA4', 'DPYSL3',
             'CDC42EP3', 'TGFBR1', 'CCNK', 'UAP1', 'EDIL3', 'ARHGAP18', 'LTBP2', 'SMTN', 'IGFBP7', 'KIF4A',
             'SLC9A3R2', 'CACNA2D1', 'DAPK1', 'JUP', 'HYOU1', 'MLLT11', 'PDLIM7', 'TOM1L1', 'HSPA4L',
             'GPN1', 'KIAA0754', 'PACSIN3', 'MYLK')

names(self$tables$feature_list)

best_matches = colSums(self$tables$feature_list[['Gene Ontology (cellular component)']]$sparse_matrix[features,])
best_matches = sort(best_matches)
best_matches = best_matches[best_matches > 1]
max(best_matches)
table(best_matches)

# Gene Ontology (cellular component) : cytosol [GO:0005829], plasma membrane [GO:0005886], nucleoplasm [GO:0005654], extracellular space [GO:0005615]
names(best_matches[best_matches > 4])

matrix = as.matrix(self$tables$feature_list$`Gene Ontology (biological process)`$sparse_matrix)
rownames(matrix)
as.matrix()

matrix[c('GREM1', 'MICAL2', 'RUNX2', 'BOP1', 'PYCR1'),]


#------------------------------------------------ PROTEOMICS TEST CELLMINER ----

self = example_proteomics(name = 'prot_1',
                          data = 'D:/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_data.csv',
                          meta = 'D:/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_meta.tsv')

self$derive_data_tables()


