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



self$plot_samples_correlation(color_palette = 'RdYlGn',
                              reverse_palette = T,
                              width = NULL,
                              height = NULL)
self$plots$samples_correlation


#------------------------------------------------ PROTEOMICS TEST CellMiner ----

self = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_data.csv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_meta.tsv',
                          param_file = './R/params/params_gene_based_omics.R')


self$add_feature_table(name = 'feat_1',
                       feature_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_feat_annotation_clean.tsv')



self$derive_data_tables()

self$plot_heatmap(row_annotations = "Group_type")
self$plots$heatmap

self$plot_pca()

data_table = 'Z-scored total normalized table'
meta_table = self$tables$raw_meta
feature_table = self$tables$feature_table
sample_groups_col = self$params$pca$sample_groups_col
feature_groups_col = self$params$pca$feature_groups_col
apply_da = T
alpha_da = self$params$pca$alpha_da
pca_method = self$params$pca$pca_method
nPcs = self$params$pca$nPcs
displayed_pc_1 = self$params$pca$displayed_pc_1
displayed_pc_2 = self$params$pca$displayed_pc_2
completeObs = self$params$pca$completeObs
displayed_plots = self$params$pca$displayed_plots
colors_palette = self$params$pca$colors_palette
return_data = TRUE
width = NULL
height = NULL


#---------------------------------------------------- PROTEOMICS TEST APO-E ----

self = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_2.tsv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/metadata.csv',
                          param_file = './R/params/params_gene_based_omics.R')


self$add_feature_table(name = 'feat_1',
                       feature_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_feat_annotation_clean.tsv')



self$derive_data_tables()


self$get_prot_list(context = 'ora')
self$over_representation_analysis(ont = 'ALL')

prot_list = self$tables$ora_prot_list
custom_col = NULL
feature_table = self$tables$feature_table
pval_cutoff_features = self$params$overrepresentation$pval_cutoff_features
padjust_features = self$params$overrepresentation$padjust_features
pval_cutoff = self$params$overrepresentation$pval_cutoff
pAdjustMethod = self$params$overrepresentation$pAdjustMethod
fc_threshold = self$params$overrepresentation$fc_threshold
keyType = self$indices$feature_id_type
ont = 'ALL'
qval_cutoff = self$params$overrepresentation$qval_cutoff
minGSSize = self$params$overrepresentation$minGSSize
maxGSSize  = self$params$overrepresentation$maxGSSize





go_enrich = clusterProfiler::enrichGO(gene = rownames(features),
                                      universe = universe,
                                      OrgDb = 'org.Hs.eg.db',
                                      keyType = keyType,
                                      readable = F,
                                      ont = ont,
                                      pvalueCutoff = pval_cutoff,
                                      pAdjustMethod = pAdjustMethod,
                                      qvalueCutoff = qval_cutoff,
                                      minGSSize = minGSSize,
                                      maxGSSize  = maxGSSize)


gene = rownames(features)
universe = universe
OrgDb = 'org.Hs.eg.db'
keyType = keyType
readable = T
ont = ont
pvalueCutoff = pval_cutoff
pAdjustMethod = pAdjustMethod
qvalueCutoff = qval_cutoff
minGSSize = minGSSize
maxGSSize  = maxGSSize

length(gene)
length(universe)

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


