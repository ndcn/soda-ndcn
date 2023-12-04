base::source('./R/utils.R')
base::source('./R/class_lips_exp.R')
base::source('./R/class_prot_exp.R')
base::source('./R/class_trns_exp.R')
base::source('./R/class_omics_exp.R')
base::source('./R/class_mofa_exp.R')
base::source('./R/class_snf_exp.R')
base::source('./R/complex_functions/pca.R')
base::source('./R/complex_functions/volcano.R')


#---------------------------------------------------- PROTEOMICS TEST APO-E ----

self = example_proteomics(name = 'prot_1',
                          data = 'D:/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_2.tsv',
                          meta = 'D:/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/metadata.csv')


self$add_feature_table(name = 'feat_1',
                       feature_file = 'D:/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/231023_feature_tables/proteomics_feat_annotation_clean.tsv')

self$derive_data_tables()

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


