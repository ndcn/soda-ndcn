base::source('./R/utils.R')
base::source('./R/class_omics_exp.R')
base::source('./R/class_mofa_exp.R')
base::source('./R/class_snf_exp.R')
base::source('./R/complex_functions/pca.R')
base::source('./R/complex_functions/volcano.R')

#------------------------------------------------------ MOFA TEST CELLMINER ----
prot_1 = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_data.csv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_meta.tsv',
                          param_file = './R/params/params_gene_based_omics.R')

trns_1 = example_transcriptomics(name = 'trns_1',
                               data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/rna/rna_data.csv',
                               meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/rna/rna_meta.tsv',
                               param_file = './R/params/params_gene_based_omics.R')


#------------------------------------------------------- SNF TEST CELLMINER ----

prot_1 = example_proteomics(name = 'prot_1',
                            data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_data.csv',
                            meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/protein_swath/prot_meta.tsv',
                            param_file = './R/params/params_gene_based_omics.R')

trns_1 = example_transcriptomics(name = 'trns_1',
                                 data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/rna/rna_data.csv',
                                 meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/rna/rna_meta.tsv',
                                 param_file = './R/params/params_gene_based_omics.R')

geno_1 = example_transcriptomics(name = 'geno_1',
                                 data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/dna_data.csv',
                                 meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/cellminer_data/sample_annotations.tsv',
                                 param_file = './R/params/params_gene_based_omics.R')

snf_data = Snf_data$new(
  name = "snf_1"
)

snf_data$tables$metadata = NULL
snf_data$tables$omics_tables = list()

snf_data$add_meta(prot_1$tables$raw_meta)

snf_data$add_data(name = prot_1$name,
            data_table = prot_1$tables$z_scored_data)

snf_data$add_data(name = trns_1$name,
                  data_table = trns_1$tables$z_scored_data)

snf_data$add_data(name = geno_1$name,
                  data_table = geno_1$tables$z_scored_data)

print(names(snf_data$tables$omics_tables))

# Single omics similarity network
snf_data$param_similarity_network(data_table = 'prot_1',
                                  sample_groups = 'Group_type',
                                  color_palette = 'Paired',
                                  K1 = 5,
                                  sigma = 0.5,
                                  K2 = 3,
                                  legend = F,
                                  context = 'similarity_network_1')

snf_data$plot_similarity_network(context = 'similarity_network_1')
snf_data$plots$similarity_network_1

# Fusion similarity network

sample_groups = snf_data$params$similarity_network_fusion$sample_groups
color_palette = snf_data$params$similarity_network_fusion$color_palette
meta_table = snf_data$tables$metadata
K1 = snf_data$params$similarity_network_fusion$K1
sigma = snf_data$params$similarity_network_fusion$sigma
K2 = snf_data$params$similarity_network_fusion$K2
Wall = snf_data$params$similarity_network_fusion$Wall
K3 = snf_data$params$similarity_network_fusion$K3
t = snf_data$params$similarity_network_fusion$t
legend = snf_data$params$similarity_network_fusion$legend


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

#------------------------------------------- TRANSCRIPTOMICS TEST CellMiner ----

self = example_transcriptomics(name = 'trns_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/rna/rna_data.csv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230927_Cellminer_data/rna/rna_meta.tsv',
                          param_file = './R/params/params_gene_based_omics.R')


self$add_feature_table(name = 'feat_1',
                       feature_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_feat_annotation_clean.tsv')



self$derive_data_tables()

#---------------------------------------------------- PROTEOMICS TEST APO-E ----

self = example_proteomics(name = 'prot_1',
                          data = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_2.tsv',
                          meta = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/metadata.csv',
                          param_file = './R/params/params_gene_based_omics.R')


self$add_feature_table(name = 'feat_1',
                       feature_file = '/home/dolivierj/Dropbox/1_Travail/221219_lumc/230828_dmc_soda/test_data/230828_multiomics_1/proteomics_feat_annotation_clean.tsv')



self$derive_data_tables()



