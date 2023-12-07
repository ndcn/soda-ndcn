list(params = list(class_distribution = list(
    dataset = "Class table total normalized", group_col = "Group_type", 
    img_format = "png"), class_comparison = list(dataset = "Class table total normalized", 
    group_col = "Group_type", img_format = "png"), volcano_plot = list(
    auto_refresh = TRUE, data_table = "Total normalized table", 
    adjustment = "BH", group_col = "Group_type", group_1 = "ApoE3/3", 
    group_2 = "null", feature_metadata = "None", keep_significant = FALSE, 
    displayed_plot = "main", p_val_threshold = 0.05, fc_threshold = 2, 
    marker_size = 6, opacity = 1, color_palette = "Spectral", 
    selected_function = "mean", selected_test = "t-Test", img_format = "png"), 
    heatmap = list(auto_refresh = TRUE, dataset = "Z-scored total normalized table", 
        impute = TRUE, cluster_samples = TRUE, cluster_features = TRUE, 
        multival_cols = "None", group_column_da = "Group_type", 
        apply_da = TRUE, alpha_da = 0.8, lock_da = TRUE, color_palette = "RdYlBu", 
        reverse_palette = FALSE, img_format = "png"), samples_correlation = list(
        auto_refresh = TRUE, dataset = "Z-scored total normalized table", 
        correlation_method = "pearson", use = "pairwise.complete.obs", 
        cluster_cols = TRUE, cluster_rows = TRUE, row_annotations = "Group_type", 
        col_annotations = "Group_type", color_palette = "RdYlBu", 
        reverse_palette = FALSE, img_format = "png"), feature_correlation = list(
        auto_refresh = TRUE, dataset = "Z-scored total normalized table", 
        multival_cols = "None", correlation_method = "pearson", 
        use = "pairwise.complete.obs", cluster_cols = TRUE, cluster_rows = TRUE, 
        roh_threshold = 0.95, top_features = 300, color_palette = "RdYlBu", 
        reverse_palette = FALSE, img_format = "png"), pca = list(
        auto_refresh = TRUE, data_table = "z_scored_total_norm_data", 
        sample_groups_col = "Group_type", apply_da = FALSE, alpha_da = 0.8, 
        pca_method = "svd", nPcs = 10, displayed_pc_1 = 1, displayed_pc_2 = 2, 
        completeObs = FALSE, displayed_plots = "both", colors_palette = "Spectral", 
        img_format = "png"), db_plot = list(dataset = "Total normalized table", 
        adjustment = "Benjamini-Hochberg", group_column = "Group_type", 
        selected_groups = c("ApoE3/3", "null"), selected_carbon_chain = "Carbon count (chain 1)", 
        selected_unsat = "Double bonds (chain 1)", selected_function = "median", 
        selected_test = "t-Test", fc_range = c(-5, 5), fc_values = c(-1, 
        1), pval_range = c(0, 5), pval_values = c(1, 5), img_format = "png"), 
    gsea = list(data_table = "Raw data table", meta_table = "Raw metadata table", 
        group_col = "Group_type", groups = c("ApoE3/3", "null"
        ), used_function = "median", test = "t-Test", p_value_cutoff_prep = 0.05, 
        prot_list = "GSEA prot list", ont = "Gene ontology (ALL)", 
        minGSSize = 3, maxGSSize = 800, p_value_cutoff = 0.05, 
        verbose = TRUE, OrgDb = "org.Hs.eg.db", pAdjustMethod = "BH", 
        termsim_method = "JC", termsim_showcat = 200), overrepresentation = list(
        pval_cutoff_features = 0.05, padjust_features = "Benjamini-Hochberg", 
        pval_cutoff = 0.05, pAdjustMethod = "BH", fc_threshold = 2, 
        ont = "Gene ontology (ALL)", qval_cutoff = 0.05, minGSSize = 10, 
        maxGSSize = 500), dot_plot = list(showCategory = 10, 
        mode = "Both", img_format = "png"), ridge_plot = list(
        showCategory = 30, img_format = "png"), cnet_plot = list(
        showCategory = 3, displayed_labels = "IDs and Description", 
        enable_physics = TRUE), emap_plot = list(showCategory = 20, 
        color = "p.adjust", size = "Count", score_threshold = 0.2, 
        similarity_score = "JC", edge_magnifier = 1, node_magnifier = 0.1, 
        enable_physics = FALSE), or_dot_plot = list(showCategory = 10, 
        img_format = "png"), or_bar_plot = list(x = "Count", 
        color = "p.adjust", showCategory = 10, img_format = "png"), 
    or_cnet_plot = list(showCategory = 3, displayed_labels = "IDs and Description", 
        enable_physics = TRUE), or_emap_plot = list(showCategory = 20, 
        color = "p.adjust", size = "Count", score_threshold = 0.2, 
        similarity_score = "JC", edge_magnifier = 1, node_magnifier = 0.1, 
        enable_physics = FALSE)), 
        hardcoded_settings = list(
        enrichment_analysis = list(
        terms = character(0),
        adjustment = character(0)),
        over_representation_analysis = list(
        terms = character(0),
        adjustment = character(0))))
