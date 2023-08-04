#---------------------------------------------- Lipidomics experiment class ----
Lips_exp = R6::R6Class(
  "Lips_exp",
  public = list(
    initialize = function(name){
      self$name = name
    },
    #--------------------------------------------------------------- Global ----
    name = NA,

    #--------------------------------------------------------------- Indices ----

    indices = list(
      id_col_meta = NA,
      id_col_data = NA,
      type_col = NA,
      group_col = NA,
      batch_col = NA,

      idx_blanks = NULL,
      idx_qcs = NULL,
      idx_pools = NULL,
      idx_samples = NULL,

      rownames_blanks = NULL,
      rownames_qcs = NULL,
      rownames_pools = NULL,
      rownames_samples = NULL,

      excluded_cols = NULL
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(

      imp_meta = NULL,
      raw_meta = NULL,

      imp_data = NULL,
      raw_data = NULL,

      blank_table = NULL,

      feature_table = NULL,

      # Normalised
      class_norm_data = NULL,
      total_norm_data = NULL,

      # Z-scored
      z_scored_data = NULL,
      z_scored_class_norm_data = NULL,
      z_scored_total_norm_data = NULL,

      # class tables
      class_table= NULL,
      class_table_total_norm = NULL,
      class_table_z_scored_total_norm = NULL

    ),

    #-------------------------------------------------------- Table methods ----

    set_raw_meta = function(){

      if (!is.na(self$indices$id_col_meta) & !is.null(self$tables$imp_meta)){
        data_table = self$tables$imp_meta
        rownames(data_table) = data_table[,self$indices$id_col_meta]
        data_table[,self$indices$id_col_meta] = NULL
        self$tables$raw_meta = data_table
      }
    },

    set_raw_data = function(apply_imputation = T,
                            impute_before = T,
                            apply_filtering = T,
                            imputation_function = 'median',
                            val_threshold = 0.6,
                            blank_multiplier = 0.8,
                            sample_threshold = 0.8,
                            group_threshold = 0.8,
                            norm_col = "") {

      if (!is.na(self$indices$id_col_data) & !is.null(self$tables$imp_data) & !is.null(self$tables$raw_meta)){

        # Copy imported table
        data_table = self$tables$imp_data

        # Set ID column
        rownames(data_table) = data_table[,self$indices$id_col_data]
        data_table[,self$indices$id_col_data] = NULL
        data_table = as.matrix(data_table)

        # Keep only rows from raw_meta
        data_table = data_table[rownames(self$tables$raw_meta),]

        # Remove columns from exclusion list
        if (!is.null(self$indices$excluded_cols)) {
          data_table = drop_cols(data_table, self$indices$excluded_cols)
        }

        # Remove empty columns
        data_table = remove_empty_cols(data_table)


        # Imputation and filtering
        if (apply_imputation & impute_before & apply_filtering) {
          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)

          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)

        } else if (apply_imputation & !impute_before & apply_filtering) {
          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)

          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)
        } else if (apply_imputation & !apply_filtering) {
          data_table = impute_na(method = imputation_function,
                                 data_table = data_table,
                                 meta_table = self$tables$raw_meta,
                                 group_col = self$indices$group_col,
                                 sample_rownames = self$indices$rownames_samples,
                                 val_threshold = val_threshold)
        } else if (!apply_imputation & apply_filtering) {
          # Filtering alone
          del_cols = lips_get_del_cols(data_table = data_table,
                                       blank_table = self$tables$blank_table,
                                       imp_meta = self$tables$imp_meta,
                                       raw_meta = self$tables$raw_meta,
                                       idx_blanks = self$indices$idx_blanks,
                                       idx_samples = self$indices$idx_samples,
                                       id_col_meta = self$indices$id_col_meta,
                                       group_col = self$indices$group_col,
                                       batch_col = self$indices$batch_col,
                                       blank_multiplier = blank_multiplier,
                                       sample_threshold = sample_threshold,
                                       group_threshold = group_threshold)

          data_table = drop_cols(data_table, del_cols)
        }

        if (norm_col != "") {
          if (is_num_coercible(self$tables$raw_meta[,norm_col]) & !base::any(is.na(self$tables$raw_meta[,norm_col]))) {
            print(paste0('Normalizing data by ', norm_col))
            data_table = data_table/as.numeric(self$tables$raw_meta[,norm_col])
          } else {
            print('Warning: Normalization skipped, selected column contains either non numeric or missing data.')
          }
        }

        self$tables$raw_data = data_table
      }
    },

    get_feature_table = function() {
      self$tables$feature_table = get_feature_metadata(data_table = self$tables$raw_data)
    },

    get_blank_table = function() {
      blank_table = self$tables$imp_data[self$indices$idx_blanks,]
      rownames(blank_table) = blank_table[,self$indices$id_col_data]
      blank_table[,self$indices$id_col_data] = NULL
      self$tables$blank_table = as.matrix(blank_table)
    },


    # Class normalisation
    normalise_class = function(){
      self$tables$class_norm_data = normalise_lipid_class(self$tables$raw_data)
    },

    # Total or Row normalisation
    normalise_total = function(){
      self$tables$total_norm_data = self$tables$raw_data/rowSums(self$tables$raw_data, na.rm = T)
    },

    # Z-score normalisation
    normalise_z_score = function() {
      self$tables$z_scored_data = z_score_normalisation(data_table = self$tables$raw_data,
                                                        impute = NA)
    },

    # Class and z-score normalisation
    normalise_class_z_score = function() {
      self$tables$z_scored_class_norm_data = z_score_normalisation(data_table = self$tables$class_norm_data,
                                                                   impute = NA)
    },

    # Total and z-score normalisation
    normalise_total_z_score = function() {
      self$tables$z_scored_total_norm_data = z_score_normalisation(data_table = self$tables$total_norm_data,
                                                                   impute = NA)
    },

    # Class table
    get_class_table = function(){
      self$tables$class_table = get_lipid_class_table(self$tables$raw_data)
    },

    # Class table total norm
    class_grouping_total_norm = function(){
      self$tables$class_table_total_norm = get_lipid_class_table(self$tables$total_norm_data)
    },

    # Z-score the class table (generated by the class_grouping method)
    normalise_class_table_z_score = function() {
      self$tables$class_table_z_scored_total_norm = z_score_normalisation(data_table = self$tables$class_table_total_norm,
                                                                    impute = NA)
    },

    derive_data_tables = function() {
      self$normalise_class()
      self$normalise_total()
      self$normalise_z_score()
      self$normalise_class_z_score()
      self$normalise_total_z_score()
      self$get_class_table()
      self$class_grouping_total_norm()
      self$normalise_class_table_z_score()
    }

  )
)
