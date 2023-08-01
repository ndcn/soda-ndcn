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
      batch_col = NA
    ),

    #--------------------------------------------------------------- Tables ----

    tables = list(

      imp_meta = NULL,
      raw_meta = NULL

    ),

    #--------------------------------------------------------------- Methods ----

    set_raw_meta = function(){

      if (!is.na(self$indices$id_col_meta) & !is.null(self$tables$imp_meta)){
        data_table = self$tables$imp_meta
        rownames(data_table) = data_table[,self$indices$id_col_meta]
        data_table[,self$indices$id_col_meta] = NULL
        self$tables$raw_meta = data_table
      }
    }


  )
)
