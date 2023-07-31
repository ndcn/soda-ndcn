#---------------------------------------------- Lipidomics experiment class ----
Lips_exp = R6::R6Class(
  "Lips_exp",
  public = list(
    initialize = function(name){
      self$name = name
    },
    #--------------------------------------------------------------- Global ----
    name = NULL
  )
)
