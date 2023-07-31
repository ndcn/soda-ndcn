#---------------------------------------------- Proteomics experiment class ----
Prot_exp = R6::R6Class(
  "Prot_exp",
  public = list(
    initialize = function(name){
      self$name = name
    },
    #--------------------------------------------------------------- Global ----
    name = NULL
  )
)
