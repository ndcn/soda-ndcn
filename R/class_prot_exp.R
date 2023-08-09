#---------------------------------------------- Proteomics experiment class ----
Prot_exp = R6::R6Class(
  "Prot_exp",
  public = list(
    initialize = function(name, slot){
      self$name = name
      self$slot = slot
    },
    #--------------------------------------------------------------- Global ----
    name = NA,
    slot = NA
  )
)
