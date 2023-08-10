#---------------------------------------------- Proteomics experiment class ----
Prot_exp = R6::R6Class(
  "Prot_exp",
  public = list(
    initialize = function(name, id = NA, slot = NA){
      self$name = name
      self$id = id
      self$slot = slot
    },
    #--------------------------------------------------------------- Global ----
    name = NA,
    id = NA,
    slot = NA
  )
)
