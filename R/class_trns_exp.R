#----------------------------------------- Transcriptomics experiment class ----
Trns_exp = R6::R6Class(
  "Trns_exp",
  public = list(
    initialize = function(name){
      self$name = name
    },
    #--------------------------------------------------------------- Global ----
    name = NULL
  )
)
