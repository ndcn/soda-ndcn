# Utility functions

#--------------------------------------------------------- Switch functions ----

r6_switch = function(exp_type, name){
  switch(EXPR = exp_type,
         "Lipidomics" = Lips_exp$new(name = name),
         "Proteomics" = Prot_exp$new(name = name),
         "Transcriptomics" = Trns_exp$new(name = name)

  )
}

#---------------------------------------------------------- Purge functions ----
purge_module_inputs = function(id, input_object) {
  base::invisible(
    lapply(grep(id, names(input_object), value = TRUE), function(i) {
      .subset2(input_object, "impl")$.values$remove(i)
    })
  )
}

