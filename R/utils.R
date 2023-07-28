# Utility functions

#---------------------------------------------------------- Purge functions ----
purge_module_inputs = function(id, input_object) {
  base::invisible(
    lapply(grep(id, names(input_object), value = TRUE), function(i) {
      .subset2(input_object, "impl")$.values$remove(i)
    })
  )
}