# Utility functions

#--------------------------------------------------------- Switch functions ----

r6_switch = function(exp_type, name){
  switch(EXPR = exp_type,
         "Lipidomics" = Lips_exp$new(name = name),
         "Proteomics" = Prot_exp$new(name = name),
         "Transcriptomics" = Trns_exp$new(name = name)

  )
}

table_switch = function(table_name, r6) {
  switch(EXPR = table_name,
         'Imported metadata table' = r6$tables$imp_meta,
         'Raw metadata table' = r6$tables$raw_meta
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


#--------------------------------------------------------- Import functions ----

find_delim = function(path) {
  probe = paste(readLines(con = path, n = 10), collapse = "")
  sep = c("\t" = lengths(regmatches(probe, gregexpr("\t", probe))),
          "," = lengths(regmatches(probe, gregexpr(",", probe))),
          ";" = lengths(regmatches(probe, gregexpr(";", probe))))
  return(names(which.max(sep)))
}

soda_read_table = function(file_path) {
  if (stringr::str_sub(file_path, -5, -1) == ".xlsx") {
    data_table = as.data.frame(readxl::read_xlsx(file_path))
  } else {
    sep = find_delim(path = file_path)
    data_table = read.csv(file_path,
                             header = T,
                             sep = sep,
                             check.names = FALSE)
  }
  return(data_table)
}

#-------------------------------------------------------- General utilities ----

unique_na_rm = function(vector) {
  vector = vector[!is.na(vector)]
  vector = unique(vector)
  return(vector)
}
