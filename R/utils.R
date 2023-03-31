get_time = function() {
  return(format(Sys.time(), "%H:%M:%S"))
}

get_time_code = function() {
  return(format(Sys.time(), "%H%M%S"))
}

timestamped_name = function(file_name) {
  return(paste0(get_time_code(), '_', file_name))
}

print_time = function(in_print) {
  print(paste0(get_time(), " - ", in_print))
}


convert_long = function(long_table, id_col, feature_col, values_col, meta_cols) {
  print_time("Table convert: Converting table")
  out_table = long_table[,c(id_col, feature_col, values_col, meta_cols)]
  out_table = out_table[!is.na(out_table[,feature_col]),]
  out_table = reshape(out_table, v.names = values_col, timevar = feature_col, idvar = id_col, direction = "wide")
  colnames(out_table) = stringr::str_replace_all(colnames(out_table), paste0(values_col, "."), "")
  meta_table = out_table[,c(id_col, meta_cols)]
  data_table = out_table[,-which(colnames(out_table) %in% meta_cols)]
  data_table = data_table[,-c(which(colnames(data_table) == "NaN"))]
  return(list("meta_table" = meta_table,
              "data_table" = data_table))
}

convert_long_file = function(file, id_col, feature_col, values_col, meta_cols) {
  print_time("Table convert: Importing table")
  sep = find_delim(file)
  long_table = read.table(file,
                          quote = "",
                          sep = sep,
                          header = TRUE)
  out_tables =  convert_long(long_table,
                             id_col,
                             feature_col,
                             values_col,
                             meta_cols)
  print_time("Table convert: Exporting tables")
  write.table(out_tables$meta_table,
              file = stringr::str_replace(file, "\\..*","_meta.tsv"),
              sep = "\t",
              row.names = FALSE)
  write.table(out_tables$data_table,
              file = stringr::str_replace(file, "\\..*","_data.tsv"),
              sep = "\t",
              na = "",
              row.names = FALSE)
}

detect_null_str = function(x) {
  if (is.null(x)) {
    return("")
  } else {
    return(x)
  }
}

cleanup_prot_list = function(prot_list) {
  out_list = c()
  for (prot in prot_list) {
    out_list = c(out_list, base::strsplit(prot, ";")[[1]])
  }
  out_list = base::unique(out_list)
  out_list = base::sort(out_list)
  return(out_list)
}

prots_get_feature_table = function(prot_list) {

  feature_table = data.frame(matrix(ncol = 8, nrow = 0))
  colnames(feature_table) = c("accession", "id", "proteinExistence", "protein", "gene", "go_components", "go_functions", "go_processes")

  while (length(prot_list) > 0) {
    print_time(paste0("Uniprot query: ", length(prot_list), " proteins remaining."))
    end = min(length(prot_list), 100)
    prot_sublist = prot_list[1:end]
    prot_list = prot_list[-c(1:end)]

    prot_query = paste(prot_sublist, collapse = "%2C")
    request_url = paste0("https://www.ebi.ac.uk/proteins/api/proteins?offset=0&size=100&accession=", prot_query)

    
    r = httr::GET(request_url, accept("application/json"))

    fail = base::try(httr::stop_for_status(r))
    if (base::inherits(fail, "try-error")) {
      
      for (prot in prot_sublist) {
        
        request_url = paste0("https://www.ebi.ac.uk/proteins/api/proteins?offset=0&size=100&accession=", prot)
        r = httr::GET(request_url, accept("application/json"))
        
        local_fail = base::try(httr::stop_for_status(r))
        
        if (base::inherits(local_fail, "try-error")) {
          next
        } else {
          uniprot_data = rjson::toJSON(content(r))
          uniprot_data = rjson::fromJSON(uniprot_data)
          
          for (i in 1:length(uniprot_data)) {
            
            go_values = c()
            for (j in 1:length(uniprot_data[[i]]$dbReferences)) {
              if (uniprot_data[[i]]$dbReferences[[j]]$type == "GO") {
                go_values = c(go_values, uniprot_data[[i]]$dbReferences[[j]]$properties$term)
              }
            }
            
            go_components = go_values[stringr::str_detect(go_values, pattern = "C:")]
            go_functions = go_values[stringr::str_detect(go_values, pattern = "F:")]
            go_processes = go_values[stringr::str_detect(go_values, pattern = "P:")]
            
            go_components = stringr::str_replace_all(go_components, "C:", "")
            go_functions = stringr::str_replace_all(go_functions, "F:", "")
            go_processes = stringr::str_replace_all(go_processes, "P:", "")
            
            go_components = paste(go_components, collapse = "|")
            go_functions = paste(go_functions, collapse = "|")
            go_processes = paste(go_processes, collapse = "|")
            
            new_row = c(uniprot_data[[i]]$accession,
                        uniprot_data[[i]]$id,
                        uniprot_data[[i]]$proteinExistence,
                        uniprot_data[[i]]$protein$recommendedName$fullName$value,
                        detect_null_str(uniprot_data[[i]]$gene[[1]]$name$value),
                        go_components,
                        go_functions,
                        go_processes
            )
            feature_table[nrow(feature_table) + 1,] = new_row
          }

        }
        
      }
    } else {
      
      uniprot_data = rjson::toJSON(content(r))
      uniprot_data = rjson::fromJSON(uniprot_data)
      
      
      for (i in 1:length(uniprot_data)) {
        
        go_values = c()
        for (j in 1:length(uniprot_data[[i]]$dbReferences)) {
          if (uniprot_data[[i]]$dbReferences[[j]]$type == "GO") {
            go_values = c(go_values, uniprot_data[[i]]$dbReferences[[j]]$properties$term)
          }
        }
        
        go_components = go_values[stringr::str_detect(go_values, pattern = "C:")]
        go_functions = go_values[stringr::str_detect(go_values, pattern = "F:")]
        go_processes = go_values[stringr::str_detect(go_values, pattern = "P:")]
        
        go_components = stringr::str_replace_all(go_components, "C:", "")
        go_functions = stringr::str_replace_all(go_functions, "F:", "")
        go_processes = stringr::str_replace_all(go_processes, "P:", "")
        
        go_components = paste(go_components, collapse = "|")
        go_functions = paste(go_functions, collapse = "|")
        go_processes = paste(go_processes, collapse = "|")
        
        new_row = c(uniprot_data[[i]]$accession,
                    uniprot_data[[i]]$id,
                    uniprot_data[[i]]$proteinExistence,
                    uniprot_data[[i]]$protein$recommendedName$fullName$value,
                    detect_null_str(uniprot_data[[i]]$gene[[1]]$name$value),
                    go_components,
                    go_functions,
                    go_processes
        )
        feature_table[nrow(feature_table) + 1,] = new_row
      }
    }
  }
  return(feature_table)
}




