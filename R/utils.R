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


convert_long = function(long_table, id_col, feature_col, values_col, meta_cols, remove_conflicts = TRUE) {
  print_time("Table convert: Converting table")
  out_table = long_table[,c(id_col, feature_col, values_col, meta_cols)]
  out_table = out_table[!is.na(out_table[,feature_col]),]
  out_table = reshape(out_table, v.names = values_col, timevar = feature_col, idvar = id_col, direction = "wide")
  colnames(out_table) = stringr::str_replace_all(colnames(out_table), paste0(values_col, "."), "")
  meta_table = out_table[,c(id_col, meta_cols)]
  data_table = out_table[,-which(colnames(out_table) %in% meta_cols)]
  data_table = data_table[,-c(which(colnames(data_table) == "NaN"))]
  if (remove_conflicts) {
    new_colnames = new_colnames = stringr::str_split_i(colnames(data_table), ";", 1)
    colnames(data_table) = new_colnames
  }
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

#prot_list = c("A0A0B4J2F0", "A0AV96", "A0AVT1")
prots_get_feature_table = function(prot_list, main_go) {

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
  
  all_go_components = unlist(stringr::str_split(feature_table[,"go_components"], "\\|"), recursive = TRUE, use.names = FALSE)
  all_go_functions = unlist(stringr::str_split(feature_table[,"go_functions"], "\\|"), recursive = TRUE, use.names = FALSE)
  all_go_processes = unlist(stringr::str_split(feature_table[,"go_processes"], "\\|"), recursive = TRUE, use.names = FALSE)
  
  all_go_components = sort(unique(all_go_components))
  all_go_functions = sort(unique(all_go_functions))
  all_go_processes = sort(unique(all_go_processes))
  
  if (any(all_go_components == "")){
    all_go_components = all_go_components[-which(all_go_components == "")]
  }
  
  if (any(all_go_functions == "")){
    all_go_functions = all_go_functions[-which(all_go_functions == "")]
  }
  
  if (any(all_go_processes == "")){
    all_go_processes = all_go_processes[-which(all_go_processes == "")]
  }

  go_components_table = data.frame(matrix(data = FALSE, ncol = length(all_go_components), nrow = nrow(feature_table)))
  colnames(go_components_table) = all_go_components
  rownames(go_components_table) = feature_table$accession

  go_functions_table = data.frame(matrix(data = FALSE, ncol = length(all_go_functions), nrow = nrow(feature_table)))
  colnames(go_functions_table) = all_go_functions
  rownames(go_functions_table) = feature_table$accession
  
  go_processes_table = data.frame(matrix(data = FALSE, ncol = length(all_go_processes), nrow = nrow(feature_table)))
  colnames(go_processes_table) = all_go_processes
  rownames(go_processes_table) = feature_table$accession
  
  print_time("Filling Gene Ontology tables")
  for (i in 1:nrow(feature_table)) {
    go_components_i = stringr::str_split(feature_table[i, "go_components"], "\\|")[[1]]
    go_functions_i = stringr::str_split(feature_table[i, "go_functions"], "\\|")[[1]]
    go_processes_i = stringr::str_split(feature_table[i, "go_processes"], "\\|")[[1]]
    
    if (any(go_components_i == "")){
      go_components_i = go_components_i[-which(go_components_i == "")]
    }
    if (any(go_functions_i == "")){
      go_functions_i = go_functions_i[-which(go_functions_i == "")]
    }
    if (any(go_processes_i == "")){
      go_processes_i = go_processes_i[-which(all_go_processes == "")]
    }
    
    go_components_table[i, go_components_i] = TRUE
    go_functions_table[i, go_functions_i] = TRUE
    go_processes_table[i, go_processes_i] = TRUE
    
  }
  
  if (main_go) {
    
    main_components = c("nuclear chromosome", "extracellular region", "extracellular space", "cell wall",
                        "nucleus", "nuclear envelope", "nucleoplasm", "chromosome", "nucleolus",
                        "mitochondrion", "lysosome", "endosome", "vacuole", "peroxisome", "endoplasmic reticulum",
                        "golgi apparatus", "lipid droplet", "microtubule organizing center", "cytosol",
                        "ribosome", "cytoskeleton", "plasma membrane", "cilium", "plastid", "thylakoid",
                        "external encapsulating structure", "extracellular matrix", "cytoplasmic vesicle",
                        "organelle")
    
    main_functions = c("virus receptor activity", "DNA binding", "RNA binding", "cytoskeletal motor activity",
                       "catalytic activity", "GTPase activity", "structural molecule activity",
                       "transporter activity", "cytoskeletal protein binding", "lipid binding",
                       "cyclase activity", "antioxidant activity", "oxidoreductase activity",
                       "transferase activity", "hydrolase activity", "lyase activity", "isomerase activity",
                       "ligase activity", "protein tag", "cargo receptor activity", "histone binding",
                       "protein folding chaperone", "translation regulator activity", "nutrient reservoir activity",
                       "receptor ligand activity", "molecular transducer activity", "molecular adaptor activity",
                       "toxin activity", "cell adhesion mediator activity", "molecular function regulator activity",
                       "virus coreceptor activity", "catalytic activity, acting on a protein",
                       "catalytic activity, acting on dna", "catalytic activity, acting on rna",
                       "molecular carrier activity", "transcription regulator activity", "general transcription initiation factor activity",
                       "small molecule sensor activity", "molecular sequestering activity", "atp-dependent activity")
    
    main_processes = c("mitotic cell cycle", "cytokinesis", "cytoplasmic translation", "immune system process",
                       "muscle system process", "circulatory system process", "renal system process",
                       "respiratory system process", "carbohydrate metabolic process", "generation of precursor metabolites and energy",
                       "dna replication", "dna repair", "dna recombination", "chromatin organization",
                       "dna-templated transcription", "regulation of dna-templated transcription",
                       "trna metabolic process", "protein folding", "protein glycosylation", "amino acid metabolic process",
                       "cellular modified amino acid metabolic process", "lipid metabolic process",
                       "vitamin metabolic process", "sulfur compound metabolic process", "intracellular protein transport",
                       "nucleocytoplasmic transport", "autophagy", "inflammatory response", "mitochondrion organization",
                       "cytoskeleton organization", "microtubule-based movement", "peroxisome organization", "lysosome organization",
                       "chromosome segregation", "cell adhesion", "establishment or maintenance of cell polarity",
                       "programmed cell death", "photosynthesis", "mrna metabolic process", "snrna metabolic process", "vesicle-mediated transport",
                       "reproductive process", "digestive system process", "signaling", "cell differentiation", "protein catabolic process",
                       "extracellular matrix organization", "rna-mediated gene silencing", "telomere organization", "cell junction organization",
                       "wound healing", "ribosome biogenesis", "cilium organization", "anatomical structure development",
                       "cell motility", "nervous system process", "endocrine process", "protein maturation",
                       "transmembrane transport", "nucleobase-containing small molecule metabolic process",
                       "hepaticobiliary system process", "membrane organization", "protein-containing complex assembly",
                       "cell wall organization or biogenesis", "nitrogen cycle metabolic process", "protein localization to plasma membrane",
                       "defense response to other organism", "detoxification", "meiotic nuclear division",
                       "mitotic nuclear division", "mitochondrial gene expression", "carbohydrate derivative metabolic process")
    
    kept_components = which(colnames(go_components_table) %in% main_components)
    go_components_table = go_components_table[,kept_components]
    
    kept_functions = which(colnames(go_functions_table) %in% main_functions)
    go_functions_table = go_functions_table[,kept_functions]
    
    kept_processes = which(colnames(go_processes_table) %in% main_processes)
    go_processes_table = go_processes_table[,kept_processes]

  }
  
  
  feature_table[,"go_components"] = NULL
  feature_table[,"go_functions"] = NULL
  feature_table[,"go_processes"] = NULL
  
  return(list("feature_table" = feature_table,
              "go_components" = go_components_table,
              "go_functions" = go_functions_table,
              "go_processes" = go_processes_table))
}




