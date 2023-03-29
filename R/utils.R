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


convert_long = function(long_table, sample_col, feature_col, values_col, meta_cols) {
  print_time("Table convert: Converting table")
  out_table = long_table[,c(sample_col, feature_col, values_col, meta_cols)]
  out_table = reshape(out_table, v.names = values_col, timevar = feature_col, idvar = sample_col, direction = "wide")
  colnames(out_table) = stringr::str_replace_all(colnames(out_table), paste0(values_col, "."), "")
  meta_table = out_table[,c(sample_col, meta_cols)]
  data_table = out_table[,-which(colnames(out_table) %in% meta_cols)]
  return(list("meta_table" = meta_table,
              "data_table" = data_table))
}

convert_long_file = function(file, sample_col, feature_col, values_col, meta_cols) {
  print_time("Table convert: Importing table")
  sep = find_delim(file)
  long_table = read.table(file,
                          quote = "",
                          sep = sep,
                          header = TRUE)
  out_tables =  convert_long(long_table,
                             sample_col,
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