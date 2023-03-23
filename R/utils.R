get_time = function() {
  return(format(Sys.time(), "%H:%M:%S"))
}

print_time = function(in_print) {
  print(paste0(get_time(), " - ", in_print))
}