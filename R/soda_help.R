library(shiny)
library(markdown)

soda_help = function(help_file){
	shiny::includeMarkdown(paste0("./help/", help_file, ".md"))
}
