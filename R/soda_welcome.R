library(shiny)
library(markdown)

soda_welcome = function(){
	shiny::includeMarkdown("./help/welcome.md")
}
