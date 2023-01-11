# library(shiny)
# library(shinydashboard)
# 
# 
# 
# # Universal data upload module
# 
# soda_data_upload_ui = function(id, label = "Upload data", head = F, type = "meta") {
# 
#   ns = NS(id)
#   shiny::tagList(
#     
#     # First column with the table input and preview
#     shiny::column(
#       width = 8,
#       shiny::h2(label),
#       shiny::fileInput(inputId = ns("file"), label = NULL, multiple = F, accept = c(".csv"), width = "100%"),
#       shinydashboard::box(
#         width = 12,
#         DT::dataTableOutput(ns("table")),style = "height:500px; overflow-y: scroll;overflow-x: scroll;"
#       )
#     ),
#     
#     # Second column for data curation
#     shiny::column(
#       width = 4,
#       actionButton(ns("do"), "Click Me"),
#       shiny::tags$h3("Select columns"),
#       shiny::checkboxInput(inputId = ns("preview"), label = "Display preview only", value = head),
#       soda_get_col_ui(id = ns("select_id"), label = "Sample IDs", desc = "Column containing the sample IDs."),
#       if (type == "meta"){
#         shiny::tagList(
#           soda_get_col_ui(id = ns("select_sample_type"), label ="Type column", desc = "Column containing the sample types."),
#           shiny::h3("Text patterns"),
#           soda_txt_input_ui(id = ns("blank_pattern"), label ="Blank pattern", default = "blank", desc = 'Text pattern to autodect blanks samples from the above metioned "Sample type" column'),
#           soda_txt_input_ui(id = ns("qc_pattern"), label ="QC pattern", default = "qc", desc = 'Text pattern to autodect QC samples from the above metioned "Sample type" column'),
#           soda_txt_input_ui(id = ns("pool_pattern"), label ="Pool pattern", default = "pool",desc = 'Text pattern to autodect Pooled samples from the above metioned "Sample type" column')
#         )
#       }
#       else if (type == "lips"){
#         shiny::tagList(
#           soda_get_col_ui(id = ns("select_sample_group"), label ="Group column", desc = "Column containing the groups for each sample."),
#         )
#       }
#     )
# 
#   )
# }
# 
# soda_data_upload_server = function(id, max_rows = 10, max_cols = 8, type = "meta", r6 = NULL) {
#   shiny::moduleServer(
#     id,
#     function(input, output, session) {
#       
# 
#       
#       # The selected file, if any
#       userFile = reactive({
#         validate(need(input$file, message = FALSE))
#         input$file
#       })
#       
#       # The user's data, parsed into a data frame
#       shiny::observe({
#         if (!is.null(userFile()$datapath)){
#           if (type == "meta"){
#             r6$set_meta(read.csv(userFile()$datapath,
#                                  header = T,
#                                  sep = ",",
#                                  check.names = FALSE))
#             soda_get_col_server(id = "select_id", col_list = colnames(r6$meta))
#             soda_get_col_server(id = "select_sample_type", col_list = colnames(r6$meta))
#           }else{
#             r6$set_raw_data(read.csv(userFile()$datapath,
#                                  header = T,
#                                  sep = ",",
#                                  check.names = FALSE))
#             soda_get_col_server(id = "select_id", col_list = colnames(r6$data_raw))
#             soda_get_col_server(id = "select_sample_group", col_list = colnames(r6$meta))
#           }
#         
#         }
#       })
# 
#       # Output a preview or the whole table depending on the user input
#       shiny::observe({
#         if (!is.null(userFile()$datapath)) {
#           # IF METADATA
#           if (type == "meta"){
#             if (input$preview){
#               output$table = renderDataTable({
#                 DT::datatable(r6$meta[1:min(max_rows, nrow(r6$meta)),1:min(max_cols, ncol(r6$meta))], options = list(paging = FALSE))
#               })
#             }else{
#               output$table = renderDataTable({
#                 DT::datatable(r6$meta, options = list(paging = FALSE))
#               })
#             }
#           }else{
#             if (input$preview){
#               output$table = renderDataTable({
#                 DT::datatable(r6$data_raw[1:min(max_rows, nrow(r6$data_raw)),1:min(max_cols, ncol(r6$data_raw))], options = list(paging = FALSE))
#               })
#             }else{
#               output$table = renderDataTable({
#                 DT::datatable(r6$data_raw, options = list(paging = FALSE))
#               })
#             }
#           }
#         }
# 
#       })
#       
#       observeEvent(input$do, {
#         print(input$select_id)
#       })
# 
# 
#     }
#   )
# }
# 
# 
