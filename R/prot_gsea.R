
get_gsea_list_prot = function() {
  plot_list = c("Dot plot" = "select_dot_plot",
                "Ridge plot" = "select_ridge_plot",
                "CNET plot" = "select_cnet_plot",
                "eMap plot" = "select_emap_plot"
  )
  return(plot_list)
}


prot_gsea_plotbox_switch_ui = function(selection_list){
  ui_functions = c()
  for (plot in selection_list) {
    ui_functions = c(ui_functions, switch(EXPR = plot,
                                          "select_dot_plot" = prot_dot_plot_ui,
                                          "select_ridge_plot" = prot_ridge_plot_ui,
                                          "select_cnet_plot" = prot_cnet_plot_ui,
                                          "select_emap_plot" = prot_emap_plot_ui
    )
    )
  }
  return(ui_functions)
}

prot_gsea_plotbox_switch_server = function(selection_list){
  server_functions = c()
  for (plot in selection_list) {
    server_functions = c(server_functions, switch(EXPR = plot,
                                                  "select_dot_plot" = prot_dot_plot_server,
                                                  "select_ridge_plot" = prot_ridge_plot_server,
                                                  "select_cnet_plot" = prot_cnet_plot_server,
                                                  "select_emap_plot" = prot_emap_plot_server)
    )
  }
  return(server_functions)
}


prot_gsea_plot_one = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_gsea_plotbox_switch_ui(selection_list = selection_list)
  
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session)
      )
    )
  })
  
  plot_servers = prot_gsea_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


prot_gsea_plot_two = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_gsea_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session)
      )
    )
  })
  
  plot_servers = prot_gsea_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

prot_gsea_plot_three = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_gsea_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session)
      )
    )
  })
  
  plot_servers = prot_gsea_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}

prot_gsea_plot_four = function(r6, dimensions_obj, selection_list, input, output, session) {
  ns = session$ns
  ui_functions = prot_gsea_plotbox_switch_ui(selection_list = selection_list)
  output$plotbox_field = shiny::renderUI({
    shiny::fluidRow(
      shiny::tagList(
        ui_functions[[1]](dimensions_obj, session),
        ui_functions[[2]](dimensions_obj, session),
        ui_functions[[3]](dimensions_obj, session),
        ui_functions[[4]](dimensions_obj, session)
      )
    )
  })
  
  plot_servers = prot_gsea_plotbox_switch_server(selection_list = input$showPlots)
  for (server_function in plot_servers) {
    server_function(r6, output, session)
  }
}


#------------------------------------------------------- Proteomics GSEA UI ----
soda_gsea_prot_ui = function(id, head_meta = F, head_data = T) {
  ns = shiny::NS(id)
  bs4Dash::tabsetPanel(
    type = "tabs",
    #------------------------------------------------------------ Setup tab ----
    shiny::tabPanel(
      title = "Setup",
      shiny::fluidRow(
        shinybrowser::detect(),
        shiny::h2("Setup for Geneset Enrichment Analysis")
      ),
      shiny::fluidRow(
        shiny::column(
          width = 4,
          shiny::h3("Data preparation"),
          shiny::actionButton(inputId = ns("refresh"),
                              label = "Refresh"),
          shiny::selectInput(inputId = ns("select_group_col"),
                             label = "Select group column",
                             choices = NULL,
                             selected = NULL,
                             width = "100%"),
          shiny::selectInput(inputId = ns("select_groups"),
                             label = "Select two groups to compare",
                             choices = NULL,
                             selected = NULL,
                             multiple = TRUE,
                             width = "100%"),
          shiny::selectInput(inputId = ns("select_method"),
                             label = "Mean or Median",
                             choices = c("median", "mean"),
                             selected = "median",
                             width = "100%"),
          shiny::selectInput(inputId = ns("select_test"),
                             label = "Select test",
                             choices = c("T-test", "Wilcoxon"),
                             selected = "T-test",
                             multiple = FALSE,
                             width = "100%"),
          shiny::checkboxInput(inputId = ns("apply_cutoff"),
                               label = "Apply p-value cutoff",
                               value = T,
                               width = "100%"),
          shiny::sliderInput(inputId = ns("p_value_cutoff_1"),
                             label = "p-value cutoff",
                             min = 0.01,
                             max = 1,
                             value = 0.05,
                             step = 0.01,
                             width = "100%"),
          shiny::selectInput(inputId = ns("select_adjustment_1"),
                             label = "Adjustment",
                             choices = c("None", "Benjamini-Hochberg"),
                             selected = "Benjamini-Hochberg",
                             width = "100%")
        ),
        shiny::column(
          width = 4,
          shiny::h3("GSEA parameters"),
          shiny::selectInput(inputId = ns("select_go_ontology"),
                             label = "GO ontology",
                             choices = c("ALL", "BP", "MF", "CC"),
                             selected = "ALL",
                             width = "100%"),
          shiny::textInput(inputId = ns("min_gs_size"),
                           label = "Minimum geneset size",
                           value = 3,
                           width = "100%"),
          shiny::textInput(inputId = ns("max_gs_size"),
                           label = "Maximum geneset size",
                           value = 800,
                           width = "100%"),
          shiny::sliderInput(inputId = ns("p_value_cutoff_2"),
                             label = "p-value cutoff",
                             min = 0.01,
                             max = 1,
                             value = 0.05,
                             step = 0.01,
                             width = "100%"),
          shiny::selectInput(inputId = ns("select_adjustment_2"),
                             label = "Adjustment",
                             choices = c("holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none"),
                             selected = "none",
                             width = "100%"),
          shiny::selectInput(inputId = ns("termsim_method"),
                             label = "Termsim - Method",
                             choices = c("JC"),
                             # choices = c("JC", "Resnik", "Lin", "Rel", "Jiang"),
                             selected = "JC",
                             width = "100%"),
          shiny::textInput(inputId = ns("termsim_showcat"),
                           label = "Show category",
                           value = "200",
                           width = "100%"),
          shiny::actionButton(inputId = ns("run_gsea"),
                              label = "Run GSEA",
                              width = "100%")
        )
      )
    ),
    shiny::tabPanel(
      title = "Results",
      shiny::fluidRow(
        shiny::column(
          width = 11,
          shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                             label = NULL,
                                             status = "default",
                                             choices = get_gsea_list_prot(),
                                             checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")),
                                             size = "normal",
                                             justified = TRUE)
        ),
        shiny::column(
          width = 1,
          shinyWidgets::actionBttn(inputId = ns("clear_plots"),
                                   label = "Clear plots",
                                   style = "stretch",
                                   color = "danger")
        )
      ),
      shiny::uiOutput(
        outputId = ns("plotbox_field")
      )
    )
  )
  
}

#--------------------------------------------------- Proteomics GSEA server ----
soda_gsea_prot_server = function(id, max_rows = 10, max_cols = 8, r6, r6_settings) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns = session$ns
      
      ## Create a dimensions object to store browser dimensions
      dimensions_obj = shiny::reactiveValues()
      
      # Constant values
      dimensions_obj$x_box = 0.9
      dimensions_obj$y_box = 0.8
      dimensions_obj$x_plot = 0.8
      dimensions_obj$y_plot = 0.75
      dimensions_obj$x_plot_full = 0.95
      dimensions_obj$y_plot_full = 0.91
      
      shiny::observe({
        dimensions_obj$xpx_total = shinybrowser::get_width()
        dimensions_obj$ypx_total = shinybrowser::get_height()
      })
      
      
      # Reactive values
      shiny::observeEvent(input$showPlots,{
        
        # x (width) in BS (Bootstrap) values
        if (length(input$showPlots) < 2) {
          dimensions_obj$xbs = 12
        } else {
          dimensions_obj$xbs = 6
        }
        
        # x (width) in pixels
        if (length(input$showPlots) < 2) {
          dimensions_obj$xpx = shinybrowser::get_width()
        } else {
          dimensions_obj$xpx = shinybrowser::get_width()/2
        }
        
        # y (height) in pixels
        if (length(input$showPlots) < 3) {
          dimensions_obj$ypx = shinybrowser::get_height()
        } else {
          dimensions_obj$ypx = shinybrowser::get_height()/2.1
        }
      })
      
      # Update metadata groups
      shiny::observeEvent(input$refresh, {
        if (!is.null(r6$tables$meta_filtered)) {
          shiny::updateSelectInput(
            session = session,
            inputId = "select_group_col",
            choices = colnames(r6$tables$meta_filtered),
            selected = colnames(r6$tables$meta_filtered)[1]
          )
        }
      })
      
      # Update selected groups
      shiny::observeEvent(input$select_group_col,{
        shiny::updateSelectInput(
          session = session,
          inputId = "select_groups",
          choices = unique(r6$tables$meta_filtered[, input$select_group_col]),
          selected = unique(r6$tables$meta_filtered[, input$select_group_col])[c(1,2)]
        )
      })
      
      # Run GSEA
      shiny::observeEvent(input$run_gsea,{
        
        r6$get_prot_list(col_group = input$select_group_col,
                         group_1 = input$select_groups[1],
                         group_2 = input$select_groups[2],
                         used_function = input$select_method,
                         test = input$select_test,
                         p_value_cutoff = input$p_value_cutoff_1)
        
        r6$get_gsea_object(ont = input$select_go_ontology,
                           minGSSize = as.numeric(input$min_gs_size),
                           maxGSSize = as.numeric(input$max_gs_size), 
                           pvalueCutoff = input$p_value_cutoff_2, 
                           verbose = TRUE, 
                           OrgDb = "org.Hs.eg.db", 
                           pAdjustMethod = input$select_adjustment_2,
                           termsim_method = input$termsim_method,
                           termsim_showcat = as.numeric(input$termsim_showcat))

      })
      
      # Plot selection
      prot_dot_plot_events(r6, dimensions_obj, r6_settings, input, output, session)
      prot_ridge_plot_events(r6, dimensions_obj, r6_settings, input, output, session)
      prot_cnet_plot_events(r6, dimensions_obj, r6_settings, input, output, session)
      prot_emap_plot_events(r6, dimensions_obj, r6_settings, input, output, session)
      
      # Plot selection
      shiny::observeEvent(input$showPlots, {
        
        # Plots selected: 1 to 4
        print_time(paste0("Plot selection: ", paste(input$showPlots, collapse = ", ")))
        if (length(input$showPlots) == 1) {
          prot_gsea_plot_one(r6 = r6,
                   dimensions_obj = dimensions_obj,
                   selection_list = input$showPlots,
                   input = input,
                   output = output,
                   session = session)
          
        } else if (length(input$showPlots) == 2) {
          prot_gsea_plot_two(r6 = r6,
                   dimensions_obj = dimensions_obj,
                   selection_list = input$showPlots,
                   input = input,
                   output = output,
                   session = session)
          
        } else if (length(input$showPlots) == 3) {
          prot_gsea_plot_three(r6 = r6,
                     dimensions_obj = dimensions_obj,
                     selection_list = input$showPlots,
                     input = input,
                     output = output,
                     session = session)
          
        } else if (length(input$showPlots) >= 4) {
          prot_gsea_plot_four(r6 = r6,
                    dimensions_obj = dimensions_obj,
                    selection_list = input$showPlots,
                    input = input,
                    output = output,
                    session = session)
          
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = setdiff(unname(get_plot_list()), input$showPlots)
          )
          
        }
        if (between(length(input$showPlots), 2, 3)) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = NULL
          )
        } else if (length(input$showPlots) == 1) {
          shinyWidgets::updateCheckboxGroupButtons(
            session = session,
            inputId = "showPlots",
            disabledChoices = input$showPlots
          )
        }
      })

      
      shiny::observeEvent(input$clear_plots, {
        print_time("Clearing plots")
        shinyWidgets::updateCheckboxGroupButtons(inputId = "showPlots",
                                                 disabled = FALSE,
                                                 selected = character(0))
        output$plotbox_field = shiny::renderUI(
          NULL
        )
      })
      
      
    }
  )
}

