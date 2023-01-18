library(shiny)
library(bs4Dash)
library(shinyWidgets)
library(shinybrowser)
library(plotly)

spawn_plotbox = function(id_box, id_plot, width_bs, height_px){
  bs4Dash::box(
    id = id_box,
    title = "Class distribution",
    width = width_bs,
    height = height_px,
    solidHeader = TRUE,
    maximizable = TRUE,
    collapsible = FALSE,
    status = "primary",
    plotly::plotlyOutput(
      outputId = id_plot,
      width = width_bs,
      height = height_px
    )
  )
}

spawn_plot = function(label = "Default", width_px, height_px){
  plotly::plot_ly(x = c(0,1, 2), y = c(2, 1, 3), type = 'bar', width = width_px, height = height_px) %>%
    layout(title = label,
           plot_bgcolor='#e5ecf6',
           xaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'),
           yaxis = list(
             zerolinecolor = '#ffff',
             zerolinewidth = 2,
             gridcolor = 'ffff'))
}

#----------------------------------------- Lipidomics data visualisation UI ----
soda_visualise_lips_ui = function(id) {
  ns = NS(id)
  
  shiny::tagList(
    shinybrowser::detect(),
    shiny::fluidRow(
      shiny::column(
        width = 9,
        shinyWidgets::checkboxGroupButtons(inputId = ns("showPlots"),
                                           label = NULL,
                                           status = "primary",
                                           choices = c("Class distribution" = "select_class_distribution",
                                                       "Class comparison" = "select_class_comparison",
                                                       "Volcano plot" = "select_volcano_plot",
                                                       "Heatmap" = "select_heatmap"),
                                           checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon")))
      ),
      
      shiny::column(
        width = 3,
        shiny::textOutput(ns("xbs")),
        shiny::textOutput(ns("xpx")),
        shiny::textOutput(ns("ypx")),
        shiny::textOutput(ns("nplots"))
      )
    ),
    shiny::uiOutput(
      outputId = ns("plotbox_field")
    )
  )


  

}

#------------------------------------- Lipidomics data visualisation server ----
soda_visualise_lips_server = function(id, r6) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      # Constant dimension ratios
      x_box = 0.9
      y_box = 0.7
      x_plot = 0.8
      y_plot = 0.65
      
      # TMP outplut
      output$xbs = shiny::renderText({
        paste0("Xbs: ", reactive_xbs())
        })

      output$xpx = shiny::renderText({
        paste0("Xpx: ", reactive_xpx(), "px")
      })

      output$ypx = shiny::renderText({
        paste0("Ypx: ", reactive_ypx(), "px")
      })

      output$nplots = shiny::renderText({
        paste0("nplots: ", length(input$showPlots))
      })
      
      

      
      # Reactive dimensions
      
      reactive_xbs = shiny::reactive({
        if (length(input$showPlots) < 2) {
          12
        } else {
          6
        }
      })
      
      reactive_xpx = shiny::reactive({
        if (length(input$showPlots) < 2) {
          shinybrowser::get_width()
        } else {
          shinybrowser::get_width()/2
        }
      })
      
      reactive_ypx = shiny::reactive({
        if (length(input$showPlots) < 3) {
          shinybrowser::get_height()
        } else {
          shinybrowser::get_height()/2
        }
      })      
      
      

      
      # Updater
      shiny::observeEvent(input$showPlots, {
        
        if (length(input$showPlots) == 1) {
          
          output$plotbox_field = shiny::renderUI({
            spawn_plotbox(id_box = ns("dummy_box"),
                          id_plot = ns("dummy_plotly"),
                          width_bs = reactive_xbs(),
                          height_px = reactive_ypx())
          })
          
          # Make plotly plot
          output$dummy_plotly = plotly::renderPlotly(
            spawn_plot(label = "n=1",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          
          # Update plotly plot
          plotdim_proxy = plotlyProxy(
            outputId = "dummy_plotly",
            session = session
          )
          
          
          # Update box
          bs4Dash::updateBox(
            id = "dummy_box",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          
          # Update plotly
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
        } else if(length(input$showPlots) == 2) {
          
          output$plotbox_field = shiny::renderUI({
            shiny::fluidRow(
              spawn_plotbox(id_box = ns("dummy_box_1"),
                            id_plot = ns("dummy_plotly_1"),
                            width_bs = reactive_xbs(),
                            height_px = reactive_ypx()),
              spawn_plotbox(id_box = ns("dummy_box_2"),
                            id_plot = ns("dummy_plotly_2"),
                            width_bs = reactive_xbs(),
                            height_px = reactive_ypx())
            )
          })
          
          # Make plotly plot
          output$dummy_plotly_1 = plotly::renderPlotly(
            spawn_plot(label = "n=2",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          output$dummy_plotly_2 = plotly::renderPlotly(
            spawn_plot(label = "n=2",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          
          # Update plotly plot
          plotdim_proxy_1 = plotlyProxy(
            outputId = "dummy_plotly_1",
            session = session
          )
          plotdim_proxy_2 = plotlyProxy(
            outputId = "dummy_plotly_2",
            session = session
          )
          
          # Update box
          bs4Dash::updateBox(
            id = "dummy_box_1",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          bs4Dash::updateBox(
            id = "dummy_box_2",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          
          # Update plotly
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_1,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_2,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
        
        } else if(length(input$showPlots) == 3) {
          
          output$plotbox_field = shiny::renderUI({
            shiny::tagList(
              shiny::fluidRow(
                spawn_plotbox(id_box = ns("dummy_box_1"),
                              id_plot = ns("dummy_plotly_1"),
                              width_bs = reactive_xbs(),
                              height_px = reactive_ypx()),
                spawn_plotbox(id_box = ns("dummy_box_2"),
                              id_plot = ns("dummy_plotly_2"),
                              width_bs = reactive_xbs(),
                              height_px = reactive_ypx())
              ),
              shiny::fluidRow(
                spawn_plotbox(id_box = ns("dummy_box_3"),
                              id_plot = ns("dummy_plotly_3"),
                              width_bs = reactive_xbs(),
                              height_px = reactive_ypx())
              )
            )

          })
          
          # Make plotly plot
          output$dummy_plotly_1 = plotly::renderPlotly(
            spawn_plot(label = "n=2",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          output$dummy_plotly_2 = plotly::renderPlotly(
            spawn_plot(label = "n=2",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          output$dummy_plotly_3 = plotly::renderPlotly(
            spawn_plot(label = "n=3",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          
          # Update plotly plot
          plotdim_proxy_1 = plotlyProxy(
            outputId = "dummy_plotly_1",
            session = session
          )
          plotdim_proxy_2 = plotlyProxy(
            outputId = "dummy_plotly_2",
            session = session
          )
          plotdim_proxy_3 = plotlyProxy(
            outputId = "dummy_plotly_3",
            session = session
          )
          
          # Update box
          bs4Dash::updateBox(
            id = "dummy_box_1",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          bs4Dash::updateBox(
            id = "dummy_box_2",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          bs4Dash::updateBox(
            id = "dummy_box_3",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          
          # Update plotly
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_1,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_2,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_3,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
          
        } else if(length(input$showPlots) == 4){
          output$plotbox_field = shiny::renderUI({
            shiny::tagList(
              shiny::fluidRow(
                spawn_plotbox(id_box = ns("dummy_box_1"),
                              id_plot = ns("dummy_plotly_1"),
                              width_bs = reactive_xbs(),
                              height_px = reactive_ypx()),
                spawn_plotbox(id_box = ns("dummy_box_2"),
                              id_plot = ns("dummy_plotly_2"),
                              width_bs = reactive_xbs(),
                              height_px = reactive_ypx())
              ),
              shiny::fluidRow(
                spawn_plotbox(id_box = ns("dummy_box_3"),
                              id_plot = ns("dummy_plotly_3"),
                              width_bs = reactive_xbs(),
                              height_px = reactive_ypx()),
                spawn_plotbox(id_box = ns("dummy_box_4"),
                              id_plot = ns("dummy_plotly_4"),
                              width_bs = reactive_xbs(),
                              height_px = reactive_ypx())
              )
            )
            
          })
          
          # Make plotly plot
          output$dummy_plotly_1 = plotly::renderPlotly(
            spawn_plot(label = "n=2",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          output$dummy_plotly_2 = plotly::renderPlotly(
            spawn_plot(label = "n=2",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          output$dummy_plotly_3 = plotly::renderPlotly(
            spawn_plot(label = "n=3",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          output$dummy_plotly_4 = plotly::renderPlotly(
            spawn_plot(label = "n=4",
                       width_px = reactive_xpx()*x_plot,
                       height_px = reactive_ypx()*y_plot)
          )
          
          # Update plotly plot
          plotdim_proxy_1 = plotlyProxy(
            outputId = "dummy_plotly_1",
            session = session
          )
          plotdim_proxy_2 = plotlyProxy(
            outputId = "dummy_plotly_2",
            session = session
          )
          plotdim_proxy_3 = plotlyProxy(
            outputId = "dummy_plotly_3",
            session = session
          )
          plotdim_proxy_4 = plotlyProxy(
            outputId = "dummy_plotly_4",
            session = session
          )
          
          # Update box
          bs4Dash::updateBox(
            id = "dummy_box_1",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          bs4Dash::updateBox(
            id = "dummy_box_2",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          bs4Dash::updateBox(
            id = "dummy_box_3",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          bs4Dash::updateBox(
            id = "dummy_box_4",
            action = "update",
            options = list(
              width = reactive_xbs(),
              height = reactive_ypx() * y_box
            )
          )
          
          # Update plotly
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_1,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_2,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_3,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
          plotly::plotlyProxyInvoke(
            p = plotdim_proxy_4,
            method = "relayout",
            list(width = reactive_xpx() * x_plot,
                 height = reactive_ypx() * y_plot)
          )
        } else {
          output$plotbox_field = shiny::renderUI({
            NULL
          })
        }
      })
    }
  )
}
