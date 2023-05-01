
#------------------------- Class General settings -------------------------------
General_settings_class = R6::R6Class(
  "General_settings",
  public = list(
    # Colors 
    color_settings = list(
      name = "Spectral",
      n = 11,
      ramp = 40,
      color_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = 11, name = 'Spectral'))(40),
      color_preview = NULL
    ),
    
    color_list = list(
      "YlOrRd" = 9,
      "YlOrBr" = 9,
      "YlGnBu" = 9,
      "YlGn" = 9,
      "Reds" = 9,
      "RdPu" = 9,
      "Purples" = 9,
      "PuRd" = 9,
      "PuBuGn" = 9,
      "PuBu" = 9,
      "OrRd" = 9,
      "Oranges" = 9,
      "Greys" = 9,
      "Greens" = 9,
      "GnBu" = 9,
      "BuPu" = 9,
      "BuGn" = 9,
      "Blues" = 9,
      "Set1" = 9,
      "Set2" = 8,
      "Set3" = 12,
      "Pastel1" = 9,
      "Pastel2" = 8,
      "Paired" = 12,
      "Dark2" = 8,
      "Accent" = 8,
      "Spectral" = 11,
      "RdYlGn" = 11,
      "RdYlBu" = 11,
      "RdGy" = 11,
      "RdBu" = 11,
      "PuOr" = 11,
      "PRGn" = 11,
      "PiYG" = 11,
      "BrBG" = 11
    ),
    
    # Colors methods
    
    get_color_preview = function(color_palette = self$color_settings$color_palette) {
      self$color_settings$color_preview = get_color_plot(color_palette)
    },
    
    set_color_palette = function(name, n, ramp) {
      print_time(paste0("Changing color palette to ", name))
      self$color_settings$name = name
      self$color_settings$n = n
      self$color_settings$ramp = ramp
      self$color_settings$color_palette = grDevices::colorRampPalette(RColorBrewer::brewer.pal(n = n, name = name))(ramp)
      self$get_color_preview()
    }
    

  )
)


