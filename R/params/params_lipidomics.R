list(params = list(
    heatmap = list(
        lock_da = F)),
    hardcoded_settings = list(
      volcano_plot = list(
        datasets = list(
          "Raw data table",
          "Class normalized table",
          "Total normalized table"
        )
      ),
      heatmap = list(
        datasets = list(
          'Z-scored table',
          'Z-scored total normalized table',
          'Class table z-scored'
        )
      ),
        enrichment_analysis = list(
            terms = NULL,
            adjustment = NULL),
        over_representation_analysis = list(
            terms = NULL,
            adjustment = NULL
        )
    )
)
