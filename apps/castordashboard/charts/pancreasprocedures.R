source("charts/chart.R")

PancreasProceduresPerMonthChart = R6Class(
  
  "PancreasProceduresPerMonthChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 1,
          operatie_pancreas != 11,
          operatie_pancreas_techniek != 7
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month, operatie_pancreas_techniek) %>%
        summarize(num_procedures = n(), .groups = "drop")
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures, fill = operatie_pancreas_techniek)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Number of Pancreas Procedures Per Month",
          x = "Month",
          y = "Number of Procedures",
          fill = "Surgical Technique"
        ) +
        theme_minimal() +
        scale_fill_discrete(labels = c(
          "0" = "Open procedure",
          "1" = "Volledig laparoscopisch",
          "2" = "Laparoscopisch met conversie",
          "3" = "Volledig robot",
          "4" = "Robot met conversie naar laparoscopisch",
          "5" = "Robot met conversie naar open",
          "6" = "Laparoscopische resectie met robot reconstructie",
          "7" = "Exploratie zonder resectie",
          "8" = "Overig",
          "9" = "NVT"
        ))
    }
  )
)