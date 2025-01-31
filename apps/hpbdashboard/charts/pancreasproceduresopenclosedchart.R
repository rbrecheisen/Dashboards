library(R6)
library(dplyr)
library(ggplot2)

source("charts/chart.R")


PancreasProceduresOpenClosedChart = R6Class(
  
  "PancreasProceduresOpenClosedChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 1,
          operatie_pancreas != 11,
          operatie_pancreas_techniek == 7
        ) %>%
        mutate(date_operatie = ymd(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month) %>%
        summarize(num_procedures = n(), .groups = "drop")
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures)) +
        geom_bar(stat = "identity") +
        labs(
          title = "PANCREAS: Number of open/closed procedures",
          x = "Month",
          y = "Number of procedures"
        ) +
        theme_minimal() +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)
