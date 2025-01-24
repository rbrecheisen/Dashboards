library(R6)
library(dplyr)
library(ggplot2)

source("charts/chart.R")


LiverTimeMdtToSurgeryChart = R6Class(
  
  "LiverTimeMdtToSurgeryChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 0,
          operatie_lever_operatie_niet_doorgegaan != 1,
          resectie != 6
        )
      self$df = self$df %>%
        mutate(
          date_mdo = as.Date(date_mdo),
          date_operatie = as.Date(date_operatie),
          mdo_to_surgery = as.numeric(date_operatie - date_mdo),
          month_year = format(date_operatie, "%Y-%m")
        )
    },
    
    show = function() {
      average_times <- self$df %>%
        group_by(month_year) %>%
        summarize(average_times = mean(mdo_to_surgery, na.rm = TRUE), .groups = "drop") %>%
        mutate(month_year = factor(month_year, levels = sort(unique(month_year))))
      ggplot(average_times, aes(x = month_year, y = average_times)) +
        geom_col(width = 0.5, fill = "blue") +
        labs(x = "Month", y = "Average number of days") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)
