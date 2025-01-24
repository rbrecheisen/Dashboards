library(R6)
library(dplyr)
library(ggplot2)

source("charts/chart.R")


LiverComplicationsChart = R6Class(
  
  "LiverComplicationsChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        mutate(orgaanfalen = factor(
          replace_na(orgaanfalen, "-1"),
          levels = c("-1", "0", "1", "2"),
          labels = c("Missing", "No", "Single-organ", "Multi-organ")
        )) %>%
        filter(
          lever_pancreas == 0,
          operatie_lever_operatie_niet_doorgegaan != 1,
          resectie != 6,
          complicatie_ok == 1,
          complicaties_waarvoor_reinterventie == 1,
          orgaanfalen %in% c("Single-organ", "Multi-organ")
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month, orgaanfalen) %>%
        summarize(num_procedures = n(), .groups = "drop")
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures, fill = orgaanfalen)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Number of liver complications per month",
          x = "Month",
          y = "Number of complications",
          fill = "Orgaanfalen"
        ) +
        theme_minimal() +
        scale_fill_discrete(labels = c(
          "-1" = "Missing",
          "0" = "No",
          "1" = "Single-orgaan",
          "2" = "Multi-orgaan"
        )) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)
