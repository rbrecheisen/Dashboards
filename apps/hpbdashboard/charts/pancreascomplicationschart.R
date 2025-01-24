library(R6)
library(dplyr)
library(ggplot2)

source("charts/chart.R")


PancreasComplicationsPerMonthChart = R6Class(
  
  "PancreasComplicationsPerMonthChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 1,
          operatie_pancreas != 11,
          operatie_pancreas_techniek < 7,
          complicatie_ok == 1,
          complicaties_waarvoor_reinterventie == 1,
          if_else(is.na(orgaanfalen), -1, orgaanfalen) %in% c(1, 2)
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month, orgaanfalen) %>%
        summarize(num_complications = n(), .groups = "drop")
      self$df <- self$df %>%
        mutate(
          orgaanfalen_label = case_when(
            orgaanfalen == 1 ~ "Single-organ failure",
            orgaanfalen == 2 ~ "Multi-organ failure",
            TRUE ~ "Unknown"
          )
        )
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_complications, fill = orgaanfalen_label)) +
        geom_bar(stat = "identity", width = 0.5) +
        scale_fill_manual(
          values = c("Single-organ failure" = "#718dbf", 
                     "Multi-organ failure" = "#e84d60")
        ) +
        labs(
          x = "Month",
          y = "Number of Complications",
          fill = "Orgaanfalen"
        ) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "right",
          panel.grid.major.x = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank()
        ) +
        scale_y_continuous(expand = c(0, 0))
      # ggplot(self$df, aes(x = month, y = num_procedures, fill = operatie_pancreas_techniek)) +
      #   geom_bar(stat = "identity") +
      #   labs(
      #     title = "Number of pancreas procedures per month",
      #     x = "Month",
      #     y = "Number of procedures",
      #     fill = "Surgical technique"
      #   ) +
      #   theme_minimal() +
      #   scale_fill_discrete(labels = c(
      #     "0" = "Open procedure",
      #     "1" = "Fully laparoscopic",
      #     "2" = "Laparoscopic with conversion",
      #     "3" = "Fully robotic",
      #     "4" = "Robotic with conversion to laparoscopic",
      #     "5" = "Robot with conversion to open",
      #     "6" = "Laparoscopic resection with robotic reconstruction",
      #     "7" = "Exploration without resection",
      #     "8" = "Other",
      #     "9" = "Not applicable"
      #   )) +
      #   scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)
