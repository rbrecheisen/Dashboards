library(R6)
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)


Chart = R6Class(
  
  "Chart",
  
  public = list(
    
    df = NULL,
    tmp_dir = NULL,
    
    initialize = function(df, tmp_dir = NULL) {
      self$tmp_dir = tmp_dir
      self$df = df
    },
    
    show = function() {
      message("Not implemented")
    }
  )
)


LiverProceduresPerMonthChart = R6Class(
  
  "LiverProceduresPerMonthChart",
  
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
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month) %>%
        summarise(num_procedures = n())
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures)) +
        geom_bar(stat = "identity") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "Number of liver procedures per month",
          x = "Month",
          y = "Number of procedures"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)


LiverProceduresPerMonthOpenClosedChart = R6Class(
  
  "LiverProceduresPerMonthOpenClosedChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 0,
          operatie_lever_operatie_niet_doorgegaan != 1,
          resectie == 6
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month) %>%
        summarise(num_procedures = n())
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures)) +
        geom_bar(stat = "identity") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "Number of open/closed liver procedures per month",
          x = "Month",
          y = "Number of procedures"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)


LiverComplicationsPerMonthChart = R6Class(
  
  "LiverComplicationsPerMonthChart",
  
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
          title = "Number of pancreas procedures per month",
          x = "Month",
          y = "Number of procedures",
          fill = "Surgical technique"
        ) +
        theme_minimal() +
        scale_fill_discrete(labels = c(
          "0" = "Open procedure",
          "1" = "Fully laparoscopic",
          "2" = "Laparoscopic with conversion",
          "3" = "Fully robotic",
          "4" = "Robotic with conversion to laparoscopic",
          "5" = "Robot with conversion to open",
          "6" = "Laparoscopic resection with robotic reconstruction",
          "7" = "Exploration without resection",
          "8" = "Other",
          "9" = "Not applicable"
        )) +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)


PancreasProceduresOpenClosedPerMonthChart = R6Class(
  
  "PancreasProceduresOpenClosedPerMonthChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        filter(
          lever_pancreas == 1,
          operatie_pancreas != 11,
          operatie_pancreas_techniek == 7
        )
      self$df = self$df %>%
        mutate(date_operatie = dmy(date_operatie)) %>%
        mutate(month = floor_date(date_operatie, "month")) %>%
        group_by(month) %>%
        summarize(num_procedures = n(), .groups = "drop")
    },
    
    show = function() {
      ggplot(self$df, aes(x = month, y = num_procedures)) +
        geom_bar(stat = "identity") +
        labs(
          title = "Number of open/closed pancreas procedures per month",
          x = "Month",
          y = "Number of procedures"
        ) +
        theme_minimal() +
        scale_y_continuous(breaks = seq(0, 50, by = 1))
    }
  )
)
