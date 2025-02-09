library(R6)
library(dplyr)
library(ggplot2)

source("charts/chart.R")


PancreasTimeLaparoscopicSurgeryToDischargeChart = R6Class(
  
  "PancreasTimeLaparoscopicSurgeryToDischargeChart",
  
  inherit = Chart,
  
  public = list(
    
    initialize = function(df, tmp_dir = NULL) {
      super$initialize(df, tmp_dir)
      self$df = self$df %>%
        mutate(operatie_pancreas = factor(
          replace_na(operatie_pancreas, -1),
          levels = c(-1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
          labels = c(
            "Missing", 
            "PPPD",
            "Classical Whipple",
            "PRPD",
            "Pancreas body/tail",
            "Appleby",
            "Total pancreatectomy",
            "Enucleation",
            "Biliary resection",
            "Frey",
            "Pancreaticojejunostomy",
            "Other",
            "Operation cancelled"
          )
        )) %>%
        filter(
          lever_pancreas == 1,
          operatie_pancreas != 11,
          operatie_pancreas_techniek == 1 # Laparoscopic
        ) %>%
        mutate(
          date_operatie = ymd(date_operatie),
          datum_ontslag = ymd(datum_ontslag),
          surgery_to_discharge = as.numeric(datum_ontslag - date_operatie),
          month = floor_date(datum_ontslag, "month")
        )
    },
    
    show = function() {
      
      average_times <- self$df %>%
        group_by(month, operatie_pancreas) %>%
        summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
      
      ggplot(average_times, aes(x = month, y = average_times, fill = operatie_pancreas)) +
        geom_bar(stat = "identity") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "PANCREAS: Average number of days LAPAROSCOPIC surgery to discharge",
          x = "Month",
          y = "Average number of days",
          fill = "Type of surgery"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_fill_discrete(labels = c(
          "-1" = "Missing",
          "0" = "PPPD",
          "1" = "Classical Whipple",
          "2" = "PRPD",
          "3" = "Pancreas body/tail",
          "4" = "Appleby",
          "5" = "Total pancreatectomy",
          "6" = "Enucleation",
          "7" = "Biliary resection",
          "8" = "Frey",
          "9" = "Pancreaticojejunostomy",
          "10" = "Other",
          "11" = "Operation cancelled"
        )) +
        scale_y_continuous(breaks = seq(0, 250, by = 10))
    }
  )
)

# load(file.path(path.expand("~"), "study_data.Rdata"))
# data <- data %>%
#   filter(
#     lever_pancreas == 1,
#     operatie_pancreas != 11,
#     operatie_pancreas_techniek == 1
#   ) %>%
#   mutate(
#     date_operatie = ymd(date_operatie),
#     datum_ontslag = ymd(datum_ontslag),
#     surgery_to_discharge = as.numeric(datum_ontslag - date_operatie),
#     month = floor_date(datum_ontslag, "month")
#   )
# data$surgery_to_discharge
# average_times <- data %>%
#   group_by(month, operatie_pancreas) %>%
#   summarize(average_times = mean(surgery_to_discharge, na.rm = TRUE), .groups = "drop")
# average_times
# 
# ggplot(average_times, aes(x = month, y = average_times)) +
#   geom_bar(stat = "identity") +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
#   labs(
#     title = "PANCREAS: Average number of days LAPAROSCOPIC surgery to discharge",
#     x = "Month",
#     y = "Average number of days"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#   scale_y_continuous(breaks = seq(0, 250, by = 10))
