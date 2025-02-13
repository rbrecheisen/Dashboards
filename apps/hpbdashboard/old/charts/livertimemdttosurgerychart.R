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
        ) %>%
        mutate(
          date_mdo = ymd(date_mdo),
          date_operatie = ymd(date_operatie),
          mdo_to_surgery = as.numeric(date_operatie - date_mdo),
          month = floor_date(date_operatie, "month")
        )
    },
    
    show = function() {
      average_times <- self$df %>%
        group_by(month) %>%
        summarize(average_times = mean(mdo_to_surgery, na.rm = TRUE), .groups = "drop")
      ggplot(average_times, aes(x = month, y = average_times)) +
        geom_bar(stat = "identity") +
        scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
        labs(
          title = "LIVER: Average number of days MDT to surgery",
          x = "Month",
          y = "Average number of days"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        scale_y_continuous(breaks = seq(0, 200, by = 10))
    }
  )
)

# load("study_data.Rdata")
# study_data <- study_data %>%
#   filter(
#     lever_pancreas == 0,
#     operatie_lever_operatie_niet_doorgegaan != 1,
#     resectie != 6
#   ) %>%
#   mutate(
#     date_mdo = ymd(date_mdo),
#     date_operatie = ymd(date_operatie),
#     mdo_to_surgery = as.numeric(date_operatie - date_mdo),
#     month = floor_date(date_operatie, "month")
#   )
# study_data$mdo_to_surgery
