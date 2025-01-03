required_packages <- c(
  "yaml",
  "httr",
  "R6",
  "tidyverse",
  "janitor",
  "lubridate",
  "shiny",
  "ggplot2"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))