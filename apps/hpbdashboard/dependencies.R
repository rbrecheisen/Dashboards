required_packages <- c(
  "shiny",
  "shinyjs",
  "httr",
  "R6",
  "jsonlite",
  "tidyverse",
  "janitor",
  "lubridate",
  "DT",
  "ggplot2",
  "devtools"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(missing_packages, library, character.only = TRUE))

if(!requireNamespace("CastoR", quietly = TRUE)) {
  devtools::install_github("rbrecheisen/CastoR", dependencies = TRUE, force = TRUE)
}