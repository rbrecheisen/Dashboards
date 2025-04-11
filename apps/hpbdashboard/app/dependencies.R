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

install_if_missing <- function(p) {
  if(!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org/")
}

invisible(sapply(required_packages, install_if_missing))