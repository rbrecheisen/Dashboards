.libPaths(file.path(getwd(), "R-Portable/library"))

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

# missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]

# if (length(missing_packages) > 0) {
#   install.packages(missing_packages)
# }
# 
# invisible(lapply(missing_packages, library, character.only = TRUE))

invisible(sapply(required_packages, install_if_missing))