required_packages <- c("tidyverse", "arrow", "shiny", "DT", "ggplot2")

install_if_missing <- function(p) {
  if(!requireNamespace(p, quietly = TRUE)) install.packages(p, repos = "https://cloud.r-project.org/")
}

invisible(sapply(required_packages, install_if_missing))