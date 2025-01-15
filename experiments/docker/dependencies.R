packages <- c("shiny", "ggplot2", "dplyr", "httr", "R6", "jsonlite", "janitor", "lubridate")

install_if_missing = function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

lapply(packages, install_if_missing)