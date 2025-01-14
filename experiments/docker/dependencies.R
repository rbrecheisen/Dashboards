packages <- c("shiny", "ggplot2", "dplyr")  # Add other necessary packages

install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
}

lapply(packages, install_if_missing)