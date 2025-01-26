required_packages <- c(
  "shiny",
  "reticulate"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(missing_packages, library, character.only = TRUE))