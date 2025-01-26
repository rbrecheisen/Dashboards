required_packages <- c(
  "shiny",
  "reticulate",
  "here"
)

missing_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(missing_packages, library, character.only = TRUE))

library(reticulate)

venv_name <- "r-reticulate"

if(!virtualenv_exists(venv_name)) {
  virtualenv_create(venv_name)
  cat("Virtual environment created: ", venv_name, "\n")
}

library(here)

virtualenv_install(
  venv_name, 
  packages = c(
    "torch", 
    "torchvision", 
    "torchaudio"
  ),
  pip_options = "--verbose"
)

use_virtualenv(venv_name, required = TRUE)