library(brochure)
library(golem)

source("R/app.R")

#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = brochureApp(
      golem_add_external_resources(),
      home(),
      login(),
      logout(),
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking
    ),
    golem_opts = list(...)
  )
}
