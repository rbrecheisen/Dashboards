pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

# shiny::shinyApp(
#   ui = hpbdashboard::app_ui,
#   server = hpbdashboard::app_server
# )

hpbdashboard::run_app()
