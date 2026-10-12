#' Run the D-score think-aloud application
#'
#' Starts the scenario-based Shiny application for think-aloud sessions
#' with professionals.
#'
#' @param launch.browser Logical. Open the app in a browser.
#' @param ... Additional arguments passed to [shiny::runApp()].
#'
#' @return No return value. The function is called for its side effect.
#' @export
run_think_aloud_app <- function(
    launch.browser = interactive(),
    ...) {

  app_dir <- system.file(
    "shiny",
    "thinkaloud",
    package = "dscoreddi"
  )

  if (!nzchar(app_dir)) {
    stop(
      "The think-aloud Shiny app was not installed with dscoreddi.",
      call. = FALSE
    )
  }

  shiny::runApp(
    appDir = app_dir,
    launch.browser = launch.browser,
    ...
  )
}
