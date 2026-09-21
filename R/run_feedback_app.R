#' Run the D-score feedback prototype
#'
#' @param data Optional prepared `dfeedback_data`. If NULL, the packaged
#'   `jgz_example_feedback` data are prepared with `prepare_args`.
#' @param prepare_args Named list passed to [prepare_feedback_data()].
#' @param launch.browser Passed to [shiny::runApp()].
#' @param ... Additional arguments passed to [shiny::runApp()].
#' @export
run_feedback_app <- function(data = NULL, prepare_args = list(),
                             launch.browser = interactive(), ...) {
  if (is.null(data)) {
    data("jgz_example_feedback", package = "dscoreddi", envir = environment())
    data <- do.call(prepare_feedback_data, c(list(data = jgz_example_feedback), prepare_args))
  } else if (!inherits(data, "dfeedback_data")) {
    data <- do.call(prepare_feedback_data, c(list(data = data), prepare_args))
  }
  app_dir <- system.file("shiny", "dfeedback", package = "dscoreddi")
  if (!nzchar(app_dir)) stop("The Shiny app was not installed with dscoreddi.", call. = FALSE)
  old <- options(dscoreddi.feedback_data = data)
  on.exit(options(old), add = TRUE)
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}
