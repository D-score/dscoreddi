
#' Open GGData voorbeeld HTML
#'
#' Opent het voorbeeldbestand in de browser.
#' @importFrom utiles browseURL
#' @export
open_GGData_example <- function() {
  path <- system.file("docs", "GGData_dscore.html", package = "dscoreddi")
  if (file.exists(path)) {
    browseURL(path)
  } else {
    stop("HTML-bestand niet gevonden.")
  }
}
