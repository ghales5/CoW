#' @export
runWPHUCentre <- function(mapbox_access_token = NULL, ...) {
  if (!is.null(mapbox_access_token)) {
    options(rdeck.mapbox_access_token = mapbox_access_token)
  }

  appDir <- system.file("shiny", "wphu_gis", package = "CoW")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", ..., port = 5555)
}
