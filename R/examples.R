#' @export
#' @importFrom shiny runApp
runExample <- function(ex = "dash1") {
    ## Extracted from
    ## http://deanattali.com/2015/04/21/r-package-shiny-app/
    appDir <- system.file("shiny", ex, package = "adminlte")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `adminlte`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}