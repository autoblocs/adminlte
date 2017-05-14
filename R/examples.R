#' adminlte examples
#'
#' @param ex name of the example
#'
#' the list of implemented examples is:
#'
#' \itemize{
#'   \item \code{dash1}
#' }
#'
#' @examples
#' if (interactive()) {
#' # Basic dashboard page template
#' library(adminlte)
#'
#' ## call shiny application
#' runExample()
#'
#' }
#' @export
#' @importFrom shiny runApp
rExample <- function(ex = "dash1") {
    ## Extracted from
    ## http://deanattali.com/2015/04/21/r-package-shiny-app/
    appDir <- system.file("shiny", ex, package = "adminlte")
    if (appDir == "") {
        stop("Could not find example directory. Try re-installing `adminlte`.", call. = FALSE)
    }

    shiny::runApp(appDir, display.mode = "normal")
}