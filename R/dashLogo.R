#' Dashboard Logo
#'
#' @param mini An object created by \code{shiny::tags}.
#' @param large An object created by \code{shiny::tags}.
#'
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' # Basic dashboard page template
#' library(adminlte)
#' clogo <- dashLogo(mini = shiny::tags$b("ALTE"),
#'                   large = shiny::tags$b("AdminLTE")
#'          )
#' cheader <- dashHeader(clogo)
#' shinyApp(
#'   ui = dashPage(
#'     cheader,
#'     dashSideBar(),
#'     dashBody()
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @export
#' @importFrom shiny tags
dashLogo <- function(mini = NULL,
                     large = NULL){

    if(is.null(mini)) {
        cmini = shiny::tags$span(class="logo-mini", shiny::tags$b("A"), "LT")
    } else {
        cmini = shiny::tags$span(class="logo-mini", mini)
    }

    if(is.null(large)) {
        clarge = shiny::tags$span(class="logo-lg", shiny::tags$b("Admin"), "LTE")
    } else {
        clarge = shiny::tags$span(class="logo-lg", large)
    }

    shiny::tags$a(class = "logo",href = "#",
        cmini, clarge)
}