#' AdminLTE Dashboard page
#'
#' Complete Dashboard page based on AdminLTE free theme
#'
#' @param header An object created by \code{\link{dashHeader}}.
#' @param sidebar An object created by \code{\link{dashSideBar}}.
#' @param body An object created by \code{\link{dashBody}}.
#' @param footer An object created by \code{\link{dashFooter}}.
#' @param title Browser's title bar.
#' @param style A css style.
#' @param skin A color theme. One of \code{"blue"}, \code{"black"},
#'   \code{"purple"}, \code{"green"}, \code{"red"}, or \code{"yellow"}.
#'
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' # Basic dashboard page template
#' library(shiny)
#' shinyApp(
#'   ui = dashPage(
#'     dashHeader(),
#'     dashSideBar(),
#'     dashBody(),
#'     dashFooter()
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#'
#' @export
#' @importFrom shiny tags bootstrapPage
dashPage <- function(header, sidebar, body, footer = NULL, title = "AdminLTE 2 | General UI", style = "height: auto;",  skin = "blue"){

    ## necessary to receive uiOutput from shiny
    tagAssert(header, type = "header", class = "main-header")
    tagAssert(sidebar, type = "aside", class = "main-sidebar")
    tagAssert(body, type = "div", class = "content-wrapper")
    if(!is.null(footer))
        tagAssert(footer, type = "footer", class = "main-footer")

    skin <- match.arg(skin)

    extractTitle <- function(header) {
        x <- header$children[[1]]
        if (x$name == "span" && !is.null(x$attribs$class) &&
            x$attribs$class == "logo" && length(x$children) !=
            0) {
            x$children[[1]]
        }
        else {
            ""
        }
    }
    title <- title %OR% extractTitle(header)
    content <- shiny::tags$div(class = "wrapper", header, sidebar, body, footer)
    ## sidebar menus are visible
    addDeps(shiny::tags$body(class = paste0("skin-", skin, " sidebar-mini"), style = style,
                      shiny::bootstrapPage(content, title = title)))
}