#' Dashboard Header
#'
#' It contains logos, dropdown menus: messages, notifications, tasks and user menus
#'
#' @param logo  An object created by \code{dashLogo}.
#' @param ... Objects created by \code{menuItem}.
#'
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' # Basic dashboard page template
#' library(shiny)
#'
#' cheader <- dashHeader()
#' shinyApp(
#'   ui = dashboardPage(
#'     cheader,
#'     dashSidebar(),
#'     dashBody(),
#'     dashFooter()
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @export
#' @importFrom shiny tags
dashHeader <- function(logo = dashLogo(), ...){

    shiny::tags$header(
        class="main-header",
        logo,
        shiny::tags$nav(
            class="navbar navbar-static-top",
            shiny::tags$a(
                href="#",
                class="sidebar-toggle",
                `data-toggle`="offcanvas",
                role="button",
                shiny::tags$span(
                    class="sr-only",
                    "Toggle navigation"
                ),
                shiny::tags$span(class="icon-bar"),
                shiny::tags$span(class="icon-bar"),
                shiny::tags$span(class="icon-bar")
            ),
            shiny::tags$div(
                class="navbar-custom-menu",
                shiny::tags$ul(
                    class = "nav navbar-nav",
                    ...
                )
            )
        )
    )

}