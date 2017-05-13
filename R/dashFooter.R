#' Dashboard footer
#'
#' @param ... An object created by \code{shiny::tags}.
#'
#' @examples
#'
#' dashFooter(
#'     shiny::tags$div(
#'         class = "pull-right hidden-xs",
#'         shiny::tags$b("Version", "2.3.8")),
#'         shiny::tags$strong("Copyright ",
#'         shiny::icon("copyright"),
#'         " 2014-2016",
#'         shiny::tags$a(href = "http://almsaeedstudio.com",
#'         "Almsaeed Studio"),
#'         "."),"All rights reserved.")
#'
#' @export
#' @importFrom shiny tags
dashFooter <- function(...){

    olst <- list(...)
    if(length(olst) != 0){
        shiny::tags$footer(class = "main-footer", ...)
    }

}