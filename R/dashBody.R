#' Dashboard body
#'
#' @param ... An object created by \code{shiny::tags}.
#'
#' @examples
#'
#' @export
#' @importFrom shiny tags
dashBody <- function(...){
    shiny::tags$div(class = "content-wrapper"
        , shiny::tags$section(class = "content", ...))
}