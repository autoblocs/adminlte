#' Dashboar sidebar
#'
#' @param ... An object created by \code{shiny::tags}.
#' @param width The width of the sidebar in css units.
#'
#'
#' @examples
#'
#' @export
#' @importFrom shinydashboard dashboardSidebar
dashSideBar <- function(..., width = NULL){
    shinydashboard::dashboardSidebar(..., disable = FALSE, width)
}