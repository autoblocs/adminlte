#' Dashboard sidebar menu container
#'
#' @param id menu identification
#' @param ... menu components
#'
#'
#' @examples
#'
#'
#' @export
#' @importFrom shinydashboard sidebarMenu
menuTab <- function(id, ...){
    shinydashboard::sidebarMenu(..., id = id)
}