#' Dashboar sidebar
#'
#' @param ... An object created by \code{shiny::tags}.
#' @param width The width of the sidebar in css units.
#'
#'
#' @examples
#' if (interactive()) {
#' # AdminLTE example
#' library(adminlte)
#' shiny::shinyApp(
#'   ui = dashPage(
#'     dashHeader(),
#'     dashSideBar(menuTab(
#'         menuSection("SECTION  1"),
#'         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
#'         menuItem(text = "Charts", tabName = "menu1_item4", icon = icon("pie-chart"),
#'             menuSubItem("ChartJS", "menu1_item4_subitem1", icon = icon("circle-o")),
#'             menuSubItem("Morris", "menu1_item4_subitem2", icon = icon("circle-o"))
#'             )
#'         )),
#'     dashBody(),
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @export
#' @importFrom shinydashboard dashboardSidebar
dashSideBar <- function(..., width = NULL){
    shinydashboard::dashboardSidebar(..., disable = FALSE, width)
}