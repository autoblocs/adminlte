#' SideBar menu section
#'
#' @param x Section name
#'
#' @examples
#' if (interactive()) {
#' # Basic dashboard page template
#' library(shiny)
#' shinyApp(
#'   ui = dashPage(
#'     dashHeader(),
#'     dashSideBar(menuTab(
#'         menuSection("SECTION  1"),
#'         menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
#'         )),
#'     dashBody(),
#'     dashFooter()
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @export
#' @importFrom shiny tags
menuSection <- function(x){
    shiny::tags$li(class = "header", x)
}

#' @export
menuItem <- function(text, ..., icon = NULL, badgeLabel = NULL, badgeColor = "green",
                     tabName = NULL, href = NULL, newtab = TRUE, selected = NULL){

    shinydashboard::menuItem(text = text, ..., icon = icon, badgeLabel = badgeLabel, badgeColor = badgeColor, tabName = tabName
                             , href = href, newtab = newtab, selected = selected)

}

#' @export
menuSubItem <- function(text, tabName = NULL, href = NULL, newtab = TRUE,
                        icon = shiny::icon("angle-double-right"), selected = NULL){

    shinydashboard::menuSubItem(text = text, tabName = tabName, href = href, newtab = newtab, icon = icon, selected = selected)

}