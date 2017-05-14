#' Dashboard body content header
#'
#' @param ... An object created by \code{shiny::tags}.
#'
#' @examples
#'
#' library(adminlte)
#' bodyContentHeader(
#'     tags$h1("Content Header", tags$small("smaller item"))
#' )
#'
#' if (interactive()) {
#' # AdminLTE example
#' library(adminlte)
#' shiny::shinyApp(
#'   ui = dashPage(
#'     dashHeader(),
#'     dashSideBar(menuTab(
#'         menuSection("SECTION  1"),
#'         menuItem("Dashboard", tabName = "dashboard"
#'         , icon = icon("dashboard"), badges = badge("new", "green")),
#'         menuItem(text = "Charts", tabName = "charts"
#'         , icon = icon("pie-chart"), status = label("4", "primary"),
#'             menuSubItem("ChartJS", "charts_1", icon = icon("circle-o")),
#'             menuSubItem("Morris", "charts_2", icon = icon("circle-o"))
#'             )
#'         )),
#'         dashBody(
#'             tabItems(tabItem(tabName = "dashboard", shiny::h1("Dashboard Body"),
#'             bodyContentHeader(tags$h1("Dashboard", tags$small("example")))),
#'                 tabItem(tabName = "charts_1", shiny::h1("ChartJS Body"),
#'                 bodyContentHeader(tags$h1("ChartJS", tags$small("example")))),
#'                 tabItem(tabName = "charts_2", shiny::h1("Morris Body"),
#'                 bodyContentHeader(tags$h1("Morris", tags$small("example")))))
#'         )
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @export
#' @importFrom shiny tags
bodyContentHeader <- function(...){
    shiny::tags$div(class = "content-header", ...)
}

#' Dash board breadcrumbs
#'
#' @param x An object created by \code{shiny::tags} or a character array.
#'
#'
#' @examples
#'
#' library(adminlte)
#' breadcrumb(c("Main", "Sub-Page", "Sub-Sub-Page"))
#'
#' @export
#' @importFrom shiny tags
breadcrumb <- function(x){
    do.call(shiny::tags$ol, c(class = "breadcrumb", lapply(x, shiny::tags$li)))
}