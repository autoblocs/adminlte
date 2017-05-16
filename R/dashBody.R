#' Dashboard body
#'
#' @param ... An object created by \code{shiny::tags}.
#' @param header An object created by \code{adminlte::bodyContentHeader}.
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
#'         menuItem("Dashboard", tabName = "dashboard"
#'         , icon = icon("dashboard"), badges = badge("new", "green")),
#'         menuItem(text = "Charts", tabName = "charts"
#'         , icon = icon("pie-chart"), status = label("4", "primary"),
#'             menuSubItem("ChartJS", "charts_1", icon = icon("circle-o")),
#'             menuSubItem("Morris", "charts_2", icon = icon("circle-o"))
#'             )
#'         )),
#'         dashBody(
#'             tabItems(tabItem(tabName = "dashboard", shiny::h1("Dashboard Body")),
#'                 tabItem(tabName = "charts_1", shiny::h1("ChartJS Body")),
#'                 tabItem(tabName = "charts_2", shiny::h1("Morris Body")))
#'         )
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @export
#' @importFrom shiny tags
dashBody <- function(..., header = NULL){
    if(is.null(header)){
        shiny::tags$div(class = "content-wrapper",
                        shiny::tags$section(class = "content", ...))
    }else{
        shiny::tags$div(class = "content-wrapper",
                        header,
                        shiny::tags$section(class = "content", ...))
    }
}