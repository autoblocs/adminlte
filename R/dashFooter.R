#' Dashboard footer
#'
#' @param ... An object created by \code{shiny::tags}.
#'
#' @examples
#'
#' footer <- dashFooter(
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
#'     dashBody(),
#'     footer
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @export
#' @importFrom shiny tags
dashFooter <- function(...){

    olst <- list(...)
    if(length(olst) != 0){
        shiny::tags$footer(class = "main-footer", ...)
    }

}