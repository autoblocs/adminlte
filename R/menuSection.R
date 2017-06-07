#' Dashboard sidebar menu
#'
#'
#'
#' @param x Section name.
#' @param text Menu label.
#' @param icon A \code{shiny::icon} object.
#' @param status A \link{label} object.
#' @param tabName Menu identification
#' @param href url reference (default is to consider href = "#")
#' @param newtab If \code{href} is supplied, should the link open in a new browser tab?
#' @param selected If \code{TRUE}, this \code{menuItem} or \code{menuSubItem} will start selected. If no item have
#'   \code{selected=TRUE}, then the first \code{menuItem} will start selected.
#' @param badges A \link{badge} object.
#' @param ... Other menu items.
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
#' @importFrom shiny tags
menuSection <- function(x){
    shiny::tags$li(class = "header", x)
}

#' @rdname menuSection
#' @export
#' @importFrom shiny tags
menuItem <- function(text, ..., icon = NULL, status = NULL,
                     tabName = NULL, href = NULL, newtab = TRUE, selected = NULL, badges = NULL){

    subItems <- list(...)
    if (!is.null(icon))
        tagAssert(icon, type = "i")
    if (!is.null(href) + (!is.null(tabName) + (length(subItems) > 0) != 1)) {
        stop("Must have either href, tabName, or sub-items (contained in ...).")
    }

    if (!is.null(badges) && length(subItems) != 0) {
        stop("Can't have both badge and subItems")
    }

    isTabItem <- FALSE
    target <- NULL
    if (!is.null(tabName)) {
        validateTabName(tabName)
        isTabItem <- TRUE
        href <- paste0("#shiny-tab-", tabName)
    }
    else if (is.null(href)) {
        href <- "#"
    }
    else {
        if (newtab)
            target <- "_blank"
    }

    if (length(subItems) == 0) {
        return(shiny::tags$li(
            `class` = if (isTRUE(selected)) "active" else NULL,
            shiny::tags$a(
                href = href, `data-toggle` = if (isTabItem) "tab",
                `data-value` = if (!is.null(tabName)) tabName,
                target = target, icon, shiny::tags$span(text), badges)))
    }


    if(is.null(status)){
        item_menu <- shiny::tags$a(href = href, icon, shiny::tags$span(text),
                                   shiny::icon("angle-left", class = "pull-right"))
    }else{
        item_menu <- shiny::tags$a(href = href, icon, shiny::tags$span(text),
                                   status)
    }

    shiny::tags$li(class = "treeview", item_menu,
            do.call(shiny::tags$ul, c(class = "treeview-menu", subItems)))

}

#' @rdname menuSection
#' @export
#' @importFrom shiny tags
menuSubItem <- function(text, tabName = NULL, href = NULL, newtab = TRUE,
                        icon = shiny::icon("angle-double-right"), selected = NULL){

    if (!is.null(href) && !is.null(tabName)) {
        stop("Can't specify both href and tabName")
    }
    isTabItem <- FALSE
    target <- NULL
    if (!is.null(tabName)) {
        validateTabName(tabName)
        isTabItem <- TRUE
        href <- paste0("#shiny-tab-", tabName)
    }
    else if (is.null(href)) {
        href <- "#"
    }
    else {
        if (newtab)
            target <- "_blank"
    }
    shiny::tags$li(shiny::tags$a(href = href, `data-toggle` = if (isTabItem)
        "tab", `data-value` = if (!is.null(tabName))
            tabName, `data-start-selected` = if (isTRUE(selected))
                1
        else NULL, target = target, icon, text))

}

#' Create a simple Badge/Label.
#'
#' Dashboard menus can contains badges and labels to help users identify
#' necessary actions.
#'
#' @param label label name.
#' @param color color defined by \link{validColors} (it can be an array).
#' @param status status array defined by \link{validStatuses} (it can be an array).
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
#'         menuItem("Dashboard", tabName = "dashboard",
#'          icon = icon("dashboard"), badges = badge("new", "green")),
#'         menuItem(text = "Charts", tabName = "charts",
#'          icon = icon("pie-chart"), status = label("4", "primary"),
#'             menuSubItem("ChartJS", "charts_1", icon = icon("circle-o")),
#'             menuSubItem("Morris", "charts_2", icon = icon("circle-o"))
#'             )
#'         )),
#'     dashBody(),
#'     dashFooter()
#'   ),
#'   server = function(input, output) { }
#' )
#' }
#' @export
#' @importFrom shiny tags
badge <- function(label, color){

    df <- data.frame(lbl = label, col = color)

    obj <- apply(df, 1, function(r){
        validateColor(r[["col"]])
        shiny::tags$span(
            class = paste0("label pull-right bg-", r[["col"]]),
            r[["lbl"]]
        )
    })

    shiny::tags$span(class = "pull-right-container", obj)
}

#' @rdname badge
#' @export
#' @importFrom shiny tags
label <- function(label, status){

    ## it will check if dimensions are correct
    df <- data.frame(lbl = label, st = status)

    obj <- apply(df, 1, function(r){
        validateStatus(r[["st"]])
        shiny::tags$small(
            class = paste0("label label-", r[["st"]], " pull-right"),
            r[["lbl"]]
        )
    })

    shiny::tags$span(class = "pull-right-container", obj)
}
