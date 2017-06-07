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
    shiny::tags$section(class = "content-header", ...)
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


#' Dashboard infobox
#'
#' @param title An object created by \code{shiny::tags}.
#' @param value An number (numeric, integer).
#' @param subtitle 1
#' @param icon 1
#' @param color 1
#' @param width 1
#' @param href 1
#' @param fill 1
#'
#' @export
infoBox <- function (title, value = NULL, subtitle = NULL, icon = shiny::icon("bar-chart"),
                      color = "aqua", width = c(3, 3, 6, 6), href = NULL, fill = FALSE) {

    validateColor(color)

    tagAssert(icon, type = "i")

    colorClass <- paste0("bg-", color)

    boxContent <- div(class = "info-box", class = if (fill)
        colorClass, span(class = "info-box-icon", class = if (!fill)
            colorClass, icon), div(class = "info-box-content"
                                   , span(class = "info-box-text",
                                          title), if (!is.null(value))
                                              span(class = "info-box-number", value), if (!is.null(subtitle))
                                                  p(subtitle)))
    if (!is.null(href))
        boxContent <- a(href = href, boxContent)
    div(class = if (!is.null(width))
        paste0("col-lg-", width[1], " col-md-", width[2], " col-sm-", width[3], " col-xs-", width[4]), boxContent)
}

#' @rdname infoBox
#' @export
infoBox2 <- function (value = NULL, subtitle = NULL, icon = shiny::icon("bar-chart"),
                     color = "aqua", width = c(3, 3, 6, 6)) {

    validateColor(color)

    tags$div(class= paste0("col-lg-", width[1], " col-md-", width[2], " col-sm-", width[3], " col-xs-", width[4]),
             tags$div(class = paste0("small-box bg-", color),
                      tags$div(class = "inner",
                               tags$h3(value),
                               tags$p(subtitle)),
                      tags$div(class = "icon", icon))
    )
}

#' Dashboard box container
#'
#' @param ... 1
#' @param title 1
#' @param footer 1
#' @param status 1
#' @param solidHeader 1
#' @param background 1
#' @param width 1
#' @param height 1
#' @param collapsible 1
#' @param collapsed 1
#'
#' @export
box <- function (..., title = NULL, footer = NULL, status = NULL, solidHeader = FALSE,
                 background = NULL, width = NULL, height = NULL, collapsible = FALSE,
                 collapsed = FALSE) {

    boxClass <- "box"
    if (solidHeader || !is.null(background)) {
        boxClass <- paste(boxClass, "box-solid")
    }
    if (!is.null(status)) {
        boxClass <- paste0(boxClass, " box-", status)
    }
    if (collapsible && collapsed) {
        boxClass <- paste(boxClass, "collapsed-box")
    }
    if (!is.null(background)) {
        validateColor(background)
        boxClass <- paste0(boxClass, " bg-", background)
    }
    style <- NULL
    if (!is.null(height)) {
        style <- paste0("height: ", validateCssUnit(height))
    }
    titleTag <- NULL
    if (!is.null(title)) {
        titleTag <- h3(class = "box-title", title)
    }
    collapseTag <- NULL
    if (collapsible) {
        buttonStatus <- status %OR% "default"
        collapseIcon <- if (collapsed)
            "plus"
        else "minus"
        collapseTag <- div(class = "box-tools pull-right", tags$button(class = paste0("btn btn-box-tool"),
                                                                       `data-widget` = "collapse", shiny::icon(collapseIcon)))
    }
    headerTag <- NULL
    if (!is.null(titleTag) || !is.null(collapseTag)) {
        headerTag <- div(class = "box-header", titleTag, collapseTag)
    }
    div(class = if (!is.null(width))
        paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style))
            style, headerTag, div(class = "box-body", ...), if (!is.null(footer))
                div(class = "box-footer", footer)))
}

#' @export
box2 <- function (..., title = NULL, footer = NULL, color = NULL, background = NULL,
                  width = NULL, height = NULL, collapsible = FALSE, collapsed = FALSE) {

    boxClass <- "box"
    if (solidHeader || !is.null(background)) {
        boxClass <- paste(boxClass, "box-solid")
    }
    if (!is.null(status)) {
        boxClass <- paste0(boxClass, " box-", status)
    }
    if (collapsible && collapsed) {
        boxClass <- paste(boxClass, "collapsed-box")
    }
    if (!is.null(background)) {
        validateColor(background)
        boxClass <- paste0(boxClass, " bg-", background)
    }
    style <- NULL
    if (!is.null(height)) {
        style <- paste0("height: ", validateCssUnit(height))
    }
    titleTag <- NULL
    if (!is.null(title)) {
        titleTag <- h3(class = "box-title", title)
    }
    collapseTag <- NULL
    if (collapsible) {
        buttonStatus <- status %OR% "default"
        collapseIcon <- if (collapsed)
            "plus"
        else "minus"
        collapseTag <- div(class = "box-tools pull-right", tags$button(class = paste0("btn btn-box-tool"),
                                                                       `data-widget` = "collapse", shiny::icon(collapseIcon)))
    }
    headerTag <- NULL
    if (!is.null(titleTag) || !is.null(collapseTag)) {
        headerTag <- div(class = "box-header", titleTag, collapseTag)
    }
    div(class = if (!is.null(width))
        paste0("col-sm-", width), div(class = boxClass, style = if (!is.null(style))
            style, headerTag, div(class = "box-body", ...), if (!is.null(footer))
                div(class = "box-footer", footer)))
}


#' Create a tabbed box
#'
#' @inheritParams shiny::tabsetPanel
#' @inheritParams box
#' @param title Title for the tabBox.
#' @param side Which side of the box the tabs should be on (\code{"left"} or
#'   \code{"right"}). When \code{side="right"}, the order of tabs will be
#'   reversed.
#'
#' @family boxes
#'
#' @examples
#' ## Only run this example in interactive R sessions
#' if (interactive()) {
#' library(shiny)
#'
#' body <- dashboardBody(
#'   fluidRow(
#'     tabBox(
#'       title = "First tabBox",
#'       # The id lets us use input$tabset1 on the server to find the current tab
#'       id = "tabset1", height = "250px",
#'       tabPanel("Tab1", "First tab content"),
#'       tabPanel("Tab2", "Tab content 2")
#'     ),
#'     tabBox(
#'       side = "right", height = "250px",
#'       selected = "Tab3",
#'       tabPanel("Tab1", "Tab content 1"),
#'       tabPanel("Tab2", "Tab content 2"),
#'       tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
#'     )
#'   ),
#'   fluidRow(
#'     tabBox(
#'       # Title can include an icon
#'       title = tagList(shiny::icon("gear"), "tabBox status"),
#'       tabPanel("Tab1",
#'         "Currently selected tab from first box:",
#'         verbatimTextOutput("tabset1Selected")
#'       ),
#'       tabPanel("Tab2", "Tab content 2")
#'     )
#'   )
#' )
#'
#' shinyApp(
#'   ui = dashboardPage(dashboardHeader(title = "tabBoxes"), dashboardSidebar(), body),
#'   server = function(input, output) {
#'     # The currently selected tab from the first box
#'     output$tabset1Selected <- renderText({
#'       input$tabset1
#'     })
#'   }
#' )
#' }
#' @export
tabBox <- function(..., id = NULL, selected = NULL, title = NULL, icon = NULL,
                   width = NULL, height = NULL, side = c("left", "right"))
{
    side <- match.arg(side)

    # The content is basically a tabsetPanel with some custom modifications
    content <- shiny::tabsetPanel(..., id = id, selected = selected)
    content$attribs$class <- "nav-tabs-custom"

    # Set height
    if (!is.null(height)) {
        content <- tagAppendAttributes(content,
                                       style = paste0("height: ", validateCssUnit(height))
        )
    }

    # Move tabs to right side if needed
    if (side == "right") {
        content$children[[1]] <- tagAppendAttributes(content$children[[1]],
                                                     class = "pull-right"
        )
    }

    # Add title
    if (!is.null(title)) {
        if (side == "left")
            titleClass <- "pull-right"
        else
            titleClass <- "pull-left"

        content$children[[1]] <- htmltools::tagAppendChild(
            content$children[[1]],
            if(is.null(icon)){
                tags$li(class = paste(titleClass, " header"), title)
            }else{
                tags$li(class = paste(titleClass, " header"), icon, title)
            }
        )
    }

    div(class = paste0("col-sm-", width), content)
}

#' @export
column <- function(width, ..., offset = 0){

    colClass <- paste0("col-sm-", width)
    if (offset > 0)
        colClass <- paste0(colClass, " col-sm-offset-", offset)
    tags$div(class = colClass, ...)

}

#' @export
infoBoxOutput <- function(outputId, width = 4) {
    shiny::uiOutput(outputId, class = paste0("col-sm-", width))
}

#' @export
renderInfoBox <- function(expr, env = parent.frame(), quoted = FALSE) {
    # Convert the expression to a function
    vbox_fun <- shiny::exprToFunction(expr, env, quoted)

    # Wrap that function in another function which strips off the outer div and
    # send it to renderUI.
    shiny::renderUI({
        vbox <- vbox_fun()
        tagAssert(vbox, type = "div")

        # Strip off outer div, since it's already present in output
        vbox$children[[1]]
    })
}