#' Dashboard sidebar menu container
#'
#' @param id menu identification
#' @param ... menu components
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
menuTab <- function(id, ...){

    items <- list(...)

    # Restore a selected tab from bookmarked state. Bookmarking was added in Shiny
    # 0.14.
    if (utils::packageVersion("shiny") >= "0.14" && !is.null(id)) {
        selectedTabName <- shiny::restoreInput(id = id, default = NULL)
        if (!is.null(selectedTabName)) {
            # Find the menuItem or menuSubItem with a `tabname` that matches
            # `selectedTab`. Then set `data-start-selected` to 1 for that tab and 0
            # for all others.

            # Given a menuItem and a logical value for `selected`, set the data-start-selected attribute to the
            # appropriate value (1 or 0).
            selectItem <- function(item, selected) {
                if (length(item$children) == 0) {
                    return(item)
                }

                if (selected) value <- 1
                else          value <- NULL

                # Try to find the child <a data-toggle="tab"> tag and then set
                # data-start-selected="1". The []<- assignment is to preserve
                # attributes.
                item$children[] <- lapply(item$children, function(child) {
                    # Find the appropriate <a> child
                    if (tagMatches(child, name = "a", `data-toggle` = "tab")) {
                        child$attribs[["data-start-selected"]] <- value
                    }

                    child
                })

                item
            }

            # Given a menuItem and a tabName (string), return TRUE if the menuItem has
            # that tabName, FALSE otherwise.
            itemHasTabName <- function(item, tabName) {
                # Must be a <li> tag
                if (!tagMatches(item, name = "li")) {
                    return(FALSE)
                }

                # Look for an <a> child with data-value=tabName
                found <- FALSE
                lapply(item$children, function(child) {
                    if (tagMatches(child, name = "a", `data-value` = tabName)) {
                        found <<- TRUE
                    }
                })

                found
            }

            # Actually do the work of marking selected tabs and unselected ones.
            items <- lapply(items, function(item) {
                if (tagMatches(item, name = "li", class = "treeview")) {
                    # Search in menuSubItems
                    item$children[] <- lapply(item$children[], function(subItem) {

                        if (tagMatches(subItem, name = "ul", class = "treeview-menu")) {
                            subItem$children[] <- lapply(subItem$children, function(subSubItem) {
                                selected <- itemHasTabName(subSubItem, selectedTabName)
                                selectItem(subSubItem, selected)
                            })
                        }
                        subItem
                    })

                } else {
                    # Regular menuItems
                    selected <- itemHasTabName(item, selectedTabName)
                    item <- selectItem(item, selected)
                }

                item
            })
        }
    }

    # Use do.call so that we don't add an extra list layer to the children of the
    # ul tag. This makes it a little easier to traverse the tree to search for
    # selected items to restore.
    do.call(tags$ul, c(id = id, class = "sidebar-menu", items))

}