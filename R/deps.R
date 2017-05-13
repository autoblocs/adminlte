# Add an html dependency, without overwriting existing ones
#' @importFrom htmltools htmlDependencies
appendDependencies <- function(x, value) {
    if (inherits(value, "html_dependency"))
        value <- list(value)

    old <- attr(x, "html_dependencies", TRUE)

    htmltools::htmlDependencies(x) <- c(old, value)
    x
}

# Add dashboard dependencies to a tag object
#' @importFrom htmltools htmlDependency
addDeps <- function(x) {
    if (getOption("shiny.minified", TRUE)) {
        adminLTE_js <- "app.min.js"
        adminLTE_css <- c("AdminLTE.min.css", "_all-skins.min.css")
    } else {
        adminLTE_js <- "app.js"
        adminLTE_css <- c("AdminLTE.css", "_all-skins.css")
    }

    dashboardDeps <- list(
        htmltools::htmlDependency("AdminLTE", "2.3.11",
                       c(file = system.file("AdminLTE", package = "adminlte")),
                       script = adminLTE_js,
                       stylesheet = adminLTE_css
        ),
        htmltools::htmlDependency("adminlte",
                       as.character(utils::packageVersion("adminlte")),
                       c(file = system.file(package = "adminlte")),
                       script = "shinydashboard.js",
                       stylesheet = "shinydashboard.css"
        )
    )

    appendDependencies(x, dashboardDeps)
}