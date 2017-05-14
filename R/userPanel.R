#' Dashboard user panel.
#'
#'
#' @param user_nm User name.
#' @param img_src Image path.
#' @param ... An object created by \code{shiny::tag}.
#' @param img_alt An image alternative text.
#'
#'
#' @examples
#' if (interactive()) {
#'
#'   # AdminLTE example
#'   library(adminlte)
#'
#'   tdir <- tempdir()
#'   dir.create(file.path(tdir, 'shiny', 'www'), recursive = TRUE, showWarnings = FALSE)
#'   img_source <- system.file('shiny', 'dash1', 'www', 'avatar.png', package = 'adminlte')
#'   file.copy(img_source, file.path(tdir, 'shiny', 'www', 'avatar.png'))
#'
#'   cat(file = file.path(tdir, 'shiny', 'app.R'), "
#'   usrPanel <- userPanel(user_nm = \'User Name\',
#'       img_src = \'avatar.png\',
#'       img_alt = \'User Image\',
#'       shiny::tags$a(href = \'#\', shiny::icon(\'circle text-success\'), \'Online\')
#'   )
#'
#'    shiny::shinyApp(
#'     ui = dashPage(
#'         dashHeader(),
#'         dashSideBar(
#'             usrPanel,
#'             menuTab(
#'                 menuSection(\'SECTION  1\'),
#'                 menuItem(\'Dashboard\', tabName = \'dashboard\',
#'                 icon = icon(\'dashboard\')),
#'                 menuItem(text = \'Charts\', tabName = \'menu1_item4\',
#'                 icon = icon(\'pie-chart\'),
#'                          menuSubItem(\'ChartJS\', \'menu1_item4_subitem1\',
#'                          icon = icon(\'circle-o\')),
#'                          menuSubItem(\'Morris\', \'menu1_item4_subitem2\',
#'                          icon = icon(\'circle-o\'))
#'                 )
#'             )),
#'         dashBody(),
#'     ),
#'     server = function(input, output) { }
#'    )
#'
#'    shiny::runApp(file.path(tdir, 'shiny'))
#'
#'   ")
#'
#'   shiny::runApp(file.path(tdir, 'shiny'))
#'
#'   }
#'
#' @export
#' @importFrom shiny tags
userPanel <- function(user_nm = "User Name", img_src = "avatar.png", ..., img_alt = "User Image"){

    shiny::tags$div(
        class = "user-panel",
        shiny::tags$div(
            class="pull-left image",
            shiny::tags$img(
                src = img_src,
                class = "img-circle",
                alt = img_alt
            )
        ),
        shiny::tags$div(
            class = "pull-left info",
            shiny::tags$p(user_nm),
            ...
        )
    )

}