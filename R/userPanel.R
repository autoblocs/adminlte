#' Dashboard user panel.
#'
#'
#' @param user_nm User name.
#' @param img_src image path.
#' @param ... An object created by \code{shiny::tag}.
#' @param img_alt aimga alternative text.
#'
#'
#' @examples
#'
#' userPanel(
#'     user_nm = "User Image",
#'     img_src = "avatar.png",
#'     img_alt = "User Image",
#'     shiny::tags$a(href = "#", shiny::icon("circle text-success"), "Online"))
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