# app

#' Launch cantabrica-app
#' @export launch_app
#' @importFrom shiny shinyAppDir
launch_app <- function(){
    app_dir <- system.file("cantabrica-app", package = "cantabricar")
    if (!dir.exists(app_dir)) {
        stop("Could not find example directory. Try re-installing cantabricar.", call. = FALSE)
    }
    shinyAppDir(app_dir, options = list(display.mode = "normal", launch.browser = TRUE))
}


