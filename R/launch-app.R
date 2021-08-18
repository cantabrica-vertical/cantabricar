# app

#' Check Azure password
#' @importFrom keyring key_get
#' @importFrom keyring key_set
check_azure_password <- function(){
    x <- tryCatch(
        {
            key_get("cantabrica", "cantabrica-admin")
        },
        error = function(cond){
            key_set("cantabrica", "cantabrica-admin")
        }
    )
        return(x)
}

#' Launch cantabrica-app
#' @export launch_app
#' @importFrom shiny shinyAppDir
launch_app <- function(){
    app_dir <- paste(.libPaths()[1], "cantabricar", "cantabrica-app", sep = .Platform$file.sep)
    if (!dir.exists(app_dir)) {
        stop("Could not find example directory. Try re-installing cantabricar.", call. = FALSE)
    }
    shinyAppDir(app_dir, options = list(display.mode = "normal", launch.browser = TRUE))
}


