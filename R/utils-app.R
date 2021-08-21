# app utils


#' Custom ggplot2 theme
#' @export theme_custom
#' @import ggplot2
theme_custom <- function() {
    theme_minimal() +
        theme(
            text = element_text(colour = "black", size = 12),
            axis.text = element_text(colour = "black", size = 12)
        )
}


