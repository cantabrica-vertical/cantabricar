tab_calendario <- function() {
    tabItem(
        tabName = "calendario",
        fluidPage(
            calendarOutput(
                outputId = "calendario"
            )
        )
    )
}
