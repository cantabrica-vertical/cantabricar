tab_datos <- function() {
    tabItem(
        tabName = "datos",
        fluidPage(
            fluidRow(
                wellPanel(
                    downloadButton("descargar", "Descargar", icon("download")),
                    br(),
                )
            ),
            br(),
            br(),
            fluidRow(
                gt_output(outputId = "database_table")
            )
        )
    )
}
