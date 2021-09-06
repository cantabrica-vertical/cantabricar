tab_estimaciones <- function() {
    tabItem(
        tabName = "estimaciones",
        fluidPage(
            title = "Estimaciones",
            actionButton("ajustar_modelos", "Ajustar modelos", icon("sync"), style = "color: #fff; background-color: red; border-color: red"),
            br(),
            br(),
            tabsetPanel(
                id = "estimaciones_panel", selected = "germinadas",
                tabPanel(
                    title = "Germinaci\u00f3n", value = "germinadas",
                    br(),
                    fluidRow(
                        box(
                            title = "Modelo", width = 3,
                            file.info(system.file("RDS", "fit_germinacion.rds", package = "cantabricar", mustWork = TRUE))$mtime,
                            br()
                        ),
                        img(src = "stan.png", width = 100, height = 100, alt = "Logo")
                    ),
                    br(),
                    br(),
                    fluidRow(
                        box(
                            title = "Germinaci\u00f3n", width = 12, solidHeader = TRUE,
                            plotOutput(outputId = "estimaciones_germinacion_plot"),
                            DT::dataTableOutput(outputId = "estimaciones_germinacion_table")
                        )
                    )
                ),
                tabPanel(
                    title = "Hojas verdaderas",
                    br(),
                    fluidRow(
                        box(
                            title = "Modelo", width = 3,
                            file.info(system.file("RDS", "fit_hojas.rds", package = "cantabricar", mustWork = TRUE))$mtime,
                            br()
                        ),
                        img(src = "stan.png", width = 100, height = 100, alt = "Logo")
                    ),
                    br(),
                    br(),
                    fluidRow(
                        box(
                            title = "Hojas verdaderas", width = 12, solidHeader = TRUE,
                            plotOutput(outputId = "estimaciones_hojas_plot"),
                            DT::dataTableOutput(outputId = "estimaciones_hojas_table")
                        )
                    )
                ),
                tabPanel(
                    title = "Cosecha",
                    br(),
                    fluidRow(
                        box(
                            title = "Modelo", width = 3,
                            file.info(system.file("RDS", "fit_cosecha.rds", package = "cantabricar", mustWork = TRUE))$mtime,
                            br()
                        ),
                        img(src = "stan.png", width = 100, height = 100, alt = "Logo")
                    ),
                    br(),
                    br(),
                    fluidRow(
                        box(
                            title = "Cosecha", width = 12, solidHeader = TRUE,
                            plotOutput(outputId = "estimaciones_cosecha_plot"),
                            DT::dataTableOutput(outputId = "estimaciones_cosecha_table")
                        )
                    )
                )
            )
        )
    )
}
