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
                            file.info(fit_germinacion_path)$mtime,
                            br()
                        ),
                        img(src = "stan.png", width = 100, height = 100, alt = "Logo")
                    ),
                    br(),
                    br(),
                    fluidRow(
                        column(
                            width = 6,
                            box(
                                width = 12, solidHeader = TRUE,
                                plotOutput(outputId = "estimaciones_germinacion_plot")
                            )
                        ),
                        column(
                            width = 6,
                            box(
                                width = 12, solidHeader = TRUE,
                                gt_output(outputId = "estimaciones_germinacion_table")
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Hojas verdaderas",
                    br(),
                    fluidRow(
                        box(
                            title = "Modelo", width = 3,
                            file.info(fit_hojas_path)$mtime,
                            br()
                        ),
                        img(src = "stan.png", width = 100, height = 100, alt = "Logo")
                    ),
                    br(),
                    br(),
                    fluidRow(
                        column(
                            width = 6,
                            box(
                                width = 12, solidHeader = TRUE,
                                plotOutput(outputId = "estimaciones_hojas_plot")
                            )
                        ),
                        column(
                            width = 6,
                            box(
                                width = 12, solidHeader = TRUE,
                                gt_output(outputId = "estimaciones_hojas_table")
                            )
                        )
                    )
                ),
                tabPanel(
                    title = "Cosecha",
                    br(),
                    fluidRow(
                        box(
                            title = "Modelo", width = 3,
                            file.info(fit_cosecha_path)$mtime,
                            br()
                        ),
                        img(src = "stan.png", width = 100, height = 100, alt = "Logo")
                    ),
                    br(),
                    br(),
                    fluidRow(
                        column(
                            width = 6,
                            box(
                                width = 12, solidHeader = TRUE,
                                plotOutput(outputId = "estimaciones_cosecha_plot")
                            )
                        ),
                        column(
                            width = 6,
                            box(
                                width = 12, solidHeader = TRUE,
                                gt_output(outputId = "estimaciones_cosecha_table")
                            )
                        )
                    )
                )
            )
        )
    )
}
