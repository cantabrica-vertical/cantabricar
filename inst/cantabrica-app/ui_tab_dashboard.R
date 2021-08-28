tab_dashboard <- function() {
    tabItem(
        tabName = "dashboard",
        fluidPage(
            useShinyjs(),
            fluidRow(actionButton("actualizar", "Actualizar", icon("sync"), style = "color: #fff; background-color: red; border-color: red"),),
            br(),
            br(),
            fluidRow(
                box(
                    title = "Monitor de actividad", width = 12,
                    img(src = "logo.jpg", width = 100, height = 100, alt = "Logo"),
                    infoBox(title = "Sembradas", width = 2, color = "aqua", value = textOutput("n_sembradas"), icon = icon("tint")),
                    infoBox(title = "Germinadas", width = 2, color = "green", value = textOutput("n_germinadas"), icon = icon("seedling")),
                    infoBox(title = "Transplantadas", width = 2, color = "yellow", value = textOutput("n_trasplantadas"), icon = icon("truck")),
                    infoBox(title = "Hojas verdaderas", width = 2, color = "orange", value = textOutput("n_hojas"), icon = icon("leaf")),
                    infoBox(title = "Cosechadas", width = 2, color = "fuchsia", value = textOutput("n_cosechadas"), icon = icon("tractor"))
                )
            ),
            fluidRow(
                column(
                    width = 8,
                    box(title = "Bandejas", width = 12, plotlyOutput("dashboard_bandejas_plot")),
                ),
                column(
                    width = 4,
                    box(
                        title = "Gestionar instalaciones",
                        column(
                            width = 6,
                            fluidRow(textOutput("dashboard_estanterias")),
                            br(), br(),
                            fluidRow(textOutput("dashboard_baldas")),
                            br(), br(),
                            fluidRow(textOutput("dashboard_bandejas")),

                        ),
                        column(
                            width = 3,
                            fluidRow(actionButton("bandejas_remove_estanteria", "-")),
                            br(),
                            fluidRow(actionButton("bandejas_remove_balda", "-")),
                            br(),
                            fluidRow(actionButton("bandejas_remove_bandeja", "-"))
                        ),
                        column(
                            width = 3,
                            fluidRow(actionButton("bandejas_add_estanteria", "+")),
                            br(),
                            fluidRow(actionButton("bandejas_add_balda", "+")),
                            br(),
                            fluidRow(actionButton("bandejas_add_bandeja", "+"))
                        )
                    )
                )
            ),
            br(),
            br(),
            fluidRow(
                column(
                    width = 3,
                    box(title = "Plantas sembradas", width = 12, plotlyOutput(outputId = "dashboard_species_plot"))
                ),
                column(
                    width = 9,
                    box(title = "Siembras por fecha", width = 12, plotlyOutput(outputId = "dashboard_fechas_plot"))
                )
            )
        )
    )
}
