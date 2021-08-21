tab_dashboard <- function() {
    tabItem(
        tabName = "dashboard",
        fluidPage(
            useShinyjs(),
            fluidRow(actionButton("actualizar", "Actualizar", icon("sync"), style = "color: #fff; background-color: red; border-color: red"),),
            br(),
            br(),
            fluidRow(
                box(title = "Monitor de actividad", width = 12, img(src = "logo.jpg", width = 100, height = 100, alt = "Logo")),
            ),
            fluidRow(
                column(
                    width = 8,
                    box(title = "Estanterias", width = 12, plotlyOutput("dashboard_estanterias_plot")),
                ),
                column(
                    width = 4,
                    fluidRow(infoBox(title = "Sembradas", width = 12, color = "aqua", value = textOutput("n_sembradas"), icon = icon("tint"))),
                    fluidRow(infoBox(title = "Germinadas", width = 12, color = "green", value = textOutput("n_germinadas"), icon = icon("seedling"))),
                    fluidRow(infoBox(title = "Transplantadas", width = 12, color = "yellow", value = textOutput("n_trasplantadas"), icon = icon("truck"))),
                    fluidRow(infoBox(title = "Hojas verdaderas", width = 12, color = "orange", value = textOutput("n_hojas"), icon = icon("leaf"))),
                    fluidRow(infoBox(title = "Cosechadas", width = 12, color = "fuchsia", value = textOutput("n_cosechadas"), icon = icon("tractor")))
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
