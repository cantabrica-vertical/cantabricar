tab_dashboard <- function() {
    tabItem(
        tabName = "dashboard",
        fluidPage(
            useShinyjs(),
            fluidRow(
                actionButton("actualizar", "Actualizar", icon("sync"), style = "color: #fff; background-color: red; border-color: red"),
                downloadButton("descargar", "Descargar", icon("download"))
            ),
            br(),
            br(),
            fluidRow(
                width = 12,
                img(src = "logo.jpg", width = 100, height = 100, alt = "Logo"),
                infoBox(title = "Sembradas", width = 2, color = "aqua", value = textOutput("n_sembradas"), icon = icon("tint")),
                infoBox(title = "Germinadas", width = 2, color = "green", value = textOutput("n_germinadas"), icon = icon("seedling")),
                infoBox(title = "Transplantadas", width = 2, color = "yellow", value = textOutput("n_trasplantadas"), icon = icon("truck")),
                infoBox(title = "Hojas verdaderas", width = 2, color = "orange", value = textOutput("n_hojas"), icon = icon("leaf")),
                infoBox(title = "Cosechadas", width = 2, color = "fuchsia", value = textOutput("n_cosechadas"), icon = icon("tractor"))
            ),
            fluidRow(
                column(
                    width = 3,
                    box(title = "Plantas sembradas", width = 12, plotOutput(outputId = "dashboard_species_plot"))
                ),
                column(
                    width = 9,
                    box(width = 12, DT::dataTableOutput(outputId = "database_table"))
                )
            )
        )
    )
}