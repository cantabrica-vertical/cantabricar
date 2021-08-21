tab_dashboard <- function() {
    tabItem(
        tabName = "dashboard",
        fluidPage(
            useShinyjs(),
            fluidRow(actionButton("actualizar", "Actualizar", icon("sync"), style = "color: #fff; background-color: red; border-color: red"),),
            br(),
            br(),
            fluidRow(
                box(title = "Monitor de actividad", width = 3, img(src = "logo.jpg", width = 100, height = 100, alt = "Logo")),
                box(title = "Estanterias", width = 9, plotlyOutput("dashboard_estanterias_plot"))
                ),
            br(),
            br(),
            column(
                width = 2,
                fluidRow(infoBox(title = "Sembradas", width = 2, color = "aqua", value = textOutput("n_sembradas"), icon = icon("tint"))),
                fluidRow(infoBox(title = "Germinadas", width = 2, color = "green", value = textOutput("n_germinadas"), icon = icon("seedling"))),
                fluidRow(infoBox(title = "Transplantadas", width = 2, color = "yellow", value = textOutput("n_trasplantadas"), icon = icon("truck"))),
                fluidRow(infoBox(title = "Hojas verdaderas", width = 2, color = "orange", value = textOutput("n_hojas"), icon = icon("leaf"))),
                fluidRow(infoBox(title = "Cosechadas", width = 2, color = "fuchsia", value = textOutput("n_cosechadas"), icon = icon("tractor")))
            ),
            column(
                width = 6,
                box(title = "Plantas sembradas", width = 12, plotlyOutput(outputId = "dashboard_species_plot"))
            )
        )
    )
}
