tab_germinacion <- function() {
    tabItem(
        tabName = "germinacion",
        fluidPage(
            useShinyjs(),
            tags$head(tags$script(src = "message-handler.js")),
            title = "Germinaci\u00f3n",
            box(
                title = "Nueva germinaci\u00f3n",
                background = "green",
                width = 4,
                selectInput(
                    inputId = "database_germinacion_id",
                    label = "ID",
                    choices = pull(
                        filter(
                            d$sembradas,
                            !(id %in% d$cosechadas$id),
                            !(id %in% d$germinadas$id)
                        ),
                        id
                    ),
                    selected = NULL,
                    multiple = TRUE
                ),
                dateInput(
                    inputId = "database_germinacion_date",
                    label = "Fecha",
                    value = today(),
                    language = "es",
                    max = today()
                ),
                textInput(
                    inputId = "database_germinacion_comentarios",
                    label = "Comentarios",
                    value = ""
                ),
                br(),
                actionButton("database_nueva_germinacion", "Guardar")
            ),
            box(
                title = "Borrar germinaci\u00f3n",
                solidHeader = TRUE,
                status = "danger",
                selectInput(
                    inputId = "database_eliminar_germinacion_id",
                    label = "ID",
                    choices = d$germinadas$id,
                    selected = NULL
                ),
                br(),
                actionButton("database_germinacion_eliminar_button", "Eliminar")
            ),
            box(
                width = 4,
                DT::dataTableOutput(outputId = "germinacion_table")
            )
        )
    )
}