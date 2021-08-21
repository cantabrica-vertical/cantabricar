tab_trasplante <- function() {
    tabItem(
        tabName = "trasplante",
        fluidPage(
            useShinyjs(),
            tags$head(tags$script(src = "message-handler.js")),
            title = "Trasplantes",
            fluidRow(
                box(
                    title = "Nuevo trasplante",
                    width = 4,
                    status = "success",
                    selectInput(
                        inputId = "database_trasplante_id",
                        label = "ID",
                        choices = pull(
                            filter(
                                d$sembradas,
                                !(id %in% d$cosechadas$id),
                                !(id %in% d$trasplantadas$id)
                            ),
                            id
                        ),
                        selected = NULL,
                        multiple = TRUE
                    ),
                    dateInput(
                        inputId = "database_trasplante_date",
                        label = "Fecha",
                        value = today(),
                        language = "es",
                        max = today()
                    ),
                    selectInput(
                        inputId = "database_trasplante_medio_trasplante",
                        label = "Medio de trasplante",
                        choices = values$medio_siembra,
                        selected = NULL,
                        multiple = FALSE
                    ),
                    selectInput(
                        inputId = "database_trasplante_luz",
                        label = "Variedad",
                        choices = values$variedad,
                        selected = NULL,
                        multiple = FALSE
                    ),
                    textInput(
                        inputId = "database_trasplantadas_comentarios",
                        label = "Comentarios",
                        value = ""
                    ),
                    br(),
                    actionButton("database_nuevo_trasplante", "Guardar")
                ),
                box(
                    title = "Borrar trasplante",
                    solidHeader = FALSE,
                    status = "danger",
                    selectInput(
                        inputId = "database_eliminar_trasplante_id",
                        label = "ID",
                        choices = d$trasplantadas$id,
                        selected = NULL
                    ),
                    br(),
                    actionButton("database_trasplante_eliminar_button", "Eliminar")
                )
            ),
            fluidRow(
                DT::dataTableOutput(outputId = "trasplante_table")
            )
        )
    )
}
