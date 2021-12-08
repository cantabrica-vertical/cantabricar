tab_germinacion <- function() {
    tabItem(
        tabName = "germinacion",
        fluidPage(
            useShinyjs(),
            tags$head(tags$script(src = "message-handler.js")),
            title = "Germinaci\u00f3n",
            fluidRow(
                box(
                    title = "Nueva germinaci\u00f3n",
                    status = "success",
                    width = 4,
                    selectInput(inputId = "database_germinacion_id", label = "ID", choices = pull(filter(d$siembras, !(id %in% d$cosechas$id), !(id %in% d$germinaciones$id)), id) %>% sort(), selected = NULL, multiple = FALSE),
                    dateInput(inputId = "database_germinacion_date", label = "Fecha", value = today(), language = "es", max = today()),
                    textInput(inputId = "database_germinacion_comentarios", label = "Comentarios", value = ""),
                    br(),
                    actionButton("database_nueva_germinacion", "Guardar")
                ),
                box(
                    title = "Borrar germinaci\u00f3n",
                    status = "danger",
                    selectInput(inputId = "database_eliminar_germinacion_id", label = "ID", choices = sort(d$germinaciones$id), selected = NULL
),
                    br(),
                    actionButton("database_germinacion_eliminar_button", "Eliminar")
                )
            ),
            fluidRow(
                DT::dataTableOutput(outputId = "germinacion_table")
            )
        )
    )
}
