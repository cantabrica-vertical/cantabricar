tab_cosecha <- function() {
    tabItem(
        tabName = "cosecha",
        fluidPage(
            useShinyjs(),
            tags$head(tags$script(src = "message-handler.js")),
            title = "Cosecha",
            fluidRow(
                box(
                    title = "Nueva cosecha",
                    status = "success",
                    width = 4,
                    selectInput(inputId = "database_cosecha_id", label = "ID", choices = sort(pull(filter(d$sembradas, !(id %in% d$cosechadas$id)), id)), selected = NULL),
                    dateInput( inputId = "database_cosecha_date", label = "Fecha", value = today(), language = "es",max = today()),
                    br(),
                    textInput(inputId = "database_cosecha_comentarios", label = "Comentarios", value = ""),
                    br(),
                    actionButton("database_nueva_cosecha", "Guardar")
                ),
                box(
                    title = "Borrar cosecha",
                    status = "danger",
                    selectInput(inputId = "database_eliminar_cosecha_id", label = "ID", choices = sort(d$cosechadas$id), selected = NULL),
                    br(),
                    actionButton("database_cosecha_eliminar_button", "Eliminar")
                )
            ),
            fluidRow(
                DT::dataTableOutput(outputId = "cosecha_table")
            )
        )
    )
}
