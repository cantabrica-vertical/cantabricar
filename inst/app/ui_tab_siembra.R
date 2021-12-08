tab_siembra <- function() {
    tabItem(
        tabName = "siembra",
        fluidPage(
            useShinyjs(),
            tags$head(tags$script(src = "message-handler.js")),
            title = "Siembra",
            box(
                title = "Nueva siembra",
                status = "success",
                width = 4,
                fluidRow(
                    column(
                        width = 6,
                        numericInput(inputId = "database_siembra_id", label = "ID", value = max(as.numeric(d$plantas$id), na.rm = TRUE)+1, min = max(as.numeric(d$plantas$id), na.rm = TRUE)+1, step = 1),
                        selectInput(inputId = "database_siembra_especie", label = "Especie", choices = values$especie, selected = "Albahaca", multiple = FALSE),
                        selectInput(inputId = "database_siembra_variedad", label = "Variedad", choices = values$variedad, selected = NULL, multiple = FALSE),
                        selectInput(inputId = "database_siembra_planta_tipo", label = "Tipo de planta", choices = values$planta_tipo, selected = NULL, multiple = FALSE),
                    ),
                    column(
                        width = 6,
                        selectInput(inputId = "database_siembra_marca", label = "Marca", choices = values$marca, selected = NULL, multiple = FALSE),
                        selectInput(inputId = "database_siembra_medio_siembra", label = "Medio de siembra", choices = values$medio_siembra, selected = NULL, multiple = FALSE),
                        numericInput(inputId = "database_siembra_peso_semillas", label = "Semillas (g)", min = 0, value = 0),
                        checkboxInput(inputId = "database_siembra_calor", label = "Calor", value = FALSE),
                        checkboxInput(inputId = "database_siembra_domo", label = "Domo", value = FALSE),
                        checkboxInput(inputId = "database_siembra_peso", label = "Peso", value = FALSE),
                        checkboxInput(inputId = "database_siembra_luz", label = "Luz", value = FALSE),
                        textInput(inputId = "database_siembra_comentarios", label = "Comentarios", value = "")
                    )
                ),
                br(),
                fluidRow(
                    column(
                        width = 12,
                        actionButton("database_nueva_siembra", "Guardar")
                    )
                )
            ),
            box(
                title = "Borrar siembra",
                status = "danger",
                selectInput(inputId = "database_eliminar_siembra_id", label = "ID", choices = d$sembradas$id, selected = NULL),
                br(),
                actionButton("database_siembra_eliminar_button", "Eliminar")
            ),
            box(
                title = "Nuevos valores",
                width = 4,
                column(
                    width = 6,
                    textInput(inputId = "database_siembra_nuevo_especie", label = "Nueva especie"),
                    actionButton("database_siembra_nuevo_especie_button", "Guardar"),
                    textInput(inputId = "database_siembra_nuevo_variedad", label = "Nueva variedad"),
                    actionButton("database_siembra_nuevo_variedad_button", "Guardar"),
                    textInput(inputId = "database_siembra_nuevo_marca", label = "Nueva marca"),
                    actionButton("database_siembra_nuevo_marca_button", "Guardar"),
                ),
                column(
                    width = 6,
                    textInput(inputId = "database_siembra_nuevo_medio", label = "Nuevo medio de siembra"),
                    actionButton("database_siembra_nuevo_medio_button", "Guardar"),
                    textInput(inputId = "database_siembra_nuevo_tipo", label = "Nuevo tipo"),
                    actionButton("database_siembra_nuevo_tipo_button", "Guardar")
                )
            )
        ),
        fluidRow(
            DT::dataTableOutput(outputId = "siembra_table")
        )
    )
}
