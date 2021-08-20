tab_datos <- function() {
    tabItem(
        tabName = "datos",
        fluidPage(
            fluidRow(
                wellPanel(
                    downloadButton("descargar", "Descargar", icon("download")),
                    br(),
                    br(),
                    pickerInput(
                        inputId = "datos_cols",
                        label = "Mostrar",
                        choices = names(d_all),
                        selected = c(
                            "id", "especie", "variedad", "marca", "tipo", "fecha_siembra",
                            "fecha_germinacion", "fecha_hojas", "fecha_trasplace", "fecha_cosecha"
                        ),
                        list(`actions-box` = TRUE),
                        multiple = TRUE
                    )
                )
            ),
            br(),
            br(),
            fluidRow(
                DT::dataTableOutput(outputId = "database_table")
            )
        )
    )
}
