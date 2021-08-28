# Connect to database
#' @export set_password
#' @importFrom keyring key_set
set_password <- function(){
    key_set("cantabrica", "cantabrica-admin")
}

#' Check Azure password
#' @export check_azure_password
#' @importFrom keyring key_get
#' @importFrom keyring key_set
check_azure_password <- function(){
    x <- tryCatch(
        {
            key_get("cantabrica", "cantabrica-admin")
        },
        error = function(cond){
            key_set("cantabrica", "cantabrica-admin")
        }
    )
    return(x)
}

#' Connect to Azure SQL server
#' @export get_connection
#' @importFrom DBI dbConnect
#' @importFrom odbc odbc
#' @importFrom keyring key_get
#' @export get_connection
get_connection <- function(){
    dbConnect(
        odbc(),
        Driver = "SQL Server",
        server = "tcp:cantabrica.database.windows.net",
        database = "cantabrica",
        uid = "cantabrica-admin",
        pwd = check_azure_password()
    )
}


#' Retrieve all data
#' @export get_data
#' @importFrom DBI dbReadTable
#' @importFrom purrr map
#' @importFrom dplyr mutate_at
#' @importFrom dplyr one_of
get_data <- function(con = NULL){
    if (is.null(con)) con <- get_connection()
    x <- list(
        plantas = dbReadTable(con, "plantas"),
        sembradas = dbReadTable(con, "sembradas"),
        germinadas = dbReadTable(con, "germinadas"),
        hojas = dbReadTable(con, "hojas"),
        trasplantadas = dbReadTable(con, "trasplantadas"),
        cosechadas = dbReadTable(con, "cosechadas")
    ) %>%
        map(function(x) mutate_at(
            x, vars(any_of(c("domo", "luz", "calor"))),
            function(y) as.logical(as.integer(y))))
    return(x)
}

#' Summarise all data into a table
#' @export get_data_summary
#' @import dplyr
#' @importFrom DBI dbReadTable
#' @importFrom purrr map
#' @importFrom purrr reduce
get_data_summary <- function(con = NULL, data = NULL){
    if (is.null(con)) con <- get_connection()
    if (is.null(data)) data <- get_data(con)

    suppressMessages({
        x <- map(
            data, function(x) select(x, -matches("comentarios")) %>%
                mutate_at(vars(matches("id")), as.integer)
        ) %>%
            reduce(left_join) %>%
            rowwise() %>%
            mutate(
                t_germinacion = difftime(fecha_germinacion, fecha_siembra, units = "days"),
                t_hojas = difftime(fecha_hojas, fecha_siembra, units = "days"),
                t_cosecha = difftime(fecha_cosecha, fecha_siembra, units = "days")
            ) %>%
            ungroup() %>%
            arrange(desc(fecha_siembra), desc(id)) %>%
            left_join(get_bandejas()) %>%
            arrange(-id)
    })
    return(x)
}

#' Get values for UI choices
#' @export get_values
#' @importFrom DBI dbReadTable
get_values <- function(con = NULL){
    if (is.null(con)) con <- get_connection()
    tb <- dbReadTable(con, "valores")
    x <- list(
        especie = unique(tb$valor[tb$tipo=="especie"]),
        variedad = unique(tb$valor[tb$tipo=="variedad"]),
        marca = unique(tb$valor[tb$tipo=="marca"]),
        medio_siembra = unique(tb$valor[tb$tipo=="medio_siembra"]),
        planta_tipo = unique(tb$valor[tb$tipo=="planta_tipo"]),
        luz = unique(tb$valor[tb$tipo=="luz"])
    )
    return(x)
}

#' Get tray and shelve arrengements
#' @export get_bandejas
#' @importFrom DBI dbReadTable
get_bandejas <- function(con = NULL){
    if (is.null(con)) con <- get_connection()
    x <- dbReadTable(con, "bandejas")
    return(x)
}

#' Get max number of trays and shelves
#' @export get_instalaciones
#' @importFrom DBI dbReadTable
#' @importFrom tidyr drop_na
get_instalaciones <- function(con = NULL){
    if (is.null(con)) con <- get_connection()
    x <- dbReadTable(con, "instalaciones") %>%
        drop_na(fecha_instalaciones) %>%
        arrange(fecha_instalaciones) %>%
        slice_tail()
    return(x)
}

#' Get empty trays
#' @export get_bandejas_vacias
#' @importFrom dplyr left_join
#' @importFrom dplyr filter
#' @importFrom tidyr unite
#' @importFrom tidyr expand_grid
#' @importFrom dplyr pull
get_bandejas_vacias <- function(
    con =  NULL,
    data = NULL,
    instalaciones = NULL,
    bandejas = NULL
){
    if (is.null(con)) con <- get_connection()
    if (is.null(data)) data <- get_data(con)
    if (is.null(instalaciones)) instalaciones <- get_instalaciones(con)
    if (is.null(bandejas)) bandejas <- get_bandejas(con)

    suppressMessages({
        x <- expand_grid(
            estanterias = 1:instalaciones$max_estanterias,
            balda = 1:instalaciones$max_baldas,
            bandeja = 1:instalaciones$max_bandejas
        ) %>%
            left_join(filter(bandejas, !(id %in% d$cosechadas$id))) %>%
            filter(is.na(id)) %>%
            arrange(estanteria, balda, bandeja) %>%
            unite("bandeja", c(estanteria, balda, bandeja), sep = "-") %>%
            pull(bandeja)
    })
    return(x)
}


#' Add new values to UI choices
#' @export add_values
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbReadTable
add_values <- function(con = NULL, type, value){
    if (is.null(con)) con <- get_connection()
    df <- data.frame(tipo = type, valor = value)
    x <- dbWriteTable(con, "valores", df, append = TRUE)
    return(dbReadTable(con, "valores"))
}

#' Add new row to SQL table
#' @export add_data_row
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbReadTable
add_data_row <- function(con = NULL, table, ...){
    if (is.null(con)) con <- get_connection()
    df <- data.frame(...)
    x <- dbWriteTable(con, table, df, append = TRUE)
    return(dbReadTable(con, table))
}

#' Delete row from SQL table
#' @export delete_data_row
#' @importFrom DBI dbExecute
#' @importFrom DBI dbReadTable
delete_data_row <- function(con = NULL, table, id){
    if (is.null(con)) con <- get_connection()
    qry <- paste0("DELETE FROM ", table, " WHERE id=", id, ";")
    x <- dbExecute(con, statement = qry)
    return(dbReadTable(con, table))
}

#' Add estanteria
#' @export add_estanteria
#' @importFrom DBI dbExecute
add_estanteria <- function(con, instalaciones){
    if (is.null(con)) con <- get_connection()
    if (is.null(instalaciones)) instalaciones <- get_instalaciones(con)
    date <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D"))
    df <- data.frame(
        max_estanterias = instalaciones$max_estanterias+1,
        max_baldas = instalaciones$max_baldas,
        max_bandejas = instalaciones$max_bandejas,
        fecha_instalaciones = date
    )
    dbWriteTable(con, "instalaciones", df, append = TRUE)
}

#' Remove estanteria
#' @export remove_estanteria
#' @importFrom DBI dbExecute
remove_estanteria <- function(con, instalaciones){
    if (is.null(con)) con <- get_connection()
    if (is.null(instalaciones)) instalaciones <- get_instalaciones(con)
    date <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D"))
    df <- data.frame(
        max_estanterias = instalaciones$max_estanterias-1,
        max_baldas = instalaciones$max_baldas,
        max_bandejas = instalaciones$max_bandejas,
        fecha_instalaciones = date
    )
    dbWriteTable(con, "instalaciones", df, append = TRUE)
}

#' Add balda
#' @export add_balda
#' @importFrom DBI dbExecute
add_balda <- function(con, instalaciones){
    if (is.null(con)) con <- get_connection()
    if (is.null(instalaciones)) instalaciones <- get_instalaciones(con)
    date <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D"))
    df <- data.frame(
        max_estanterias = instalaciones$max_estanterias,
        max_baldas = instalaciones$max_baldas+1,
        max_bandejas = instalaciones$max_bandejas,
        fecha_instalaciones = date
    )
    dbWriteTable(con, "instalaciones", df, append = TRUE)
}

#' Remove balda
#' @export remove_balda
#' @importFrom DBI dbExecute
remove_balda <- function(con, instalaciones){
    if (is.null(con)) con <- get_connection()
    if (is.null(instalaciones)) instalaciones <- get_instalaciones(con)
    date <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D"))
    df <- data.frame(
        max_estanterias = instalaciones$max_estanterias,
        max_baldas = instalaciones$max_baldas-1,
        max_bandejas = instalaciones$max_bandejas,
        fecha_instalaciones = date
    )
    dbWriteTable(con, "instalaciones", df, append = TRUE)
}

#' Add bandeja
#' @export add_bandeja
#' @importFrom DBI dbExecute
add_bandeja <- function(con, instalaciones){
    if (is.null(con)) con <- get_connection()
    if (is.null(instalaciones)) instalaciones <- get_instalaciones(con)
    date <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D"))
    df <- data.frame(
        max_estanterias = instalaciones$max_estanterias,
        max_baldas = instalaciones$max_baldas,
        max_bandejas = instalaciones$max_bandejas+1,
        fecha_instalaciones = date
    )
    dbWriteTable(con, "instalaciones", df, append = TRUE)
}

#' Add bandeja
#' @export remove_bandeja
#' @importFrom DBI dbExecute
remove_bandeja <- function(con, instalaciones){
    if (is.null(con)) con <- get_connection()
    if (is.null(instalaciones)) instalaciones <- get_instalaciones(con)
    date <- as.POSIXct(format(Sys.time(), "%Y-%m-%d %H:%M:%D"))
    df <- data.frame(
        max_estanterias = instalaciones$max_estanterias,
        max_baldas = instalaciones$max_baldas,
        max_bandejas = instalaciones$max_bandejas-1,
        fecha_instalaciones = date
    )
    dbWriteTable(con, "instalaciones", df, append = TRUE)
}
