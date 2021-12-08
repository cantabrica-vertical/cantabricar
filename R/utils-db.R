#' Create database
#' export db_Create
db_create <- function(){
    db_path <- system.file("cantabrica.db", package = "cantabricar")
    cmd <- paste0("sqlite3 ", db_path)
    if (db_path=="") system(cmd, intern = FALSE)
}

#' Connect to database
#' @export db_set_password
#' @importFrom keyring key_set
db_set_password <- function()
{
    key_set("cantabrica", "cantabrica-admin")
}

#' Check database password
#' @export db_check_password
#' @importFrom keyring key_get
#' @importFrom keyring key_set
db_check_password <- function()
{
    x <- tryCatch(
        {db_set_password()},
        error = function(cond) db_set_password()
    )
    return(x)
}

#' Connect to SQL local host
#' @export db_connect
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @importFrom keyring key_get
db_connect <- function()
{
    db_path <- system.file("cantabrica.db", package = "cantabricar")
    dbConnect(SQLite(), db_path)
}


#' Create new tables if they don't exist
#' @export db_create_tables
#' @importFrom glue glue_sql
#' @importFrom DBI dbExecute
#' @param con connection to database, as returned by \code{db_connect}
db_create_tables <- function(con)
{
    suppressWarnings({
        qrys <- list.files(
            system.file("SQL", package = "cantabricar", mustWork = TRUE),
            pattern = "create-", full.names = TRUE
        ) %>%
            lapply(
                function(x)
                {
                    qry_lines <- paste0(readLines(x), collapse = "\n")
                    qry <- glue_sql(qry_lines)
                    return(qry)
                }
            )

        lapply(qrys, function(x) dbExecute(con, x))
    })
    message("Tables created successfully")
}

#' Add values if table 'valores' is empty
#' @export db_fill_values
#' @importFrom DBI dbGetQuery
#' @importFrom purrr map_lgl
#' @param con connection to database, as returned by \code{db_connect}
db_fill_values <- function(con)
{
    val <- db_get_values(con)
    val_lgl <- (map_lgl(val, function(x) length(x) < 1))
    if (all(val_lgl))
    {
        new_val <- read.csv(system.file("valores.csv", package = "cantabricar", mustWork = TRUE))
        db_add_values(
            con,
            tipo = new_val$tipo,
            valor = new_val$valor
        )
    }
}

#' Add values to all tables other than 'valores'
#' @export db_fill_tables
#' @param con connection to database, as returned by \code{db_connect}
#' @param n number of observations to simulate
db_fill_tables <- function(con, n = 50)
{
    values <- db_get_values(con)

    # simulate dates, drawing date lags between steps from Poisson distribution (in seconds)
    f_siembras <- sample(seq(as.POSIXct("2021/01/01 12:00:00"), as.POSIXct("2021/01/20 12:00:00"), by = "hour"), n, replace = TRUE)
    f_germinaciones <- f_siembras + rpois(n, 10^6) + floor(rnorm(n, 0, 10^5))
    f_hojas <- f_germinaciones + rpois(n, 10^6) + floor(rnorm(n, 0, 10^5))
    f_cosechas <- f_hojas + rpois(n, 10^6) + floor(rnorm(n, 0, 10^5))

    comment_strings <- replicate(n, paste(sample(c(0:9, letters, LETTERS), 20, replace = TRUE), collapse = ""))

    db_add_row(
        con, "plantas",
        data.frame(
            id = 1:50,
            especie = sample(values$especie, size = n, replace = TRUE),
            variedad = sample(values$variedad, size = n, replace = TRUE),
            marca = sample(values$marca, size = n, replace = TRUE),
            planta_tipo = sample(values$planta_tipo, size = n, replace = TRUE)
        )
    )
    db_add_row(
        con, "siembras",
        data.frame(
            id = 1:50,
            fecha_siembra = as.character(f_siembras),
            medio_siembra = "Bizcochito",
            luz = sample(c(TRUE, FALSE), size = n, replace = TRUE),
            peso = sample(c(TRUE, FALSE), size = n, replace = TRUE),
            calor = sample(c(TRUE, FALSE), size = n, replace = TRUE),
            domo = sample(c(TRUE, FALSE), size = n, replace = TRUE),
            peso_semillas = rpois(n = n, lambda = 100),
            comentarios = comment_strings
        ))
    db_add_row(con, "germinaciones", data.frame(id = 1:n, fecha_germinacion = as.character(f_germinaciones), comentarios = comment_strings))
    db_add_row(con, "hojas", data.frame(id = 1:n, fecha_hojas = as.character(f_hojas), comentarios = comment_strings))
    db_add_row(con, "cosechas", data.frame(id = 1:n, fecha_cosecha = as.character(f_cosechas), comentarios = comment_strings))
}

#' Empty all values in all tables but 'valores'
#' @export db_empty_tables
#' @importFrom DBI dbExecute
#' @importFrom purrr map
#' @param con connection to database, as returned by \code{db_connect}
db_empty_tables <- function(con)
{
    invisible({
        qrys <- paste0("DELETE FROM ", dbListTables(con)[-which(dbListTables(con)=="valores")])
        map(qrys, ~dbExecute(con, .))
    })
}

#' Delete tables
#' @export db_delete_tables
#' @importFrom DBI dbRemoveTable
#' @importFrom DBI dbListTables
#' @param con connection to database, as returned by \code{db_connect}
db_delete_tables <- function(con)
{
    invisible({
        db_list <- dbListTables(con)
        map(db_list, ~dbRemoveTable(con, .))
    })
}

#' Retrieve all data
#' @export db_get_data
#' @importFrom DBI dbReadTable
#' @importFrom purrr map
#' @importFrom dplyr mutate_at
#' @importFrom dplyr one_of
#' @param con connection to database, as returned by \code{db_connect}
db_get_data <- function(con)
{
    x <- list(
        plantas = dbReadTable(con, "plantas"),
        siembras = dbReadTable(con, "siembras"),
        germinaciones = dbReadTable(con, "germinaciones"),
        hojas = dbReadTable(con, "hojas"),
        cosechas = dbReadTable(con, "cosechas")
    ) %>%
        map(
            function(x)
            {
                mutate_at(
                    x, vars(any_of(c("domo", "luz", "calor"))),
                    function(y) as.logical(as.integer(y))
                )
            }
        )
    return(x)
}

#' Summarise all data into a table
#' @export db_summarise
#' @import dplyr
#' @importFrom DBI dbReadTable
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @param con connection to database, as returned by \code{db_connect}
db_summarise <- function(con, data = NULL)
{
    if (is.null(data)) data <- db_get_data(con)

    suppressMessages({
        x <- map(
            data, function(x)
            {
                y <- x %>%
                    select(-matches("comentarios")) %>%
                    mutate_at(vars(matches("id")), as.integer)
                return(y)
            }
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
            arrange(-id)
    })
    return(x)
}

#' @rdname db_summarise
#' @export
db_summarize <- db_summarise

#' Get values for UI choices
#' @export db_get_values
#' @importFrom DBI dbReadTable
#' @param con connection to database, as returned by \code{db_connect}
db_get_values <- function(con)
{
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

#' Add new values to UI choices
#' @export db_add_values
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbReadTable
#' @param con connection to database, as returned by \code{db_connect}
#' @param tipo character string indicating the type of value to add: "especie",
#' "variedad", "marca", "medio_siembra", "planta_tipo", or "luz"
#' @param valor character string with the value
db_add_values <- function(con, tipo, valor)
{
    df <- data.frame(tipo = tipo, valor = valor)
    x <- dbWriteTable(con, "valores", df, append = TRUE)
    return(dbReadTable(con, "valores"))
}

#' Add new row to SQL table
#' @export db_add_row
#' @importFrom DBI dbWriteTable
#' @importFrom DBI dbReadTable
#' @param con connection to database, as returned by \code{db_connect}
#' @param table character string indicating the table to which the new row should be added
#' @param ... variables to add
db_add_row <- function(con, table, ...)
{
    df <- data.frame(...)
    x <- dbWriteTable(con, table, df, append = TRUE)
    x <- dbReadTable(con, table)
    return(x)
}

#' Delete row from SQL table
#' @export db_delete_row
#' @importFrom DBI dbExecute
#' @importFrom DBI dbReadTable
#' @param con connection to database, as returned by \code{db_connect}
#' @param table character string indicating the table from which a row should be deleted
#' @param id character string indicating the ID of the row (plant) to be deleted
db_delete_row <- function(con, table, id)
{
    qry <- paste0("DELETE FROM ", table, " WHERE id=", id, ";")
    x <- dbExecute(con, statement = qry)
    x <- dbReadTable(con, table)
    return(x)
}

