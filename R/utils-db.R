#' Connect to SQL local host
#' @export db_connect
#' @importFrom DBI dbConnect
#' @importFrom RSQLite SQLite
#' @param test should connection be established with the cantabrica-test.db?
db_connect <- function(test = FALSE)
{
    if (test){
        db_path <- file.path(system.file("db", package = "cantabricar"), "cantabrica-test.db")
    } else {
        db_path <- file.path(system.file("db", package = "cantabricar"), "cantabrica.db")
    }
    dbConnect(SQLite(), db_path)
}


#' Create new tables if they don't exist
#' @export db_create_tables
#' @importFrom glue glue_sql
#' @importFrom DBI dbExecute
#' @param con connection to database, as returned by \code{db_connect}
#' @param tables character vector indicating the name of the tables to create ("plantas", "siembras", "germinaciones", "hojas", "cosechas", and "valores" by default)))
#' @param fill logical values indicating whether values should be filled in using db_fill_tables
db_create_tables <- function(
    con,
    tables = c("plantas", "siembras", "germinaciones", "hojas", "cosechas", "valores"),
    fill = FALSE
)
{
    if (!all(tables %in% dbListTables(con)))
    {
        suppressWarnings({
            qrys <- list.files(
                system.file("sql", package = "cantabricar", mustWork = TRUE),
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
    } else {
        message("All tables exist already")
    }

    if (fill)
    {
        db_fill_values(con)
        db_fill_tables(con)
    }
}

#' Add values if table 'valores' is empty
#' @export db_fill_values
#' @importFrom DBI dbGetQuery
#' @importFrom purrr map_lgl
#' @param con connection to database, as returned by \code{db_connect}
db_fill_values <- function(con)
{
    val <- db_get_values(con)
    val_lgl <- map_lgl(val, function(x) length(x) < 1)
    if (all(val_lgl)){
        db_add_values(
            con,
            tipo = valores$tipo,
            valor = valores$valor
        )
    }

    if (interactive()){
        message("Table 'valores' filled")
    }
}

#' Add values to all tables other than 'valores'
#' @export db_fill_tables
#' @param con connection to database, as returned by \code{db_connect}
#' @param n number of observations to simulate (defaults to 200)
db_fill_tables <- function(con, n = 200)
{
    values <- db_get_values(con)

    tbl_ls <- dbListTables(con)
    tables <- c("plantas", "siembras", "germinaciones", "hojas", "cosechas", "valores")

    if (!all(tbl_ls %in% tables)) db_create_tables(con)

    # simulate dates, drawing date lags between steps from Poisson distribution (in seconds)
    f_siembras <- sample(seq(as.POSIXct("2021/01/01 12:00:00", tz = "CET"), as.POSIXct("2021/01/20 12:00:00", tz = "CET"), by = "hour"), n, replace = TRUE)
    f_germinaciones <- f_siembras + rpois(n, 10^6) + floor(rnorm(n, 0, 10^5))
    f_hojas <- f_germinaciones + rpois(n, 10^6) + floor(rnorm(n, 0, 10^5))
    f_cosechas <- f_hojas + rpois(n, 10^6) + floor(rnorm(n, 0, 10^5))

    comment_strings <- replicate(n, paste(sample(c(0:9, letters, LETTERS), 20, replace = TRUE), collapse = ""))

    db_add_row(
        con, "plantas",
        data.frame(
            id = 1:n,
            especie = sample(values$especie, size = n, replace = TRUE),
            variedad = sample(values$variedad, size = n, replace = TRUE),
            marca = sample(values$marca, size = n, replace = TRUE),
            planta_tipo = sample(values$planta_tipo, size = n, replace = TRUE)
        )
    )
    db_add_row(
        con, "siembras",
        data.frame(
            id = 1:n,
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

    if (interactive()){
        message("Tables filled")
    }
}

#' Empty all values in all tables but 'valores'
#' @export db_empty_tables
#' @importFrom DBI dbExecute
#' @param con connection to database, as returned by \code{db_connect}
db_empty_tables <- function(con)
{
    tbl_ls <- dbListTables(con)[-which(dbListTables(con)=="valores")]
    if (length(tbl_ls) > 0){
        invisible({
            qrys <- paste0("DELETE FROM ", tbl_ls)
            lapply(qrys, function(x) dbExecute(con, x))
        })
        if (interactive()){
            message("Tables emptied")
        }
    } else {
        if (interactive()){
            message("Tables do not exist")
        }
    }
}

#' Delete tables
#' @export db_delete_tables
#' @importFrom DBI dbRemoveTable
#' @importFrom DBI dbListTables
#' @param con connection to database, as returned by \code{db_connect}
db_delete_tables <- function(con)
{
    tbl_ls <- dbListTables(con)
    if (length(tbl_ls) > 0)
    {
        invisible({
            db_list <- dbListTables(con)
            lapply(db_list, function(x) dbRemoveTable(con, x))
        })
        if (interactive()){
            message("Tables deleted")
        }
    } else {
        if (interactive()){
            message("No tables to delete")
        }
    }
}

#' Retrieve all data
#' @export db_get_data
#' @importFrom DBI dbReadTable
#' @importFrom dplyr mutate_at
#' @importFrom dplyr one_of
#' @param con connection to database, as returned by \code{db_connect}
db_get_data <- function(con)
{
    tbl_ls <- dbListTables(con)[-which(dbListTables(con)=="valores")]

    if (length(tbl_ls) > 0)
    {

        x <- list(
            plantas = dbReadTable(con, "plantas"),
            siembras = dbReadTable(con, "siembras"),
            germinaciones = dbReadTable(con, "germinaciones"),
            hojas = dbReadTable(con, "hojas"),
            cosechas = dbReadTable(con, "cosechas")
        ) %>%
            lapply(
                function(x)
                {
                    x %>%
                        mutate_at(
                            vars(any_of(c("domo", "luz", "calor", "peso"))),
                            function(y) as.logical(as.integer(y))
                        ) %>%
                        mutate_at(
                            vars(starts_with("fecha_")),
                            function(y) as.POSIXct(y)
                        ) %>%
                        mutate_at(
                            vars(any_of(c("peso_semillas"))),
                            function(y) as.numeric(y)
                        )
                }
            )
        return(x)
    } else {
        stop("Tables do not exist")
    }
}

#' Summarise all data into a table
#' @export db_summarise
#' @import dplyr
#' @importFrom DBI dbReadTable
#' @importFrom purrr map
#' @importFrom purrr reduce
#' @param con connection to database, as returned by \code{db_connect}
#' @param data datasets, as returned by \code{db_get_data}
db_summarise <- function(con, data = NULL)
{
    if (is.null(data)) data <- db_get_data(con)

    suppressMessages({
        x <- lapply(
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

