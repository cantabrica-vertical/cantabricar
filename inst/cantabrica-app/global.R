# run this script when app is launched

# load packages
library(tidyr)
library(stringr)
library(ggplot2)
library(purrr)
library(dplyr)
library(dbplyr)
library(DBI)
library(odbc)
library(keyring)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(shinybusy)
library(toastui)
library(lubridate)
library(scales)


# app utils

get_data <- function(con = NULL){
    if (is.null(con)) con <- get_connection()
    list(
        plantas = dbReadTable(con, "plantas"),
        sembradas = dbReadTable(con, "sembradas"),
        germinadas = dbReadTable(con, "germinadas"),
        hojas = dbReadTable(con, "hojas"),
        trasplantadas = dbReadTable(con, "trasplantadas"),
        cosechadas = dbReadTable(con, "cosechadas")
    )
}

summarise_data <- function(con = NULL, data = NULL){
    if (is.null(con)) con <- get_connection()
    if (is.null(data)) data <- get_data(con)

    suppressMessages({
        x <- map(
            data, ~select(., -matches("comentarios")) %>%
                mutate_at(vars(matches("id")), as.character)
        ) %>%
            reduce(left_join) %>%
            rowwise() %>%
            mutate(
                t_germinacion = difftime(fecha_germinacion, fecha_siembra, units = "days"),
                t_hojas = difftime(fecha_hojas, fecha_siembra, units = "days"),
                t_cosecha = difftime(fecha_cosecha, fecha_siembra, units = "days"),
            ) %>%
            ungroup() %>%
            arrange(desc(fecha_siembra), desc(id))
    })
    return(x)
}

get_values <- function(con = NULL){
    if (is.null(con)) con <- get_connection()
    tb <- dbReadTable(con, "valores")
    x <- list(
        especie = unique(tb$valor[tb$tipo=="especie"]),
        variedad = unique(tb$valor[tb$tipo=="variedad"]),
        marca = unique(tb$valor[tb$tipo=="marca"]),
        medio_siembra = unique(tb$valor[tb$tipo=="medio_siembra"]),
        planta_tipo = unique(tb$valor[tb$tipo=="planta_tipo"])
    )
    return(x)
}


add_values <- function(con = NULL, type, value){
    if (is.null(con)) con <- get_connection()
    df <- data.frame(tipo = type, valor = value)
    x <- dbWriteTable(con, "valores", df, append = TRUE)
    return(dbReadTable(con, "valores"))
}

add_data_row <- function(con = NULL, table, ...){
    if (is.null(con)) con <- get_connection()
    df <- data.frame(...)
    x <- dbWriteTable(con, table, df, append = TRUE)
    return(dbReadTable(con, table))
}


delete_data_row <- function(con = NULL, table, id){
    if (is.null(con)) con <- get_connection()
    qry <- paste0("DELETE FROM ", table, " WHERE id=", id, ";")
    x <- dbExecute(con, statement = qry)
    return(dbReadTable(con, table))
}

create_id <- function(x, y){
    number <- y %>% str_extract("\\d.*") %>% as.numeric() %>% max(na.rm = TRUE)
    x <- x %>% str_remove_all(" |-") %>%  paste0(., "_", number+1) %>% tolower()
    return(x)
}

#' Custom ggplot2 theme
#' @export theme_custom
#' @import ggplot2
theme_custom <- function() {
    theme_minimal() +
        theme(
            text = element_text(colour = "black", size = 12),
            axis.text = element_text(colour = "black", size = 12)
        )
}


set_password <- function(){
    key_set("cantabrica", "cantabrica-admin")
}

get_connection <- function(){
    dbConnect(
        odbc(),
        Driver = "SQL Server",
        server = "tcp:cantabrica.database.windows.net",
        database = "cantabrica",
        uid = "cantabrica-admin",
        pwd = key_get("cantabrica", "cantabrica-admin")
    )
}


# import data ----
con <- get_connection()
values <- get_values(con)
d <- get_data(con)
d_all <- summarise_data(con)

# source modules ----
source("ui_tab_dashboard.R")
source("ui_tab_calendario.R")
source("ui_tab_siembra.R")
source("ui_tab_germinacion.R")
source("ui_tab_trasplante.R")
source("ui_tab_hojas.R")
source("ui_tab_cosecha.R")
