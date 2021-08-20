# run this script when app is launched

# load packages
library(tidyr)
library(stringr)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggplot2)
library(purrr)
library(dplyr)
library(dbplyr)
library(DT)
library(DBI)
library(odbc)
library(keyring)
library(shiny)
library(shinyjs)
library(shinyWidgets)
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
            data, function(x) select(x, -matches("comentarios")) %>%
                mutate_at(vars(matches("id")), as.character)
        ) %>%
            reduce(left_join) %>%
            rowwise() %>%
            mutate(
                t_germinacion = as.numeric(difftime(fecha_germinacion, fecha_siembra, units = "days")),
                t_hojas = as.numeric(difftime(fecha_hojas, fecha_siembra, units = "days")),
                t_cosecha = as.numeric(difftime(fecha_cosecha, fecha_siembra, units = "days")),
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
        planta_tipo = unique(tb$valor[tb$tipo=="planta_tipo"]),
        luz = unique(tb$valor[tb$tipo=="luz"])
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

# fit models

#' Fit germinacion model
#' @import cmdstanr
#' @param type Type of data to fit. One of "germinacion", "hojas", or "cosecha".
#' @param data Data frame with a row for each plant and at least one column with the germination days
#' @param lambda_prior Prior distribution of the lambda parameter in the Poisson distribution used to model germination days
#' @param save Path where the CmdStanFit object should be saved (as a RDS file)
#' @param ... Arguments to be passed to the \code{$sample} operator
fit_model <- function(
    type,
    data = NULL,
    lambda_prior_alpha = 15,
    lambda_prior_beta = 1,
    save = NULL,
    ...
){
    if (is.null(data)) data <- data.frame(y = double())
    if (type=="germinacion") stan_code <- system.file("stan/germinacion.stan", package = "cantabricar")
    if (type=="hojas") stan_code <- system.file("stan/hojas.stan", package = "cantabricar")
    if (type=="cosecha") stan_code <- system.file("stan/cosecha.stan", package = "cantabricar")
    data_stan <- list(N = nrow(data), y = data$y, lambda_prior_alpha = lambda_prior_alpha, lambda_prior_beta = lambda_prior_beta)
    mod <- cmdstan_model(stan_code)
    fit <- mod$sample(data = data_stan, ...)
    if (!is.null(save)) fit$save_object(file = save)
    return(fit)
}


#' Diagnose germination model using CmdStan diagnostics
#' @import cmdstanr
#' @param fit CmdStanFit object (output from \code{fit_germination})
get_diagnostics <- function(fit){
    fit$cmdstan_diagnose()
}

#' Leave-one-out cross-validation using
#' @import cmdstanr
#' @param fit CmdStanFit object (output from \code{fit_germination})
get_loo <- function(fit){
    fit$loo()
}

#' Get posterior draws
#' @import cmdstanr
#' @importFrom posterior as_draws_df
#' @importFrom tidyr pivot_longer
#' @param fit CmdStanFit object (output from \code{fit_germination})
get_draws <- function(
    fit
){
    suppressWarnings({
        draws <- fit$draws() %>%
            as_draws_df() %>%
            pivot_longer(
                -one_of(".chain", ".iteration", ".draw"),
                names_to = "variable",
                values_to = "value"
            )

        return(draws)
    })
}

#' Get posterior predictions
#' @import cmdstanr
#' @importFrom rjson fromJSON
#' @importFrom posterior as_draws_matrix
#' @importFrom bayesplot ppc_dens_overlay
#' @param fit CmdStanFit object (output from \code{fit_germination})
#' @param n Number of samples per observation
get_post_preds <- function(
    fit,
    n = 20
){
    library(cmdstanr)
    library(rjson)
    data <- fromJSON(paste(readLines(fit$data_file()), collapse=""))
    draws <- fit$draws()
    preds <- draws[,,dimnames(draws)$variable[grepl("y_rep", dimnames(draws)$variable)]] %>%
        as_draws_matrix()
    preds$
        ppc_dens_overlay(y = data$y, yrep = preds[1:n, 0:data$N])
}

#' Get prior predictions
#' @import cmdstanr
#' @importFrom rjson fromJSON
#' @importFrom posterior as_draws_matrix
#' @importFrom bayesplot ppc_dens_overlay
#' @param fit CmdStanFit object (output from \code{fit_germination})
#' @param n Number of samples per observation
get_prior_preds <- function(
    fit,
    n = 20
){
    library(cmdstanr)
    library(rjson)
    data <- fromJSON(paste(readLines(fit$data_file()), collapse=""))
    draws <- fit$draws()
    preds <- draws[,,dimnames(draws)$variable[grepl("y_sim", dimnames(draws)$variable)]] %>%
        as_draws_matrix()
    ppc_dens_overlay(y = data$y, yrep = preds[1:n, 0:data$N])
}

#' Plot MCMC posterior draws as a histogram
#' @import cmdstanr
#' @import ggplot2
#' @importFrom dplyr mutate
#' @param draws Posterior draws from a CmdStanFit object (output from \code{draws_germination})
plot_model <- function(
    fit
){

    mcmc_hist(fit$draws(), "lambda", "lambda_sim")$data %>%
        mutate(
            parameter = ifelse(grepl("_sim", Parameter), "Previa", "Posterior"),
            parameter = factor(parameter, levels = c("Previa", "Posterior"))
        )%>%
        ggplot(aes(value)) +
        annotate(geom = "rect", xmin = fit$summary()$q5[2], xmax = fit$summary()$q95[2], ymin = -Inf, ymax = Inf,
                 colour = "white", fill = "grey", alpha = 0.5) +
        geom_histogram(
            aes(fill = parameter, colour = parameter),
            colour = "white", alpha = 0.75, bins = 30, position = position_identity()) +
        geom_vline(xintercept = fit$summary()$median[2], colour = "black") +
        labs(
            x = expression(lambda),
            y = "Muestras",
            colour = "Distribuci\u00f3n",
            fill = "Distribuci\u00f3n",
            parse = TRUE
        ) +
        scale_color_brewer(palette = "Dark2") +
        scale_fill_brewer(palette = "Dark2") +
        theme_minimal() +
        theme(
            legend.position = "top",
            legend.title = element_blank(),
            text = element_text(colour = "black", size = 12),
            axis.text = element_text(colour = "black", size = 12),
            axis.title = element_text(colour = "black", size = 15)
        )
}


# import data ----
con <- get_connection()
values <- get_values(con)
d <- get_data(con)
d_all <- summarise_data(con)
rds_path <- system.file("RDS", "fit_germinacion.rds", package = "cantabricar", mustWork = TRUE)
if (rds_path=="") {
    fit_germinacion <- fit_model(
        type = "germinacion",
        save = system.file("RDS", "fit_germinacion.rds", package = "cantabricar", mustWork = TRUE),
        lambda_prior_alpha = 15,
        lambda_prior_beta = 1
        )
    fit_hojas <- fit_model(
        type = "hojas",
        save = system.file("RDS", "fit_hojas.rds", package = "cantabricar", mustWork = TRUE),
        lambda_prior_alpha = 15,
        lambda_prior_beta = 1

        )
    fit_cosecha <- fit_model(
        type = "cosecha",
        save = system.file("RDS", "fit_cosecha.rds", package = "cantabricar", mustWork = TRUE),
        lambda_prior_alpha = 15,
        lambda_prior_beta = 1
        )
} else {
    fit_germinacion <- readRDS(system.file("RDS", "fit_germinacion.rds", package = "cantabricar", mustWork = TRUE))
    fit_hojas <- readRDS(system.file("RDS", "fit_hojas.rds", package = "cantabricar", mustWork = TRUE))
    fit_cosecha <- readRDS(system.file("RDS", "fit_cosecha.rds", package = "cantabricar", mustWork = TRUE))
}

# source modules ----
source("ui_tab_dashboard.R")
source("ui_tab_calendario.R")
source("ui_tab_datos.R")
source("ui_tab_estimaciones.R")
source("ui_tab_siembra.R")
source("ui_tab_germinacion.R")
source("ui_tab_trasplante.R")
source("ui_tab_hojas.R")
source("ui_tab_cosecha.R")
