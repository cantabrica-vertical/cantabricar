# run this script when app is launched

# load packages
library(tidyr)
library(stringr)
library(cantabricar)
library(cmdstanr)
library(posterior)
library(bayesplot)
library(ggplot2)
library(plotly)
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

# import data ----
con <- get_connection()
values <- get_values(con)
d <- get_data(con)
d_all <- summarise_data(con)
estanterias <- get_estanterias()

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
