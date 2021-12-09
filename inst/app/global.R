# run this script when app is launched

# load packages ----
library(brms)
library(cantabricar)
library(DBI)
library(DT)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(purrr)
library(plotly)
library(shinydashboard)
library(shinybusy)
library(shinyjs)
library(gt)
library(tidyr)
library(tibble)
library(toastui)

# import data ----
con <- db_connect()
db_create_tables(con)
db_fill_values(con)
values <- db_get_values(con)
d <- db_get_data(con)
d_all <- db_summarise(con, d)

# import model_fits ----
base_path <- system.file("RDS", package = "cantabricar")
fit_files <- list.files(base_path)
rds_path_index <- which.max(as.Date(gsub("(.+?)(\\_.*)", "\\1", fit_files)))
rds_path <- fit_files[rds_path_index]

if (length(rds_path) < 1) {
    fit_germinacion <- fit_model(type = "germinacion", y = "t_germinacion", data = d_all)
    fit_hojas <- fit_model(type = "hojas", y = "t_hojas", data = d_all)
    fit_cosecha <- fit_model(type = "cosecha", y = "t_cosecha", data = d_all)
} else {
    # import germinacion fits
    fit_germinacion_paths_all <- list.files(base_path, pattern = "germinacion")
    fit_germinacion_index <- which.max(as.Date(gsub("(.+?)(\\_.*)", "\\1", fit_germinacion_paths_all)))
    fit_germinacion_path <- list.files(base_path, pattern = "germinacion", full.names = TRUE)[fit_germinacion_index]
    fit_germinacion <- readRDS(fit_germinacion_path)
    # import hojas fits
    fit_hojas_paths_all <- list.files(base_path, pattern = "hojas")
    fit_hojas_index <- which.max(as.Date(gsub("(.+?)(\\_.*)", "\\1", fit_hojas_paths_all)))
    fit_hojas_path <- list.files(base_path, pattern = "hojas", full.names = TRUE)[fit_hojas_index]
    fit_hojas <- readRDS(fit_hojas_path)
    # import cosecha fits
    fit_cosecha_paths_all <- list.files(base_path, pattern = "cosecha")
    fit_cosecha_index <- which.max(as.Date(gsub("(.+?)(\\_.*)", "\\1", fit_cosecha_paths_all)))
    fit_cosecha_path <- list.files(base_path, pattern = "cosecha", full.names = TRUE)[fit_cosecha_index]
    fit_cosecha <- readRDS(fit_cosecha_path)
}


# source modules ----
source("ui_tab_dashboard.R")
source("ui_tab_calendario.R")
source("ui_tab_datos.R")
source("ui_tab_estimaciones.R")
source("ui_tab_siembra.R")
source("ui_tab_germinacion.R")
source("ui_tab_hojas.R")
source("ui_tab_cosecha.R")
