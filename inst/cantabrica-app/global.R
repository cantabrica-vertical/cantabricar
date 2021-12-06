# run this script when app is launched

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
library(shinyWidgets)
library(tidyr)
library(toastui)

# import data ----
db_create()
con <- db_connect()
db_create_tables(con)
db_fill_values(con)
values <- db_get_values(con)
d <- db_get_data(con)
d_all <- db_summarise(con, d)

rds_path <- system.file("RDS", "fit_germinacion.rds", package = "cantabricar")
if (rds_path=="") {
    fit_germinacion <- fit_model(type = "germinacion", y = "t_germinacion", data = d_all)
    fit_hojas <- fit_model(type = "hojas", y = "t_hojas", data = d_all)
    fit_cosecha <- fit_model(type = "cosecha", y = "t_cosecha", data = d_all)
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
source("ui_tab_hojas.R")
source("ui_tab_cosecha.R")
