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
