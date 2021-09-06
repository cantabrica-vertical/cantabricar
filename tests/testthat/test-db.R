library(cantabricar)

con <- get_connection()
d <- get_data()
d_all <- as.data.frame(get_data_summary(con, d))
bandejas <- get_bandejas(con)
instalaciones <- get_instalaciones(con)
bandejas_vacias <- get_bandejas_vacias(con, d, instalaciones)

test_that("get_connection returns a SQL Server connection", {
    expect_type(con, "S4")
})

test_that("get_data returns a list of data frames", {
    expect_type(d, "list")
    expect_named(d, c("plantas", "sembradas", "germinadas", "hojas", "trasplantadas", "cosechadas"))
    expect_true(all(unlist(lapply(d, class))=="data.frame"))
})

test_that("data frames returned by get_data have the right column names", {
    expect_true(all(c("id", "especie", "variedad", "marca", "planta_tipo", "comentarios") %in% names(d$plantas)))
    expect_true(all(c("id", "medio_siembra", "peso", "peso_semillas", "comentarios", "calor", "domo", "fecha_siembra") %in% names(d$sembradas)))
    expect_true(all(c("id", "luz", "comentarios", "fecha_germinacion") %in% names(d$germinadas)))
    expect_true(all(c("id", "fecha_hojas", "tamano_hoja", "comentarios") %in% names(d$hojas)))
    expect_true(all(c("id", "medio_trasplante", "luz_tipo", "fecha_trasplante", "comentarios") %in% names(d$trasplantadas)))
    expect_true(all(c("id", "comentarios", "fecha_cosecha") %in% names(d$cosechadas)))
})

test_that("data frames returned by get_data have are the right classes", {
    # plantas
    expect_type(d$plantas$id, "integer")
    expect_type(d$plantas$especie, "character")
    expect_type(d$plantas$variedad, "character")
    expect_type(d$plantas$marca, "character")
    expect_type(d$plantas$planta_tipo, "character")
    expect_type(d$plantas$comentarios, "character")
    # sembradas
    expect_type(d$sembradas$id, "integer")
    expect_type(d$sembradas$medio_siembra, "character")
    expect_type(d$sembradas$peso, "logical")
    expect_type(d$sembradas$peso_semillas, "double")
    expect_type(d$sembradas$comentarios, "character")
    expect_type(d$sembradas$calor, "logical")
    expect_type(d$sembradas$domo, "logical")
    expect_true(all(class(d$sembradas$fecha_siembra) %in% c("POSIXct", "POSIXt")))
    # germinadas
    expect_type(d$germinadas$id, "integer")
    expect_type(d$germinadas$comentarios, "character")
    expect_type(d$germinadas$luz, "logical")
    expect_true(all(class(d$germinadas$fecha_germinacion) %in% c("POSIXct", "POSIXt")))
    # hojas
    expect_type(d$hojas$id, "integer")
    expect_type(d$hojas$comentarios, "character")
    expect_true(all(class(d$hojas$fecha_hojas) %in% c("POSIXct", "POSIXt")))
    # trasplantadas
    expect_type(d$trasplantadas$id, "integer")
    expect_type(d$trasplantadas$comentarios, "character")
    expect_type(d$trasplantadas$luz_tipo, "character")
    expect_true(all(class(d$trasplantadas$fecha_trasplante) %in% c("POSIXct", "POSIXt")))
    # cosechadas
    expect_type(d$cosechadas$id, "integer")
    expect_type(d$cosechadas$comentarios, "character")
    expect_true(all(class(d$cosechadas$fecha_cosecha) %in% c("POSIXct", "POSIXt")))
})


test_that("get_data_summary returns a data frame with the right classes", {
    expect_true(class(d_all)=="data.frame")
    expect_true(all(class(d_all$id)=="integer"))
    expect_true(all(class(d_all$especie)=="character"))
    expect_true(all(class(d_all$variedad)=="character"))
    expect_true(all(class(d_all$marca)=="character"))
    expect_true(all(class(d_all$planta_tipo)=="character"))
    expect_true(all(class(d_all$medio_siembra)=="character"))
    expect_true(all(class(d_all$peso)=="logical"))
    expect_true(all(class(d_all$peso_semillas)=="numeric"))
    expect_true(all(class(d_all$calor)=="logical"))
    expect_true(all(class(d_all$domo)=="logical"))
    expect_true(all(class(d_all$fecha_siembra) %in% c("POSIXct", "POSIXt")))
    expect_true(all(class(d_all$fecha_germinacion) %in% c("POSIXct", "POSIXt")))
    expect_true(all(class(d_all$fecha_hojas) %in% c("POSIXct", "POSIXt")))
    expect_true(all(class(d_all$fecha_trasplante) %in% c("POSIXct", "POSIXt")))
    expect_true(all(class(d_all$fecha_cosecha) %in% c("POSIXct", "POSIXt")))
    expect_true(all(class(d_all$fecha_bandeja) %in% c("POSIXct", "POSIXt")))
    expect_true(all(class(d_all$tamano_hoja)=="character"))
    expect_true(all(class(d_all$medio_trasplante)=="character"))
    expect_true(all(class(d_all$luz_tipo)=="character"))
    expect_true(all(class(d_all$t_germinacion)=="difftime"))
    expect_true(all(class(d_all$t_hojas)=="difftime"))
    expect_true(all(class(d_all$t_cosecha)=="difftime"))
    expect_true(all(class(d_all$balda)=="integer"))
    expect_true(all(class(d_all$bandeja)=="integer"))
})

test_that("get_bandejas returns a data frame with the right column names and classes", {
    expect_true(class(bandejas)=="data.frame")
    expect_named(bandejas, c("id", "balda", "bandeja", "fecha_bandeja", "estanteria"))
    expect_type(bandejas$id, "integer")
    expect_type(bandejas$balda, "integer")
    expect_type(bandejas$estanteria, "integer")
    expect_type(bandejas$bandeja, "integer")
    expect_true(all(class(bandejas$fecha_bandeja) %in% c("POSIXct", "POSIXt")))
})

test_that("get_instalaciones returns a one-row data frame with the right column names and classes", {
    expect_true(class(instalaciones)=="data.frame")
    expect_named(instalaciones, c("max_bandejas", "max_baldas", "fecha_instalaciones", "max_estanterias"))
    expect_type(instalaciones$max_baldas, "integer")
    expect_type(instalaciones$max_bandejas, "integer")
    expect_type(instalaciones$max_estanterias, "integer")
    expect_true(all(class(instalaciones$fecha_instalaciones) %in% c("POSIXct", "POSIXt")))
})

test_that("get_bandejas_vacias returns a character vector with the empty trays", {
    expect_type(bandejas_vacias, "character")
})


