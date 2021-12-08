library(cantabricar)

con <- db_connect(test = TRUE)
db_create_tables(con)
d <- db_get_data(con)
d_all <- as.data.frame(db_summarise(con))

test_that("db_connect: connection to database can be established", {
    expect_error(db_connect(test = TRUE), NA)
})

test_that("db_create_tables: tables can be created", {
    expect_error(db_create_tables(con), NA)
})

test_that("db_fill_values: table 'valores' can be filled", {
    expect_error(db_fill_values(con), NA)
})

test_that("db_fill_tables: all tables other than 'valores' can be filled", {
    expect_error(db_fill_tables(con, n = 10), NA)
})

test_that("db_get_data: data can be extracted", {
    expect_error(db_get_data(con), NA)
})

test_that("db_summarise/db_summarize: data can be summarised", {
    expect_error(db_summarise(con), NA)
    expect_error(db_summarize(con), NA)
})

test_that("get_connection returns a SQL Server connection", {
    expect_type(con, "S4")
})

test_that("db_get_data returns a list of data frames", {
    expect_type(d, "list")
    expect_named(d, c("plantas", "siembras", "germinaciones", "hojas", "cosechas"))
    expect_true(all(unlist(lapply(d, class))=="data.frame"))
})

test_that("data frames returned by db_get_data have the right column names", {
    expect_true(all(c("id", "especie", "variedad", "marca", "planta_tipo") %in% names(d$plantas)))
    expect_true(all(c("id", "medio_siembra", "peso", "peso_semillas", "comentarios", "calor", "domo", "fecha_siembra") %in% names(d$siembras)))
    expect_true(all(c("id", "comentarios", "fecha_germinacion") %in% names(d$germinaciones)))
    expect_true(all(c("id", "fecha_hojas", "comentarios") %in% names(d$hojas)))
    expect_true(all(c("id", "fecha_cosecha", "comentarios") %in% names(d$cosechas)))
})

test_that("data frames returned by db_get_data have are the right classes", {
    # plantas
    expect_type(d$plantas$id, "integer")
    expect_type(d$plantas$especie, "character")
    expect_type(d$plantas$variedad, "character")
    expect_type(d$plantas$marca, "character")
    expect_type(d$plantas$planta_tipo, "character")
    # sembradas
    expect_type(d$siembras$id, "integer")
    expect_type(d$siembras$medio_siembra, "character")
    expect_type(d$siembras$peso, "logical")
    expect_type(d$siembras$peso_semillas, "double")
    expect_type(d$siembras$comentarios, "character")
    expect_type(d$siembras$luz, "logical")
    expect_type(d$siembras$calor, "logical")
    expect_type(d$siembras$domo, "logical")
    expect_error(as.POSIXct(d$siembras$fecha_siembra), NA)
    # germinadas
    expect_type(d$germinaciones$id, "integer")
    expect_type(d$germinaciones$comentarios, "character")
    expect_error(as.POSIXct(d$siembras$fecha_siembra), NA)
    # hojas
    expect_type(d$hojas$id, "integer")
    expect_type(d$hojas$comentarios, "character")
    expect_error(as.POSIXct(d$siembras$fecha_siembra), NA)
    # cosechadas
    expect_type(d$cosechas$id, "integer")
    expect_type(d$cosechas$comentarios, "character")
    expect_error(as.POSIXct(d$siembras$fecha_siembra), NA)
})


test_that("db_summarise returns a data frame with the right classes", {
    expect_true("data.frame" %in% class(d_all))
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
    expect_true(all(class(d_all$fecha_cosecha) %in% c("POSIXct", "POSIXt")))
    expect_true(all(class(d_all$t_germinacion)=="difftime"))
    expect_true(all(class(d_all$t_hojas)=="difftime"))
    expect_true(all(class(d_all$t_cosecha)=="difftime"))
})

test_that("db_empty_tables: tables can be emptied", {
    expect_error(db_empty_tables(con), NA)
})

test_that("db_delete_tables: tables can be deleted", {
    expect_error(db_delete_tables(con), NA)
})



