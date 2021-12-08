app <- ShinyDriver$new("../../")
app$snapshotInit("test-database")

app$snapshot()
app$setInputs(datos_cols = c("id", "especie", "variedad", "marca", "peso", "fecha_siembra", "fecha_germinacion", "fecha_hojas", "fecha_cosecha"))
app$snapshot()
app$snapshotDownload("descargar")
app$snapshot()
