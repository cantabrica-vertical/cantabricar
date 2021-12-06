-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/* crear tabla de cosechas */
CREATE TABLE IF NOT EXISTS cosechas (
    id INTEGER PRIMARY KEY UNIQUE,
	fecha_cosecha DATETIME NOT NULL,
    comentarios TEXT
) WITHOUT ROWID;

