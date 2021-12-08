-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/* crear tabla de hojas */
CREATE TABLE IF NOT EXISTS hojas (
    id INTEGER PRIMARY KEY UNIQUE,
	fecha_hojas DATETIME NOT NULL,
    comentarios TEXT
) WITHOUT ROWID;

