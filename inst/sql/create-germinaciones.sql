-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/* crear tabla de hojas */
CREATE TABLE IF NOT EXISTS germinaciones (
    id INTEGER PRIMARY KEY UNIQUE,
	fecha_germinacion DATETIME NOT NULL,
    comentarios TEXT
) WITHOUT ROWID;


