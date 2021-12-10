-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/* crear tabla de siembras */
CREATE TABLE IF NOT EXISTS siembras (
    id INTEGER PRIMARY KEY UNIQUE,
	fecha_siembra DATETIME NOT NULL,
   	medio_siembra TEXT NOT NULL,
	luz TEXT,
	peso TEXT,
	calor TEXT,
	domo TEXT,
	peso_semillas TEXT,
    comentarios TEXT
) WITHOUT ROWID;

