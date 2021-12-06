-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/* crear tabla de plantas individuales */
CREATE TABLE IF NOT EXISTS plantas (
    id INTEGER PRIMARY KEY UNIQUE,
	especie TEXT NOT NULL,
   	variedad TEXT NOT NULL,
	marca TEXT,
	planta_tipo TEXT
) WITHOUT ROWID;
