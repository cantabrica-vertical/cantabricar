-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

/* crear tabla de valores */
CREATE TABLE IF NOT EXISTS valores (
    tipo TEXT NOT NULL,
	valor TEXT NOT NULL
);
