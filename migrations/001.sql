CREATE TABLE command_to_run (id INTEGER PRIMARY KEY,
	desc TEXT NOT NULL,
	path TEXT NOT NULL,
	text TEXT NOT NULL,
	server_id INTEGER NULL,
	FOREIGN KEY(server_id) REFERENCES server(id));

CREATE TABLE point_of_interest (id INTEGER PRIMARY KEY,
	description TEXT NOT NULL COLLATE NOCASE,
	location TEXT NOT NULL,
	interest_type INTEGER NOT NULL,
	server_id INTEGER NULL,
	FOREIGN KEY(server_id) REFERENCES server(id));

CREATE TABLE server (id INTEGER PRIMARY KEY,
	desc TEXT NOT NULL COLLATE NOCASE,
	ip TEXT NOT NULL,
	username TEXT NOT NULL,
	password TEXT NOT NULL,
	project_id INTEGER NOT NULL,
	type INTEGER NOT NULL,
	FOREIGN KEY(project_id) REFERENCES project(id));

CREATE TABLE project (id INTEGER PRIMARY KEY,
	name TEXT NOT NULL COLLATE NOCASE,
	icon BLOB NOT NULL);

CREATE TABLE db_version (id INTEGER PRIMARY KEY,
	code INTEGER NOT NULL,
	upgrade_date TEXT NOT NULL);

INSERT INTO db_version (code, upgrade_date) VALUES (1, datetime('now'));
