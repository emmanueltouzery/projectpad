CREATE TABLE command_to_run (id INTEGER PRIMARY KEY,
	command_desc TEXT NOT NULL,
	command_path TEXT NOT NULL,
	command_text TEXT NOT NULL,
	server_id INTEGER NULL, -- if null it's the developer's machine
	FOREIGN KEY(server_id) REFERENCES server(id));

CREATE TABLE point_of_interest (id INTEGER PRIMARY KEY,
	poi_description TEXT NOT NULL,
	poi_location TEXT NOT NULL,
	poi_interest_type INTEGER NOT NULL,
	server_id INTEGER NULL, -- if null it's the developer's machine
	FOREIGN KEY(server_id) REFERENCES server(id));

CREATE TABLE server (id INTEGER PRIMARY KEY,
	server_desc TEXT NOT NULL,
	server_ip TEXT NOT NULL,
	server_username TEXT NOT NULL,
	server_password TEXT NOT NULL,
	server_type INTEGER NOT NULL);

CREATE TABLE project (id INTEGER PRIMARY KEY,
	project_name TEXT NOT NULL,
	project_icon BLOB NOT NULL);

CREATE TABLE db_version (id INTEGER PRIMARY KEY,
	version_code INTEGER NOT NULL,
	upgrade_date TEXT NOT NULL);

INSERT INTO db_version (version_code, upgrade_date) VALUES (1, datetime('now'));
