var actions = {
    "PoiLogFile": {"text": "Open", "icon": "glyphicons-30-notes-2"},
    "PoiCommandToRun": {"text": "Run", "icon": "glyphicons-138-cogwheels"},
    "PoiCommandTerminal": {"text": "Run", "icon": "glyphicons-138-cogwheels"},
    "PoiApplication":{"text": "Open", "icon": "glyphicons-149-folder-flag"},
    "PoiConfigFile":{"text": "Open", "icon": "glyphicons-281-settings"},
    "PoiBackupArchive":{"text": "Open", "icon": "glyphicons-58-history"}
}


function envDesc(environment) {
    switch (environment) {
    case "EnvDevelopment":
        return "Development"
    case "EnvUat":
        return "UAT"
    case "EnvStage":
        return "Staging"
    case "EnvProd":
        return "PROD"
    }
}
