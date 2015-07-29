.import "utils.js" as Utils

function getServerIcon(server) {
    // a bit messy because I check several
    // independant criteria. but makes sense in a way.
    if (server.accessType === "SrvAccessWww") {
        return "glyphicons-372-global"
    } else if (server.type === "SrvDatabase") {
        return "glyphicons-343-hdd"
    } else if (server.accessType === "SrvAccessRdp") {
        return "../pics/windows_logo"
    } else {
        return "glyphicons-464-server"
    }
}

function editServer(project, curServer, refreshAction) {
    popup.setContents("Edit server", serverEditComponent,
        function (serverEdit) {
            serverEdit.activate(project, curServer, curServer.environment)
        },
        function (serverEdit) {
            serverEdit.onOk()
            // force refresh
            refreshAction()
        })
}

function showSelectMenu(project, server, parnt, desktopSize, refreshAction, menu, global) {
    var options = [
        ["glyphicons-145-folder-open", function() { loadView("ServerView.qml", server) }],
        ["glyphicons-151-edit", function() {editServer(project, server, refreshAction)}],
        ["glyphicons-512-copy", function() { appContext.copyItem(server.password, true) }],
        ["glyphicons-193-circle-remove", function() {
            appContext.confirmDelete(function() {
                var msg = getAppState().projectViewState.canDeleteServer(server)
                if (msg !== null) {
                    appContext.errorMessage(msg)
                    return
                }
                Utils.handleEitherVoid(getAppState().projectViewState.deleteServers([server.id]))
                // force refresh
                refreshAction()
            })
        }]]
    if (server.accessType === "SrvAccessWww") {
        options.push(["glyphicons-372-global", function() {
            getAppState().openAssociatedFile(server.serverIp)
        }])
    }
    if (server.authKeyFilename !== "...") {
        options.push(["glyphicons-45-keys", function() {
            saveAuthKeyDialog.server = server
            saveAuthKeyDialog.visible = true
        }])
    }
    if (server.accessType === "SrvAccessRdp"
            && server.username.length > 0
            && server.password.length > 0) {
        options.push(["glyphicons-489-multiple-displays", function() {
            var desktopWidth = desktopSize.width
            if (desktopSize.width / desktopSize.height > 3) {
                // I can assume a double monitor setup: divide the
                // width by two to compensate.
                desktopWidth = desktopWidth / 2
            }
            Utils.handleEitherVoid(
                getAppState().projectViewState.runRdp(
                    server,
                    Math.round(desktopWidth * 0.75),
                    Math.round(desktopSize.height * 0.75)))
        }])
    }
    if (server.accessType === "SrvAccessSsh"
            && server.username.length > 0
            && server.password.length > 0) {
        options.push(["glyphicons-489-multiple-displays", function() {
            Utils.runIfSshHostTrusted(server, function () {
                Utils.handleEitherVoid(getAppState().projectViewState.openSshSession(server))
            })
        }])
    }
    menu.options = options
    menu.show(parnt || parent, global)
}
