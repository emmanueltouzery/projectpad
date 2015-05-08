function editServer(curServer, refreshAction) {
    popup.setContents("Edit server", serverEditComponent,
        function (serverEdit) {
            serverEdit.activate(curServer, curServer.environment)
        },
        function (serverEdit) {
            serverEdit.onOk()
            // force refresh
            refreshAction()
        })
}

function showSelectMenu(server, parnt, desktopSize, refreshAction, menu, global) {
    var options = [
        ["glyphicons-145-folder-open", function() { loadView("ServerView.qml", server) }],
        ["glyphicons-151-edit", function() {editServer(server, refreshAction)}],
        ["glyphicons-512-copy", function() { appContext.copyItem(server.password, true) }],
        ["glyphicons-193-circle-remove", function() {
            appContext.confirmDelete(function() {
                var msg = projectViewState.canDeleteServer(server)
                if (msg !== null) {
                    appContext.errorMessage(msg)
                    return
                }
                projectViewState.deleteServers([server.id])
                // force refresh
                refreshAction()
            })
        }]]
    if (server.accessType === "SrvAccessWww") {
        options.push(["glyphicons-372-global", function() {
            openAssociatedFile(server.serverIp)
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
            var info = projectViewState.runRdp(server,
                Math.round(desktopWidth * 0.75),
                Math.round(desktopSize.height * 0.75))
            if (info[0] === "error") {
                appContext.errorMessage(info[1])
            }
        }])
    }
    if (server.accessType === "SrvAccessSsh"
            && server.username.length > 0
            && server.password.length > 0) {
        options.push(["glyphicons-489-multiple-displays", function() {
            var info = projectViewState.openSshSession(server)
            if (info[0] === "error") {
                appContext.errorMessage(info[1])
            }
        }])
    }
    menu.options = options
    menu.show(parnt || parent, global)
}
