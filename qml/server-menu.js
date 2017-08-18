.import "utils.js" as Utils
.import "remote-control.js" as Remote

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

function showSelectMenu(project, server, parnt, desktopSize,
                        refreshAction, menu, global, overrides) {
    var customEdit = null
    var customDelete = null
    if (overrides) {
        customEdit = overrides.edit
        customDelete = overrides.delete
    }
    var options = [
        ["glyphicons-145-folder-open", function() { loadView("ServerView.qml", server, parnt.tileId(), null) }],
        ["glyphicons-151-edit", customEdit || function() {editServer(project, server, refreshAction)}],
        ["glyphicons-512-copy", function() {
            appContext.copyItemEntity("ServerEntityType", server.id, true)
        }],
        ["glyphicons-193-circle-remove", customDelete || function() {
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
            Utils.handleEither(
                getAppState().projectViewState.saveAuthKey(server),
                function(location) { successMessage("Saved file to " + location) })
        }])
    }
    options = options.concat(Remote.tileRemoteControlOptions(
        server, desktopSize,
        function(w, h) {
            var result = getAppState().projectViewState.runRdp(server, w, h)
            if (!result.success && result.errorMsg.toLowerCase().indexOf("remote host identification has changed") >= 0) {
                var title = "The certificate for server '" + server.serverIp + "' has changed!"
                var msg = "Accept the new key in the trust store?"
                appContext.confirmDanger(title, msg, "Add", function() {
                    getAppState().projectViewState.removeFromRdpTrustStore(server)
                    getAppState().projectViewState.runRdp(server, w, h)
                })
            } else {
                return result
            }
        },
        function() { return getAppState().projectViewState.openSshSession(server) }))
    menu.options = options
    menu.show(parnt || parent, global)
}
