function editServer(curServer) {
	popup.setContents("Edit server", serverEditComponent,
		function (serverEdit) {
			serverEdit.activate(curServer, curServer.environment)
		},
		function (serverEdit) {
			serverEdit.onOk()
			// force refresh
			itemsrepeater.model = projectViewState.getServers(pv.model.project.id, pv.model.environment)
		})
}

function showSelectMenu(server, parnt) {
	var options = [
		["glyphicons-145-folder-open", function() { loadView("ServerView.qml", server) }],
		["glyphicons-151-edit", function() {editServer(server)}],
		["glyphicons-512-copy", function() { appContext.copyItem(server.password, true) }],
		["glyphicons-193-circle-remove", function() {
			appContext.confirmDelete(function() {
				projectViewState.deleteServers([server.id])
				// force refresh
				itemsrepeater.model = projectViewState.getServers(pv.model.project.id, pv.model.environment)
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
			var desktopWidth = Screen.desktopAvailableWidth
			if (Screen.desktopAvailableWidth / Screen.desktopAvailableHeight > 3) {
				// I can assume a double monitor setup: divide the
				// width by two to compensate.
				desktopWidth = desktopWidth / 2
			}
			var info = projectViewState.runRdp(server,
				Math.round(desktopWidth * 0.75),
				Math.round(Screen.desktopAvailableHeight * 0.75))
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
	selectMenu.options = options
	selectMenu.show(parnt || parent)
}
