import QtQuick 2.0

ItemTile {
	property int modelId: modelData.server.id
	property bool selected: false
	color: "light blue"
	border.width: selected ? 4 : 0
	border.color: "green"
	itemDesc: modelData.server.desc
	icon: {
		if (modelData.server.accessType === "SrvAccessRdp") {
			return "../pics/windows_logo"
		} else {
			return "glyphicons-464-server"
		}
	}

	Flow {
		x: 5
		y: 145
		spacing: 2
		opacity: 0.6
		Image {
			height: 16
			fillMode: Image.PreserveAspectFit
			smooth: true
			source: '../glyphicons-free/glyphicons-526-user-key.png'
			visible: modelData.userCount > 0
		}
		Text {
			text: modelData.userCount
			visible: modelData.userCount > 0
		}
		Image {
			height: 16
			fillMode: Image.PreserveAspectFit
			smooth: true
			source: '../glyphicons-free/glyphicons-372-global.png'
			visible: modelData.wwwCount > 0
		}
		Text {
			text: modelData.wwwCount
			visible: modelData.wwwCount > 0
		}
		Image {
			height: 16
			fillMode: Image.PreserveAspectFit
			smooth: true
			source: '../glyphicons-free/glyphicons-528-database.png'
			visible: modelData.dbCount > 0
		}
		Text {
			text: modelData.dbCount
			visible: modelData.dbCount > 0
		}
		Image {
			height: 16
			fillMode: Image.PreserveAspectFit
			smooth: true
			source: '../glyphicons-free/glyphicons-149-folder-flag.png'
			visible: modelData.poiCount > 0
		}
		Text {
			text: modelData.poiCount
			visible: modelData.poiCount > 0
		}
	}

	MouseArea {
		anchors.fill: parent
		onClicked: {
			var options = [
				["glyphicons-145-folder-open", function() { loadView("ServerView.qml", modelData.server) }],
				["glyphicons-151-edit", function() {editServer(modelData.server)}],
				["glyphicons-512-copy", function() { appContext.copyItem(modelData.server.password, true) }],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						projectViewState.deleteServers([modelData.server.id])
						// force refresh
						itemsrepeater.model = projectViewState.getServers(pv.model.project.id, pv.model.environment)
					})
				}]]
			if (modelData.server.accessType === "SrvAccessWww") {
				options.push(["glyphicons-372-global", function() {
					openAssociatedFile(modelData.server.serverIp)
				}])
			}
			if (modelData.server.authKeyFilename !== "...") {
				options.push(["glyphicons-45-keys", function() {
					saveAuthKeyDialog.server = modelData.server
					saveAuthKeyDialog.visible = true
				}])
			}
			if (modelData.server.accessType === "SrvAccessRdp"
					&& modelData.server.username.length > 0
					&& modelData.server.password.length > 0) {
				options.push(["glyphicons-489-multiple-displays", function() {
					var desktopWidth = Screen.desktopAvailableWidth
					if (Screen.desktopAvailableWidth / Screen.desktopAvailableHeight > 3) {
						// I can assume a double monitor setup: divide the
						// width by two to compensate.
						desktopWidth = desktopWidth / 2
					}
					var info = projectViewState.runRdp(modelData.server,
						Math.round(desktopWidth * 0.75),
						Math.round(Screen.desktopAvailableHeight * 0.75))
					if (info[0] === "error") {
						appContext.errorMessage(info[1])
					}
				}])
			}
			if (modelData.server.accessType === "SrvAccessSsh"
					&& modelData.server.username.length > 0
					&& modelData.server.password.length > 0) {
				options.push(["glyphicons-489-multiple-displays", function() {
					var info = projectViewState.openSshSession(modelData.server)
					if (info[0] === "error") {
						appContext.errorMessage(info[1])
					}
				}])
			}
			selectMenu.options = options
			selectMenu.show(parent)
		}
	}
}
