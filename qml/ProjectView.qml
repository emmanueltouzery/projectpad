import QtQuick 2.0
import QtQuick.Window 2.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "utils.js" as Utils
import "poiactions.js" as PoiActions

ScrollView {
	id: pv
	anchors.fill: parent
	signal loadView(string name, variant model)
	property variant model
	property variant appContext: null

	property bool editMode

	property variant actions: [
		["addsrv", "glyphicons-470-server-new", "Add server"],
		["addpoi", "glyphicons-336-pushpin", "Add point of interest"]]

	function getBreadCrumbs() {
		return {pathLinks: [], title: model.name};
	}

	function editServer(curServer) {
		popup.setContents("Edit server", serverEditComponent,
			function (serverEdit) {
				serverEdit.activate(curServer)
			},
			function (serverEdit) {
				serverEdit.onOk()
				// force refresh
				itemsrepeater.model = projectViewState.getServers(pv.model.id)
			})
	}

	function editPoi(curPoi) {
		popup.setContents("Edit point of interest", poiEditComponent,
			function (poiEdit) {
				poiEdit.activate(curPoi)
			},
			function (poiEdit) {
				poiEdit.onOk()
				// force refresh
				poisrepeater.model = projectViewState.getPois(pv.model.id)
			})
	}

	function actionTriggered(name) {
		switch (name) {
			case "addsrv":
				popup.setContents("Add server", serverEditComponent,
						function (serverEdit) {
							serverEdit.activate(serverEdit.getDefaultModel())
						},
						function (serverEdit) {
							serverEdit.onOk()
							// force refresh
							itemsrepeater.model = projectViewState.getServers(pv.model.id)
						})
				break;
			case "addpoi":
				popup.setContents("Add point of interest", poiEditComponent,
						function (poiEdit) {
							poiEdit.activate(poiEdit.getDefaultModel())
						},
						function (poiEdit) {
							poiEdit.onOk();
							// force refresh
							poisrepeater.model = projectViewState.getPois(pv.model.id)
						})
				break;
		}
	}

	Rectangle {
		id: projectHeader
		color: "light grey"
		x: 0
		width: parent.width
		height: 70

		Text {
			text: model.name
			font.pointSize: 16
			x: 10
			height: 70
			verticalAlignment: Text.AlignVCenter
		}

		Button {
			anchors.right: deleteBtn.left
			y: 35
			id: copyBtn
			text: "Edit project"
			onClicked: {
				popup.setContents("Edit project", projectEditComponent,
					function (projectEdit) {
						projectEdit.activate(pv.model)
					},
					function (projectEdit) {
						projectEdit.onOk()
						loadView("ProjectList.qml", null)
					})
			}
		}
		Button {
			x: parent.width-width
			id: deleteBtn
			text: "Delete project"
			y: 35
			onClicked: {
				appContext.confirmDelete(function() {
					projectListState.deleteProjects([pv.model.id])
					loadView("ProjectList.qml", null)
				})
			}
		}
	}

	Flickable {
		width: parent.width
		anchors.topMargin: 70
		contentHeight: flow.implicitHeight
		Flow {
			anchors.fill: parent
			anchors.margins: 4
			spacing: 10
			id: flow

			Repeater {
				id: itemsrepeater
				model: projectViewState.getServers(pv.model.id)

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
										itemsrepeater.model = projectViewState.getServers(pv.model.id)
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
			}

			Repeater {
				id: poisrepeater
				model: projectViewState.getPois(pv.model.id)

				ItemTile {
					itemDesc: modelData.desc
					icon: PoiActions.actions[modelData.interestType].icon
					property int modelId: modelData.id
					property bool selected: false
					color: "light gray"
					border.width: selected ? 4 : 0
					border.color: "green"

					MouseArea {
						anchors.fill: parent
						onClicked: {
							selectMenu.options = [[PoiActions.actions[modelData.interestType].icon, function() {
								var info = projectViewState.runPoiAction(modelData)
								appContext.progressMessage("Started program\n")
								}],
								["glyphicons-151-edit", function() {editPoi(modelData)}],
								["glyphicons-193-circle-remove", function() {
									appContext.confirmDelete(function() {
										projectViewState.deleteProjectPois([modelData.id])
										// force refresh
										poisrepeater.model = projectViewState.getPois(pv.model.id)
									})
								}]]
							selectMenu.show(parent)
						}
					}
				}
			}
		}

		SelectMenu {
			id: selectMenu
			visible: false
			z: 3
		}

		Component {
			id: projectEditComponent
			ProjectEdit {
				id: projectEdit
			}
		}
		Component {
			id: serverEditComponent
			ServerEdit {
				id: serverEdit
			}
		}
		Component {
			id: poiEditComponent
			PoiEdit {
				id: poiEdit
			}
		}

		FileDialog {
			id: saveAuthKeyDialog
			title: "Please choose a destination"
			property variant server
			visible: false
			selectFolder: true
			onAccepted: {
				projectViewState.saveAuthKey(fileUrls[0]
					+ "/" + server.authKeyFilename, server)
				appContext.successMessage("Saved file to "
					+ fileUrls[0] + "/" + server.authKeyFilename)
			}
		}
	}
}

