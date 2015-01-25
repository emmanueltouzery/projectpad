import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils
import "poiactions.js" as PoiActions

ScrollView {
	id: pv
	anchors.fill: parent
	signal loadView(string name, variant model)
	property variant model
	property variant appContext: null

	property variant actions: [
		["addpoi", "glyphicons-336-pushpin", "Add point of interest"],
		["addwww", "glyphicons-372-global", "Add website"],
		["adddb", "glyphicons-142-database-plus", "Add database"]]

	function getBreadCrumbs() {
		var projectModel = Utils.findById(projectListState.projects, model.projectId)
		return {pathLinks:
			[
				{screen: "ProjectView.qml", 
				model: projectModel,
				display: projectModel.name}
			],
			title: model.desc}
	}

	function editPoi(curPoi) {
		popup.setContents("Edit point of interest", editPoiComponent,
				function (poiEdit) {
					poiEdit.activate(curPoi)
				},
				function (poiEdit) {
					poiEdit.onServerOk()
					// force refresh
					poisrepeater.model = serverViewState.getPois(pv.model.id)
				})
	}

	function editSrvWww(curPoi) {
		popup.setContents("Edit website", editSrvWwwComponent,
				function (wwwEdit) {
					wwwEdit.activate(curPoi)
				},
				function (wwwEdit) {
					wwwEdit.onOk()
					// force refresh
					wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
				})
	}

	function editDb(curDb) {
		popup.setContents("Edit database", editDatabaseComponent,
				function (dbEdit) {
					dbEdit.activate(curDb)
				},
				function (dbEdit) {
					dbEdit.onOk()
					// force refresh
					dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
				})
	}

	function actionTriggered(name) {
		switch (name) {
			case "addpoi":
				popup.setContents("Add point of interest", editPoiComponent,
						function (poiEdit) {
							poiEdit.activate(poiEdit.getDefaultModel())
						},
						function (poiEdit) {
							poiEdit.onServerOk();
							poisrepeater.model = serverViewState.getPois(pv.model.id)
						})
				break;
			case "addwww":
				popup.setContents("Add website", editSrvWwwComponent,
						function (wwwEdit) {
							wwwEdit.activate(wwwEdit.getDefaultModel())
						},
						function (wwwEdit) {
							wwwEdit.onOk();
							wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
						})
				break;
			case "adddb":
				popup.setContents("Add database", editDatabaseComponent,
						function (dbEdit) {
							dbEdit.activate(dbEdit.getDefaultModel())
						},
						function (dbEdit) {
							dbEdit.onOk();
							dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
						})
				break;
		}
	}

	Rectangle {
		id: serverHeader
		color: "light grey"
		x: 0
		width: parent.width
		height: 70

		Text {
			text: model.desc
			font.pointSize: 16
			x: 10
			height: 70
			verticalAlignment: Text.AlignVCenter
		}

		Rectangle {
			x: parent.width-width
			width: Math.max(ipText.width, Math.max(usernameText.width, copyBtn.width))

			Text {
				id: ipText
				text: model.serverIp
			}

			Text {
				id: usernameText
				text: model.username
				y: 20
			}

			Button {
				id: copyBtn
				text: "Copy password"
				y: 40
				onClicked: {
					appContext.copyItem(model.password)
				}
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
				id: wwwsrepeater
				model: serverViewState.getServerWebsites(pv.model.id)

				ItemTile {
					property int modelId: modelData.id
					property bool selected: false
					color: "dark gray"
					border.width: selected ? 4 : 0
					border.color: "green"
					itemDesc: modelData.desc
					icon: "glyphicons-372-global"
					MouseArea {
						anchors.fill: parent
						onClicked: {
							selectMenu.options = [["glyphicons-151-edit", function() { editSrvWww(modelData)}],
								["glyphicons-145-folder-open", function() { openAssociatedFile(modelData.url)}],
								["glyphicons-512-copy", function() { appContext.copyItem(modelData.password) }],
								["glyphicons-193-circle-remove", function() {
									appContext.confirmDelete(function() {
										serverViewState.deleteServerWebsites([modelData.id])
										wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
									})
								}]]
							selectMenu.show(parent)
						}
					}
				}
			}

			Repeater {
				id: dbsrepeater
				model: serverViewState.getServerDatabases(pv.model.id)

				ItemTile {
					property int modelId: modelData.id
					property bool selected: false
					width: 180; height: 180
					color: "gray"
					border.width: selected ? 4 : 0
					border.color: "green"
					itemDesc: modelData.desc
					icon: "glyphicons-528-database"

					MouseArea {
						anchors.fill: parent
						onClicked: {
							selectMenu.options = [["glyphicons-151-edit", function() { editDb(modelData)}],
								["glyphicons-512-copy", function() { appContext.copyItem(modelData.password) }],
								["glyphicons-193-circle-remove", function() {
									appContext.confirmDelete(function() {
										var msg = serverViewState.canDeleteServerDatabase(modelData)
										if (msg !== null) {
											appContext.errorMessage(msg)
											return
										}
										serverViewState.deleteServerDatabases([modelData.id])
										dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
									})
								}]]
							selectMenu.show(parent)
						}
					}
				}
			}

			Repeater {
				id: poisrepeater
				model: serverViewState.getPois(pv.model.id)

				ItemTile {
					property int modelId: modelData.id
					property bool selected: false
					color: "light gray"
					border.width: selected ? 4 : 0
					border.color: "green"
					itemDesc: modelData.desc
					icon: PoiActions.actions[modelData.interestType].icon

					MouseArea {
						anchors.fill: parent
						onClicked: {
							var options = [["glyphicons-151-edit", function() { editPoi(modelData)}],
								["glyphicons-512-copy", function() { appContext.copyItem(modelData.path) }],
								["glyphicons-193-circle-remove", function() {
									appContext.confirmDelete(function() {
										serverViewState.deleteServerPois([modelData.id])
										poisrepeater.model = serverViewState.getPois(pv.model.id)
									})
								}]]
							if (modelData.interestType === "PoiCommandToRun"
									&& pv.model.serverIp.length > 0
									&& pv.model.accessType === "SrvAccessSsh"
									&& pv.model.username.length > 0
									&& pv.model.password.length > 0) {
								options.push(["glyphicons-138-cogwheels", function() {
									var info = serverViewState.executePoiAction(pv.model, modelData)
									if (info[0] === "error") {
										appContext.errorMessage(info[1])
									} else {
										appContext.successMessage(info[1])
									}
								}])
							}
							selectMenu.options = options
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
			id: editPoiComponent
			PoiEdit {
				id: poiEdit
			}
		}
		Component {
			id: editSrvWwwComponent
			ServerWebsiteEdit {
				id: wwwEdit
			}
		}
		Component {
			id: editDatabaseComponent
			ServerDatabaseEdit {
				id: dbEdit
			}
		}
	}
}

