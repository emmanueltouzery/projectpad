import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "selection.js" as Select
import "utils.js" as Utils
import "poiactions.js" as PoiActions

ScrollView {
	id: pv
	anchors.fill: parent
	signal loadView(string name, variant model)
	signal selectionChange(int selectionCount)
	property variant model
	property variant appContext: null

	property bool editMode

	property variant actions: [
		["addpoi", "glyphicons-336-pushpin", "Add point of interest"],
		["addwww", "glyphicons-372-global", "Add website"],
		["adddb", "glyphicons-142-database-plus", "Add database"]]

	onSelectionChange: {
		Select.updateSelectDisplay("www", wwwsrepeater)
		Select.updateSelectDisplay("db", dbsrepeater)
		Select.updateSelectDisplay("poi", poisrepeater)
	}
	onEditModeChanged: Select.clearSelection(pv.selectionChange)

	function getBreadCrumbs() {
		var projectModel = Utils.findById(projectListState.projects, parseInt(model.projectId))
		return {pathLinks:
			[
				{screen: "ProjectView.qml", 
				model: projectModel,
				display: projectModel.name}
			],
			title: model.desc}
	}

	function editPoi(sId) {
		var curPoi = Utils.findById(serverViewState.getPois(pv.model.id), sId)
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

	function editSrvWww(sId) {
		var curPoi = Utils.findById(serverViewState.getServerWebsites(pv.model.id), sId)
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

	function editDb(sId) {
		var curDb = Utils.findById(serverViewState.getServerDatabases(pv.model.id), sId)
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
			case "edit":
				var sId = Select.getSelectedItem(["www", "db", "poi"])
				if (sId[0] === "poi") {
					editPoi(sId[1])
				} else if (sId[0] == "db") {
					editDb(sId[1])
				} else {
					editSrvWww(sId[1])
				}
				break;
			case "delete":
				var serverDbs = Select.selectedItems["db"]
				for (var i=0;i<serverDbs.length;i++) {
					var serverDbId = serverDbs[i]
					var serverDb = Utils.findById(dbsrepeater.model, serverDbId)
					var msg = serverViewState.canDeleteServerDatabase(serverDb)
					if (msg !== null) {
						appContext.errorMessage(msg)
						return
					}
				}
				serverViewState.deleteServerDatabases(serverDbs)
				dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
				var serverPois = Select.selectedItems["poi"]
				serverViewState.deleteServerPois(serverPois)
				poisrepeater.model = serverViewState.getPois(pv.model.id)

				var serverWwws = Select.selectedItems["www"]
				serverViewState.deleteServerWebsites(serverWwws)
				wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
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
							selectMenu.options = [["Edit", function() { editSrvWww(modelData.id)}],
								["Open", function() { openAssociatedFile(modelData.url)}],
								["Delete", function() {}]]
							selectMenu.show(parent)
							Select.handleClick(pv.selectionChange, "www", modelData.id, function() {
							})
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
							selectMenu.options = [["Edit", function() { editDb(modelData.id)}],
								["Copy pass", function() { appContext.copyItem(modelData.password) }],
								["Delete", function() {}]]
							selectMenu.show(parent)
							Select.handleClick(pv.selectionChange, "db", modelData.id, function() {
							})
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
							selectMenu.options = [["Edit", function() { editPoi(modelData.id)}],
								["Copy path", function() { appContext.copyItem(modelData.path) }],
								["Delete", function() {}]]
							selectMenu.show(parent)
							Select.handleClick(pv.selectionChange, "poi", modelData.id, function() {
							})
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

