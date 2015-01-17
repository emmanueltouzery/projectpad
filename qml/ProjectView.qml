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
	signal selectionChange(int selectionCount)
	property variant model
	property variant appContext: null

	property bool editMode

	property variant actions: [
		["addsrv", "glyphicons-470-server-new", "Add server"],
		["addpoi", "glyphicons-336-pushpin", "Add point of interest"]]

	function getBreadCrumbs() {
		return {pathLinks: [], title: model.name};
	}

	function editServer(sId) {
		var curServer = Utils.findById(projectViewState.getServers(pv.model.id), sId)
		popup.setContents("Edit server", serverEditComponent,
				function (serverEdit) {
					serverEdit.activate(curServer.server)
				},
				function (serverEdit) {
					serverEdit.onOk()
					// force refresh
					itemsrepeater.model = projectViewState.getServers(pv.model.id)
				})
	}

	function editPoi(sId) {
		var curPoi = Utils.findById(projectViewState.getPois(pv.model.id), sId)
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
			x: parent.width-width
			y: 5
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
					icon: "glyphicons-464-server"

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
								["Contents", function() { loadView("ServerView.qml", modelData.server) }],
								["Edit", function() {editServer(modelData.id)}],
								["Delete", function() {
									appContext.confirmDelete(function() {
										projectViewState.deleteServers([modelData.server.id])
										// force refresh
										itemsrepeater.model = projectViewState.getServers(pv.model.id)
									})
								}]]
							if (modelData.server.accessType == "SrvAccessWww") {
								options.push(["Open", function() { openAssociatedFile(modelData.server.serverIp)}])
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
							selectMenu.options = [[PoiActions.actions[modelData.interestType].text, function() {
								var info = projectViewState.runPoiAction(modelData)
								if (info[0] === "error") {
									appContext.errorMessage(info[1])
								} else if (info[1].length > 0){
									appContext.successMessage(info[1])
								}}],
								["Edit", function() {editPoi(modelData.id)}],
								["Delete", function() {
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
	}
}

