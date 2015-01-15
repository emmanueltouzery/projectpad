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
		["addsrv", "glyphicons-470-server-new", "Add server"],
		["addpoi", "glyphicons-336-pushpin", "Add point of interest"]]

	onSelectionChange: {
		Select.updateSelectDisplay("server", itemsrepeater)
		Select.updateSelectDisplay("poi", poisrepeater)
	}
	onEditModeChanged: Select.clearSelection(pv.selectionChange)

	function getBreadCrumbs() {
		return {pathLinks: [], title: model.name};
	}

	function editServer(sId) {
		var curServer = Utils.findById(projectViewState.getServers(pv.model.id), sId)
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
			case "edit":
				var itemInfo = Select.getSelectedItem(["server", "poi"])
				if (itemInfo[0] === "poi") {
					editPoi(itemInfo[1])
				} else {
					editServer(itemInfo[1])
				}
				break;
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
			case "delete":
				var projectPois = Select.selectedItems["poi"]
				projectViewState.deleteProjectPois(projectPois)
				// force refresh
				poisrepeater.model = projectViewState.getPois(pv.model.id)

				var serverPois = Select.selectedItems["server"]
				projectViewState.deleteServers(serverPois)
				// force refresh
				itemsrepeater.model = projectViewState.getServers(pv.model.id)
				break;
		}
	}

	Flickable {
		width: parent.width
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
					property int modelId: modelData.id
					property bool selected: false
					color: "light blue"
					border.width: selected ? 4 : 0
					border.color: "green"
					itemDesc: modelData.desc
					icon: "glyphicons-464-server"

					MouseArea {
						anchors.fill: parent
						onClicked: {
							Select.handleClick(pv.selectionChange, "server", modelData.id, function() {
								loadView("ServerView.qml", modelData)
							})
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
								["Delete", function() {console.log("delete")}]]
							selectMenu.show(parent)
							Select.handleClick(selectionChange, "poi", modelData.id, function() {
								//loadView("ServerView.qml", modelData])
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

