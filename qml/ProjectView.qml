import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "selection.js" as Select
import "utils.js" as Utils

ScrollView {
	id: pv
	anchors.fill: parent
	signal loadView(string name, variant model)
	signal selectionChange(variant selection)
	property variant model

	property bool editMode

	property variant actions: [
		["addsrv", "glyphicons-470-server-new", "Add server"],
		["addpoi", "glyphicons-336-pushpin", "Add point of interest"]]

	onSelectionChange: {
		Select.updateSelectDisplay(itemsrepeater)
		Select.updateSelectDisplay(poisrepeater)
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
				var sId = Select.selectedItems[0]
				if (sId > 1000000) {
					editPoi(sId-1000000)
				} else {
					editServer(sId)
				}
				break;
			case "addsrv":
				popup.setContents("Add server", serverEditComponent,
						function (serverEdit) {
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
						},
						function (poiEdit) {
							poiEdit.onOk();
							// force refresh
							poisrepeater.model = projectViewState.getPois(pv.model.id)
						})
				break;
			case "delete":
				var projectPois = Utils.map(
						Utils.filter(Select.selectedItems,
							function(i) { return i >=1000000}),
						function(x) { return x-1000000 })
				projectViewState.deleteProjectPois(projectPois)
				// force refresh
				poisrepeater.model = projectViewState.getPois(pv.model.id)

				var serverPois = Utils.filter(Select.selectedItems,
							function(i) { return i < 1000000})
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

				Rectangle {
					property int modelId: modelData.id
					property bool selected: false
					width: 180; height: 180
					color: "light blue"
					border.width: selected ? 4 : 0
					border.color: "green"

					Text {
						text: modelData.desc
					}
					MouseArea {
						anchors.fill: parent
						onClicked: {
							Select.handleClick(pv.selectionChange, modelData.id, function() {
								loadView("ServerView.qml", modelData)
							})
						}
					}
				}
			}

			Repeater {
				id: poisrepeater
				model: projectViewState.getPois(pv.model.id)

				Rectangle {
					property int modelId: 1000000 + modelData.id
					property bool selected: false
					width: 180; height: 180
					color: "light gray"
					border.width: selected ? 4 : 0
					border.color: "green"

					Text {
						text: modelData.desc
					}
					MouseArea {
						anchors.fill: parent
						onClicked: {
							Select.handleClick(selectionChange, 1000000 + modelData.id, function() {
								//loadView("ServerView.qml", modelData])
							})
						}
					}
				}
			}
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

