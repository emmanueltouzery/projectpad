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
		["addpoi", "glyphicons-336-pushpin", "Add point of interest"]]

	onSelectionChange: Select.updateSelectDisplay(poisrepeater)
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
				editPoi(Select.selectedItems[0])
				break;
			case "delete":
				serverViewState.deleteServerPois(Select.selectedItems)
				poisrepeater.model = serverViewState.getPois(pv.model.id)
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
					passwordCopy.selectAll()
					passwordCopy.copy()
				}
			}
			TextField {
				id: passwordCopy
				text: model.password
				visible: false
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
				id: poisrepeater
				model: serverViewState.getPois(pv.model.id)

				Rectangle {
					property int modelId: modelData.id
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
							Select.handleClick(pv.selectionChange, modelData.id, function() {
							})
						}
					}
				}
			}
		}
		Component {
			id: editPoiComponent
			PoiEdit {
				id: poiEdit
			}
		}
	}
}

