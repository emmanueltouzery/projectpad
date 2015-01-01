import QtQuick 2.0
import QtQuick.Controls 1.2
import "selection.js" as Select

ScrollView {
	id: projectList
	anchors.fill: parent
	property variant model /* model is ignored in this screen */

	property variant actions: [
		["addprj", "glyphicons-146-folder-plus", "Add project"]]

	property bool editMode

	function actionTriggered(name) {
		switch (name) {
			case "addprj":
				popup.setContents("Add project", projectEditComponent,
						function(projectEdit) {
							projectEdit.activate({name: "Project name"})
						},
						function(projectEdit) {
							projectEdit.onOk()
						})
				break;
		}
	}

	signal selectionChange(variant selection)
	signal loadView(string name, variant model, variant displayPath)

	onSelectionChange: Select.updateSelectDisplay(itemsrepeater)
	onEditModeChanged: Select.clearSelection(projectList.selectionChange)

	Flickable {
		width: parent.width
		contentHeight: flow.implicitHeight
		Loader {
			id: plLoader
		}
		Flow {
			anchors.fill: parent
			anchors.margins: 4
			spacing: 10
			id: flow

			Repeater {
				id: itemsrepeater
				model: projectListState.projects

				Rectangle {
					width: 180; height: 180
					property int modelId: modelData.id
					property bool selected: false
					color: "light blue"
					border.width: selected ? 4 : 0
					border.color: "green"

					Text {
						text: modelData.name
					}
					MouseArea {
						anchors.fill: parent
						onClicked: {
							Select.handleClick(projectList.selectionChange, modelData.id, function() {
								loadView("ProjectView.qml", modelData, [modelData.name])
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
	}
}
