import QtQuick 2.0
import QtQuick.Controls 1.2

ScrollView {
	anchors.fill: parent
	property variant model /* model is ignored in this screen */

	property variant actions: [
		["addprj", "glyphicons-146-folder-plus", "Add project"]]

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

	signal loadView(string name, variant model, variant displayPath)
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
					color: "light blue"

					Text {
						text: modelData.name
					}
					MouseArea {
						anchors.fill: parent
						onClicked: {
							loadView("ProjectView.qml", modelData, [modelData.name])
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
