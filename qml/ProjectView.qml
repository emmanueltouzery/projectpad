import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

ScrollView {
	id: pv
	anchors.fill: parent
	signal loadView(string name, variant model, variant displayPath)
	property variant model

	property variant actions: [
		["addsrv", "glyphicons-470-server-new", "Add server"],
		["edit", "glyphicons-31-pencil", "Edit project"]]

	function actionTriggered(name) {
		switch (name) {
			case "edit":
				popup.setContents("Edit project", projectEditComponent,
						function (projectEdit) {
							projectEdit.activate(pv.model)
						},
						function (projectEdit) {
							projectEdit.onOk()
						})
				break;
			case "addsrv":
				popup.setContents("Add server", addServerContents, function() {
					console.log("OK")
				})
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
					width: 180; height: 180
					color: "light blue"

					Text {
						text: modelData.desc
					}
					MouseArea {
						anchors.fill: parent
						onClicked: {
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
			id: addServerContents
			Text {
				text : "yupi"
			}
		}
	}
}

