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
		["edit", "glyphicons-31-pencil", "Edit server"]]

	function actionTriggered(name) {
		switch (name) {
			case "edit":
				popup.setContents("Edit server", serverEditComponent,
						function (serverEdit) {
							serverEdit.activate(pv.model)
						},
						function (serverEdit) {
							serverEdit.onOk()
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
				model: serverViewState.getPois(pv.model.id)

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
			id: serverEditComponent
			ServerEdit {
				id: serverEdit
			}
		}
	}
}

