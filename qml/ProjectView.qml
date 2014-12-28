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
				projectEdit.activate(pv.model)
				projectEdit.x = 0
				projectEdit.width = pv.width
				projectEdit.height = pv.height
				break;
			case "addsrv":
				popup.setContents(addServerContents)
				popup.visible = true
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

			Rectangle {
				width: 180; height: 180
				color: "light grey"
				id: addIcon

				Text {
					text: "+"
				}
				MouseArea {
					anchors.fill: parent
					onClicked: {
						expandAnimation.start()
						projectEdit.activate(pv.model)
					}
				}
			}

			ParallelAnimation {
				id: expandAnimation
				loops: 1
				PropertyAnimation {
					target: projectEdit
					properties: "width"
					from: addIcon.width
					to: window.width
					duration: 200
				}
				PropertyAnimation {
					target: projectEdit
					properties: "height"
					from: addIcon.height
					to: window.height
					duration: 200
				}
				PropertyAnimation {
					target: projectEdit
					properties: "x"
					from: addIcon.x
					to: 0
					duration: 200
				}
				PropertyAnimation {
					target: projectEdit
					properties: "y"
					from: addIcon.y
					to: 0
					duration: 200
				}
			}
		}
		ProjectEdit {
			id: projectEdit
		}
		Component {
			id: addServerContents
			Text {
				text : "yupi"
			}
		}
	}
}

