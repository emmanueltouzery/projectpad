import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

ScrollView {
	id: pv
	anchors.fill: parent
	signal loadView(string name, int displayId, variant displayPath)
	property int displayId /* project ID */

	property variant actions: [["edit", "glyphicons-31-pencil", "Edit project"]]

	function actionTriggered(name) {
		console.log("action triggered: " + name)
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
				model: projectViewState.getServers(pv.displayId)

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
						addRect.activate()
					}
				}
			}

			ParallelAnimation {
				id: expandAnimation
				loops: 1
				PropertyAnimation {
					target: addRect
					properties: "width"
					from: addIcon.width
					to: window.width
					duration: 200
				}
				PropertyAnimation {
					target: addRect
					properties: "height"
					from: addIcon.height
					to: window.height
					duration: 200
				}
				PropertyAnimation {
					target: addRect
					properties: "x"
					from: addIcon.x
					to: 0
					duration: 200
				}
				PropertyAnimation {
					target: addRect
					properties: "y"
					from: addIcon.y
					to: 0
					duration: 200
				}
			}
		}
		ProjectEdit {
			id: addRect
		}
	}
}

