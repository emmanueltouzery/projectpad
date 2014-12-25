import QtQuick 2.0
import QtQuick.Controls 1.2

ScrollView {
	anchors.fill: parent
	property int displayId /* displayId is ignored in this screen */
	signal loadView(string name, int displayId, variant displayPath, variant actions)
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
							var myActions = [["edit", "glyphicons-31-pencil", "Edit project"]]
							loadView("projectview.qml", modelData.id, [modelData.name], myActions)
							/*plLoader.source = "projectview.qml"*/
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
