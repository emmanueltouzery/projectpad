import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

ScrollView {
	anchors.fill: parent
	property int displayId /* displayId is ignored in this screen */
	signal loadView(string name, int displayId, variant displayPath)
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
							loadView("projectview.qml", modelData.id, [modelData.name])
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
						projectNameEntry.selectAll()
						projectNameEntry.forceActiveFocus()
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
	}
	Rectangle {
		width: 180; height: 180
		x: -400
		id: addRect
		color: "light grey"
	
		function closePopup() {
			addRect.width = 180
			addRect.x = -400
		}
	
		GridLayout {
			anchors.fill: parent
			columns: 2
	
			Text {
				text: "Project name:"
			}
	
			Rectangle {
				color: "white"
				width: 200
				height: 40
				TextInput {
					font.pointSize: 22
					id: projectNameEntry
					text: "Project name"
					anchors.fill: parent
				}
			}
	
			Button {
				text: "OK"
				onClicked: {
					projectListState.addProject(projectNameEntry.text)
					addRect.closePopup()
					/* TODO now directly open the new project */
				}
			}
	
			Button {
				text: "Cancel"
				onClicked: addRect.closePopup()
			}
		}
	}
}
