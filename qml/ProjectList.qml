import QtQuick 2.0
import QtQuick.Controls 1.2

ScrollView {
	anchors.fill: parent
	property variant model /* model is ignored in this screen */
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
						projectEdit.activate({name: "Project name"})
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
	}
}
