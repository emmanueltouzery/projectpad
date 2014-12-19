import QtQuick 2.0
import QtQuick.Window 2.0

Window {
	width: 800; height: 600;
	title: 'ProjectPAD';
	visible: true;
	id: window

	Flow {
		anchors.fill: parent
		anchors.margins: 4
		spacing: 10
		id: flow

		Repeater {
			id: itemsrepeater
			model: projects

			Rectangle {
				width: 180; height: 180
				color: "light blue"

				Text {
					text: modelData.name
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
				onClicked: expandAnimation.start()
			}
		}

		ParallelAnimation {
			id: expandAnimation
			loops: 1
			PropertyAnimation {
				target: addRect
				properties: "width"
				from: addIcon.width
				to: flow.width
				duration: 200
			}
			PropertyAnimation {
				target: addRect
				properties: "height"
				from: addIcon.height
				to: flow.height
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

	Rectangle {
		width: 180; height: 180
		x: -180
		id: addRect
		color: "light grey"

		Text {
			text: "+"
		}
	}

}
