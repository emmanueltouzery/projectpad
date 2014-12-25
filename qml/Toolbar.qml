import QtQuick 2.0

Rectangle {
	id: toolbarRoot
	color: 'light gray'
	width: parent.width
	height: 32

	signal loadView(string name, int displayId, variant displayPath)
	property variant displayPath: []

	Flow {
		x: 5
		width: parent.width-x
		height: parent.height
		spacing: 5

		Rectangle {
			height: parent.height
			border.width: 1
			border.color: "black"
			width: homeText.x+homeText.width+5
			Image {
				x: 3
				y: (toolbarRoot.height - 16)/2
				width : 16
				height: 16
				smooth: true
				source: "../glyphicons-free/glyphicons-21-home.png"
			}
			Text {
				id: homeText
				x: 20
				text: 'home'
				height: parent.height
				verticalAlignment: Text.AlignVCenter
			}

			MouseArea {
				anchors.fill: parent
				onClicked: loadView("projectlist.qml", null, [])
			}
		}
		Repeater {
			model: displayPath

			Text {
				text: modelData
				height: toolbarRoot.height
				verticalAlignment: Text.AlignVCenter
			}
		}
	}
}
