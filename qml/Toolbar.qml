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

		IconButton {
			text: 'home'
			iconName: 'glyphicons-21-home'
			onClicked: loadView("projectlist.qml", null, [])
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
