import QtQuick 2.0
import QtQuick.Controls 1.2

Rectangle {
	id: editModeActionBar
	color: "light gray"
	width: parent.width
	height: 32

	property int selectionCount: 0
	signal modeActionBarAction(string type)
	signal actionExecuted()

	Flow {
		anchors.right: parent.right

		Button {
			id: editBtn
			text: 'Edit'
			style: NormalButtonStyle {}
			height: editModeActionBar.height
			enabled: selectionCount === 1
			onClicked: {
				modeActionBarAction("edit")
				actionExecuted()
			}
		}
		Button {
			id: deleteBtn
			text: 'Delete'
			style: NormalButtonStyle {}
			height: editModeActionBar.height
			enabled: selectionCount > 0
		}
	}
}
