import QtQuick 2.0
import QtQuick.Controls 1.2

Rectangle {
	id: editModeActionBar
	color: "light gray"
	width: parent.width
	height: 32

	Flow {
		anchors.right: parent.right

		Button {
			id: editBtn
			text: 'Edit'
			style: NormalButtonStyle {}
			height: editModeActionBar.height
		}
		Button {
			id: deleteBtn
			text: 'Delete'
			style: NormalButtonStyle {}
			height: editModeActionBar.height
		}
	}
}
