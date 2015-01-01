import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	anchors.fill: parent
	color: "#aa000000"
	z: 1
	property var curCallback

	function setContents(title, contents, initCallback, okCallback) {
		popupTitle.text = title
		popupContentsLoader.sourceComponent = contents
		initCallback(popupContentsLoader.item)
		var f = function() {
			okCallback(popupContentsLoader.item)
			okButton.clicked.disconnect(curCallback)
			popup.visible = false
		}
		okButton.clicked.connect(f)
		curCallback = f
		popup.visible = true
	}

	function setContentsDelete(title, contents, initCallback, okCallback) {
		setContents(title, contents, initCallback, okCallback);
		okButton.text = "Delete"
		okButton.style = dangerButtonStyle
	}

	Rectangle {
		id: popupWindow
		anchors.horizontalCenter: parent.horizontalCenter
		y: 40
		width: 580
		color: Qt.lighter("light gray", 1.15)
		height: popupHeader.height + popupContentsLoader.height
		z: 2
		radius: 5
	
		Rectangle {
			id: popupHeader
			width: parent.width
			color: Qt.lighter("light gray", 1.1)
			height: 40
			radius: 5
	
			Button {
				id: cancelButton
				text: "Cancel"
				x: 5
				style: NormalButtonStyle {}
				anchors.verticalCenter: parent.verticalCenter
				onClicked: {
					popup.visible = false
					okButton.clicked.disconnect(curCallback)
				}
			}
	
			Text {
				id: popupTitle
				text: "Title"
				anchors.horizontalCenter: parent.horizontalCenter
				anchors.verticalCenter: parent.verticalCenter
				font.bold: true
			}
	
			Button {
				id: okButton
				text: "OK"
				anchors.right: parent.right
				anchors.rightMargin: 5
				style: DefaultButtonStyle {}
				anchors.verticalCenter: parent.verticalCenter
			}
		}

		Loader {
			id: popupContentsLoader
			width: parent.width
			y: popupHeader.height
			onLoaded: height = item.preferredHeight
		}
	}

	/* this mouse area catches the clicks outside of the popup,
	 * preventing the user from clicking in the greyed out areas. */
	MouseArea {
		anchors.fill: parent
	}

	Component {
		id: dangerButtonStyle
		DangerButtonStyle {}
	}
}
