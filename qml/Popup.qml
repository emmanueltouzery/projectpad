import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	anchors.fill: parent
	color: "#aa000000"
	z: 1

	function setContents(contents) {
		popupContentsLoader.sourceComponent = contents
	}

	Rectangle {
		id: popup
		anchors.horizontalCenter: parent.horizontalCenter
		y: 40
		width: 580
		color: Qt.lighter("light gray", 1.15)
		height: 500
		z: 2
		radius: 5
	
		Rectangle {
			id: popupHeader
			width: parent.width
			color: Qt.lighter("light gray", 1.1)
			height: 40
			radius: 5
	
			Button {
				text: "Cancel"
				x: 5
				style: NormalButtonStyle {}
				anchors.verticalCenter: parent.verticalCenter
			}
	
			Text {
				text: "Title"
				anchors.horizontalCenter: parent.horizontalCenter
				anchors.verticalCenter: parent.verticalCenter
				font.bold: true
			}
	
			Button {
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
			height: popup.height - popupHeader.height
		}
	}

	/* this mouse area catches the clicks outside of the popup,
	 * preventing the user from clicking in the greyed out areas. */
	MouseArea {
		anchors.fill: parent
	}
}