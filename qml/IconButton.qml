import QtQuick 2.0

Rectangle {
	width: text.width+23
	height: 32
	border.width: 1
	border.color: "black"

	property string text;
	property string iconName;
	signal clicked;

	Image {
		x: 3
		y: (parent.height - 16)/2
		width : 16
		height: 16
		smooth: true
		source: "../glyphicons-free/" + parent.iconName + ".png"
	}

	Text {
		x: 20
		id: text
		text: parent.text
		height: parent.height
		verticalAlignment: Text.AlignVCenter
	}

	MouseArea {
		id: mousearea
		anchors.fill: parent
	}
	Component.onCompleted: mousearea.clicked.connect(clicked)
}
