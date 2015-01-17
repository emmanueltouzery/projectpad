import QtQuick 2.0
import QtQuick.Controls 1.3

Button {
	property string iconName
	property string btnText
	property int iconSize: 16
	property int iconX: 3

	width: text.width+image.width+10

	Image {
		x: iconX
		y: (parent.height - iconSize)/2
		source: '../glyphicons-free/' + parent.iconName + '.png'
		height: iconSize
		fillMode: Image.PreserveAspectFit
		smooth: true
		id: image
	}

	Text {
		x: image.width+5
		id: text
		text: parent.btnText
		height: parent.height
		verticalAlignment: Text.AlignVCenter
	}
}
