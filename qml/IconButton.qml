import QtQuick 2.0
import QtQuick.Controls 1.3

Button {
	property string iconName
	property string btnText

	width: text.width+23

	Image {
		x: 3
		y: (parent.height - 16)/2
		source: '../glyphicons-free/' + parent.iconName + '.png'
		width: 16
		height: 16
		smooth: true
	}

	Text {
		x: 20
		id: text
		text: parent.btnText
		height: parent.height
		verticalAlignment: Text.AlignVCenter
	}
}
