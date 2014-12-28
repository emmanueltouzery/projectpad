import QtQuick 2.0
import QtQuick.Controls.Styles 1.3

ButtonStyle {
	background: Rectangle {
		implicitWidth: 100
		implicitHeight: 25
		border.width: control.activeFocus ? 2 : 1
		border.color: "#1c5187"
		radius: 4
		gradient: Gradient {
		    GradientStop { position: 0 ; color: control.pressed ? "#3985d6" : "#5999dc" }
		    GradientStop { position: 1 ; color: control.pressed ? "#2c7acc" : "#3784d5" }
		}
	}
	label: Text {
		color: "white"
		text: control.text
		horizontalAlignment: Text.AlignHCenter
		width: control.width
		style: Text.Raised; styleColor: "black"
		font.bold: true
	}
}

