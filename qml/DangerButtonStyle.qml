import QtQuick 2.0
import QtQuick.Controls.Styles 1.3

ButtonStyle {
	background: Rectangle {
		implicitWidth: 100
		implicitHeight: 25
		border.width: control.activeFocus ? 2 : 1
		border.color: "#ac2925"
		radius: 4
		gradient: Gradient {
		    GradientStop { position: 0 ; color: control.pressed ? "#bd2724" : "#c9302c" }
		    GradientStop { position: 1 ; color: control.pressed ? "#c9302c" : "#d9534f" }
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

