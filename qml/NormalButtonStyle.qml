import QtQuick 2.0
import QtQuick.Controls.Styles 1.3

ButtonStyle {
    background: Rectangle {
        implicitWidth: 100
        implicitHeight: 25
        border.width: control.activeFocus ? 2 : 1
        border.color: "#888"
        radius: 4
        gradient: Gradient {
            GradientStop { position: 0 ; color: control.pressed ? "#ccc" : "#eee" }
            GradientStop { position: 1 ; color: control.pressed ? "#aaa" : "#ccc" }
        }
    }
}
