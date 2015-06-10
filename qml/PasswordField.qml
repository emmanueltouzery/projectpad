import QtQuick 2.2
import QtQuick.Controls 1.2

TextField {
    echoMode: TextInput.Password

    function resetToMasked() {
        echoMode = TextInput.Password
        btn.source = '../glyphicons-free/glyphicons-204-lock.png'
    }

    Image {
        id: btn
        anchors { top: parent.top; right: parent.right; margins: 7 }
        source: '../glyphicons-free/glyphicons-204-lock.png'
        fillMode: Image.PreserveAspectFit
        height: parent.height - 7 * 2
        width: parent.height - 7 * 2

        MouseArea {
            anchors.fill: parent
            onClicked: {
                if (parent.parent.echoMode === TextInput.Normal) {
                    resetToMasked()
                } else {
                    parent.parent.echoMode = TextInput.Normal
                    parent.source = '../glyphicons-free/glyphicons-205-unlock.png'
                }
            }
        }
    }
}
