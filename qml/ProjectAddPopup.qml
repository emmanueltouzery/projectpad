import QtQuick 2.0
import QtQuick.Controls 1.3

Rectangle {
    color: "light grey"
    property int preferredHeight: 170

    function init() {
    }

    function next() {
        if (addServer.checked) {
            return 1
        } else if (addPointOfInterest.checked) {
            return 2
        } else if (addNote.checked) {
            return 3
        } else if (addServerLink.checked) {
            return 4
        }
    }

    ExclusiveGroup { id: addGroup }

    Column {
        spacing: 10
        anchors.verticalCenter: parent.verticalCenter
        x: 10

        RadioButton {
            id: addServer
            text: "Add server"
            exclusiveGroup: addGroup
            checked: true
        }
        RadioButton {
            id: addServerLink
            text: "Add link to server"
            exclusiveGroup: addGroup
        }
        RadioButton {
            id: addPointOfInterest
            text: "Add point of interest"
            exclusiveGroup: addGroup
        }
        RadioButton {
            id: addNote
            text: "Add note"
            exclusiveGroup: addGroup
        }
    }
}
