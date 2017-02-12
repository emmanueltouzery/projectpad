import QtQuick 2.0
import QtQuick.Controls 1.3

Rectangle {
    color: "light grey"
    height: childrenRect.height

    function init() {
    }

    function next() {
        if (addServerPoi.checked) {
            return 1
        } else if (addServerWebsite.checked) {
            return 2
        } else if (addServerDatabase.checked) {
            return 3
        } else if (addExtraUserAccount.checked) {
            return 4
        }
    }

    ExclusiveGroup { id: addGroup }

    Column {
        spacing: 10
        // anchors.verticalCenter: parent.verticalCenter
        x: 15

        RadioButton {
            id: addServerPoi
            text: "Add point of interest"
            exclusiveGroup: addGroup
            checked: true
        }
        RadioButton {
            id: addServerWebsite
            text: "Add website"
            exclusiveGroup: addGroup
        }
        RadioButton {
            id: addServerDatabase
            text: "Add database"
            exclusiveGroup: addGroup
        }
        RadioButton {
            id: addExtraUserAccount
            text: "Add extra user account"
            exclusiveGroup: addGroup
        }
    }
}
