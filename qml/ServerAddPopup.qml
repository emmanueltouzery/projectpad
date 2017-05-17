import QtQuick 2.7
import QtQuick.Controls 1.3

Rectangle {
    color: "light grey"
    height: childrenRect.height

    function init() {
        // this focus is nice, also enables <esc> to close the dialog
        addServerWebsite.forceActiveFocus()
    }

    function next() {
        if (addServerPoi.checked) {
            return "addpoi"
        } else if (addServerWebsite.checked) {
            return "addwww"
        } else if (addServerDatabase.checked) {
            return "adddb"
        } else if (addExtraUserAccount.checked) {
            return "addaccount"
        } else if (addServerNote.checked) {
            return "addsrvnote"
        }
    }

    ExclusiveGroup { id: addGroup }

    Column {
        spacing: 10
        // anchors.verticalCenter: parent.verticalCenter
        topPadding: 10
        leftPadding: 10

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
        RadioButton {
            id: addServerNote
            text: "Add note"
            exclusiveGroup: addGroup
        }
    }
}
