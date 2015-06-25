import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
    id: srvDatabaseEdit
    color: "light grey"
    property int preferredHeight: 230

    property variant model: getDefaultModel()
    property var origModel

    function getDefaultModel() {
        return {"desc": "New database", "name": "",
            "text": "", "username": "", "password": ""}
    }

    function activate(server, _model) {
        origModel = _model
        srvDatabaseEdit.model = Utils.deepCopy(_model)

        var groups = getAppState().serverViewState
            .getServerGroupNames(server.id)
        group.model.clear()
        groups.forEach(function(grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)

        description.selectAll()
        description.forceActiveFocus()
    }

    function onOk(server) {
        if (model.id) {
            srvDatabaseEdit.model = getAppState()
                .serverViewState.updateServerDatabase(
                origModel, description.text, name.text, txt.text,
                username.text, password.text, group.editText)
        } else {
            getAppState().serverViewState.addServerDatabase(server.id,
                description.text, name.text,
                txt.text, username.text, password.text, group.editText)
        }
    }

    GridLayout {
        y: 10
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 10
        columns: 2

        Text {
            text: "Description:"
        }
        TextField {
            id: description
            Layout.fillWidth: true
            text: srvDatabaseEdit.model.desc
        }

        Text {
            text: "name:"
        }
        TextField {
            id: name
            Layout.fillWidth: true
            text: srvDatabaseEdit.model.name
        }

        Text {
            text: "Group:"
        }
        ComboBox {
            id: group
            Layout.fillWidth: true
            textRole: "text"
            model: ListModel {}
            editable: true
        }

        Text {
            text: "Text:"
        }
        TextField {
            id: txt
            Layout.fillWidth: true
            text: srvDatabaseEdit.model.text
        }

        Text {
            text: "Username:"
        }
        TextField {
            id: username
            Layout.fillWidth: true
            text: srvDatabaseEdit.model.username
        }

        Text {
            text: "Password:"
        }
        PasswordField {
            id: password
            Layout.fillWidth: true
            text: srvDatabaseEdit.model.password
        }
    }
}
