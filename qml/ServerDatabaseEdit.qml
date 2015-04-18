import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
    id: srvDatabaseEdit
    color: "light grey"
    property int preferredHeight: 230

    property variant model: getDefaultModel()

    function getDefaultModel() {
        return {"desc": "New database", "name": "",
            "text": "", "username": "", "password": ""}
    }

    function activate(_model) {
        srvDatabaseEdit.model = _model
        description.selectAll()
        description.forceActiveFocus()
    }

    function onOk() {
        if (model.id) {
            srvDatabaseEdit.model = serverViewState.updateServerDatabase(
                model, description.text, name.text, txt.text,
                username.text, password.text)
        } else {
            serverViewState.addServerDatabase(description.text, name.text,
                txt.text, username.text, password.text)
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
