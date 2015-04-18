import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "utils.js" as Utils

Rectangle {
    id: extraUserEdit
    color: "light grey"
    property int preferredHeight: 180

    property variant model: getDefaultModel()
    property string keyFilepath

    function getDefaultModel() {
        return {"desc": "New user",
            "username": "", "password": "",
            "authKeyFilename": "..."}
    }

    function activate(_model) {
        extraUserEdit.model = _model
        authFilename.text = keyFilepath = _model.authKeyFilename
        userAccountDescription.selectAll()
        userAccountDescription.forceActiveFocus()
    }

    function onOk() {
        if (model.id) {
            extraUserEdit.model = serverViewState.updateServerExtraUserAccount(
                model, userAccountDescription.text,
                username.text, password.text, extraUserEdit.keyFilepath);
        } else {
            serverViewState.addServerExtraUserAccount(userAccountDescription.text,
                username.text, password.text, extraUserEdit.keyFilepath)
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
            id: userAccountDescription
            Layout.fillWidth: true
            text: extraUserEdit.model.desc
        }

        Text {
            text: "Username:"
        }
        TextField {
            id: username
            Layout.fillWidth: true
            text: extraUserEdit.model.username
        }

        Text {
            text: "Password:"
        }
        PasswordField {
            id: password
            Layout.fillWidth: true
            text: extraUserEdit.model.password
        }

        Text {
            text: "Authentication key:"
        }
        Button {
            id: authFilename
            Layout.fillWidth: true
            text: "..."
            onClicked: fileDialog.visible = true
        }
    }

    FileDialog {
        id: fileDialog
        title: "Please choose a file"
        visible: false
        onAccepted: {
            extraUserEdit.keyFilepath = fileDialog.fileUrls[0]
            authFilename.text = extraUserEdit.keyFilepath.substring(
                extraUserEdit.keyFilepath.lastIndexOf("/")+1, extraUserEdit.keyFilepath.length)
        }
    }
}
