import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "utils.js" as Utils

Rectangle {
    id: extraUserEdit
    color: "light grey"
    height: childrenRect.height

    property variant model: getDefaultModel()
    property var origModel
    property string keyFilepath

    function getDefaultModel() {
        return {"desc": "New user",
            "username": "", "password": "",
            "authKeyFilename": "..."}
    }

    function activate(server, _model) {
        origModel = _model
        extraUserEdit.model = Utils.deepCopy(_model)
        authFilename.text = keyFilepath = _model.authKeyFilename
        userAccountDescription.selectAll()
        userAccountDescription.forceActiveFocus()

        var groups = getAppState().serverViewState.getServerGroupNames(server.id)
        group.model.clear()
        groups.forEach(function(grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)
    }

    function onOk(server) {
        if (model.id) {
            extraUserEdit.model = getAppState().serverViewState
                .updateServerExtraUserAccount(
                    origModel, userAccountDescription.text,
                    username.text, password.text,
                    extraUserEdit.keyFilepath, group.editText);
        } else {
            getAppState().serverViewState.addServerExtraUserAccount(server.id,
                userAccountDescription.text, username.text, password.text,
                extraUserEdit.keyFilepath, group.editText)
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
