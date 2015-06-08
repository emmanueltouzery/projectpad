import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "utils.js" as Utils

Rectangle {
    id: serverEdit
    color: "light grey"
    property int preferredHeight: 330

    property variant model: getDefaultModel()
    property string keyFilepath
    property string environment

    function getDefaultModel() {
        return {"desc": "New server", "serverIp": "",
            "text": "",
            "username": "", "password": "",
            "authKeyFilename": "...",
            "type": "", "accessType": ""}
    }

    function activate(parent, _model, _environment) {
        serverEdit.model = _model
        serverEdit.environment = _environment
        serverType.currentIndex = Math.max(
            0, Utils.listModelGetValueIndex(serverType.model, _model.type))
        serverAccessType.currentIndex = Math.max(
            0, Utils.listModelGetValueIndex(serverAccessType.model, _model.accessType))
        authFilename.text = keyFilepath = _model.authKeyFilename
        var groups = projectViewState.getProjectGroupNames(parent.id)
        group.model.clear()
        groups.forEach(function (grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)
        serverDescription.selectAll()
        serverDescription.forceActiveFocus()
    }

    function onOk(project) {
        if (model.id) {
            serverEdit.model = projectViewState.updateServer(
                model, serverDescription.text, ipAddress.text, txt.text,
                username.text, password.text, serverEdit.keyFilepath,
                serverTypeItems.get(serverType.currentIndex).value,
                serverAccessTypeItems.get(serverAccessType.currentIndex).value,
                group.editText);
        } else {
            projectViewState.addServer(
                project.id, serverDescription.text, ipAddress.text,
                txt.text, username.text, password.text, serverEdit.keyFilepath,
                serverTypeItems.get(serverType.currentIndex).value,
                serverAccessTypeItems.get(serverAccessType.currentIndex).value,
                serverEdit.environment, group.editText)
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
            id: serverDescription
            Layout.fillWidth: true
            text: serverEdit.model.desc
        }

        Text {
            text: "IP Address:"
        }
        TextField {
            id: ipAddress
            Layout.fillWidth: true
            text: serverEdit.model.serverIp
        }

        Text {
            text: "Text"
        }
        TextField {
            id: txt
            Layout.fillWidth: true
            text: serverEdit.model.text
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
            text: serverEdit.model.username
        }

        Text {
            text: "Password:"
        }
        PasswordField {
            id: password
            Layout.fillWidth: true
            text: serverEdit.model.password
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

        Text {
            text: "Server type:"
        }
        ComboBox {
            id: serverType
            currentIndex: 0
            Layout.fillWidth: true
            model: ListModel {
                id: serverTypeItems
                ListElement { text: "Application"; value: "SrvApplication"}
                ListElement { text: "Database"; value: "SrvDatabase"}
            }
        }

        Text {
            text: "Server access type:"
        }
        ComboBox {
            id: serverAccessType
            currentIndex: 0
            Layout.fillWidth: true
            model: ListModel {
                id: serverAccessTypeItems
                ListElement { text: "SSH"; value: "SrvAccessSsh"}
                ListElement { text: "Remote desktop (RDP)"; value: "SrvAccessRdp"}
                ListElement { text: "Website"; value: "SrvAccessWww"}
            }
        }
    }

    FileDialog {
        id: fileDialog
        title: "Please choose a file"
        visible: false
        onAccepted: {
            serverEdit.keyFilepath = fileDialog.fileUrls[0]
            authFilename.text = serverEdit.keyFilepath.substring(
                serverEdit.keyFilepath.lastIndexOf("/")+1, serverEdit.keyFilepath.length)
        }
    }
}
