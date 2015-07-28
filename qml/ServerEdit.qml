import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Controls.Styles 1.3
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "utils.js" as Utils

Rectangle {
    id: serverEdit
    color: "light grey"
    property int preferredHeight: isSshTunnelAccess() ? 455 : 330

    property variant model: getDefaultModel()
    property var origModel
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
        password.resetToMasked()
        origModel = _model
        serverEdit.model = Utils.deepCopy(_model)
        serverEdit.environment = _environment
        serverType.currentIndex = Math.max(
            0, Utils.listModelGetValueIndex(serverType.model, _model.type))
        serverAccessType.currentIndex = Math.max(
            0, Utils.listModelGetValueIndex(serverAccessType.model, _model.accessType))
        authFilename.text = keyFilepath = _model.authKeyFilename
        var groups = getAppState().projectViewState.getProjectGroupNames(parent.id)
        group.model.clear()
        groups.forEach(function (grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)
        var sshServers = getAppState().projectListState.getAllSshServers()
        sshTunnelThrough.model.clear()
        sshServers.forEach(function (sshServer) {
            sshTunnelThrough.model.append({"value": sshServer.id, "text": sshServer.desc})
        })
        sshTunnelThrough.currentIndex = Utils.listModelGetValueIndex(
            sshTunnelThrough.model, _model.sshTunnelThroughServerId)
        serverDescription.selectAll()
        serverDescription.forceActiveFocus()
    }

    function onOk(project) {
        var srvAccessType = serverAccessTypeItems.get(serverAccessType.currentIndex).value
        var isSshTunnel = srvAccessType === "SrvAccessSshTunnel"
        var port = isSshTunnel ? sshTunnelPort.value : null
        var sshTunnelThroughId = isSshTunnel ?
            sshTunnelThrough.model.get(sshTunnelThrough.currentIndex).value : null
        if (model.id) {
            serverEdit.model = getAppState().projectViewState.updateServer(
                origModel, serverDescription.text, ipAddress.text, txt.text,
                username.text, password.text, serverEdit.keyFilepath,
                serverTypeItems.get(serverType.currentIndex).value,
                srvAccessType, sshTunnelPort, sshTunnelThroughId,
                group.editText);
        } else {
            getAppState().projectViewState.addServer(
                project.id, serverDescription.text, ipAddress.text,
                txt.text, username.text, password.text, serverEdit.keyFilepath,
                serverTypeItems.get(serverType.currentIndex).value,
                srvAccessType, sshTunnelPort, sshTunnelThroughId,
                serverEdit.environment, group.editText)
        }
    }

    function isSshTunnelAccess() {
        return serverAccessTypeItems.get(
            serverAccessType.currentIndex).value === "SrvAccessSshTunnel"
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
                ListElement { text: "SSH tunnel"; value: "SrvAccessSshTunnel"}
            }
        }

        Text {
            text: "SSH tunnel port:"
            visible: isSshTunnelAccess()
        }
        SpinBox {
            id: sshTunnelPort
            Layout.fillWidth: true
            minimumValue: 1025
            maximumValue: 65535
            value: serverEdit.model.sshTunnelPort ||
                getAppState().projectListState.getNewSshTunnelPort()
            visible: isSshTunnelAccess()
        }

        Text {
            text: "SSH tunnel through:"
            visible: isSshTunnelAccess()
        }
        ComboBox {
            id: sshTunnelThrough
            Layout.fillWidth: true
            textRole: "text"
            model: ListModel {}
            visible: isSshTunnelAccess()
        }

        Rectangle {
            Layout.columnSpan: 2
            Layout.fillWidth: true
            visible: isSshTunnelAccess()
            height: 55
            TextArea {
                anchors.fill: parent
                style: TextAreaStyle {
                    backgroundColor: "yellow"
                }
                text: "SSH tunnels only work if over a single hop and if the login is passwordless from the second host."
                readOnly: true
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
