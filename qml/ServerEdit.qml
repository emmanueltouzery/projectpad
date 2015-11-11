import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Controls.Styles 1.3
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "utils.js" as Utils
import "core"

Rectangle {
    id: serverEdit
    color: "light grey"
    property int preferredHeight: isSshTunnelAccess() ? 410 : 330

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
        updateSshServerButtonText()
        serverDescription.selectAll()
        serverDescription.forceActiveFocus()
    }

    function updateSshServerButtonText() {
        var sshServers = getAppState().projectListState.getAllSshServers()
        var serversWithId = sshServers.filter(function (d) {
            return d.id == model.sshTunnelThroughServerId;
        });
        sshTunnelThroughButton.text = serversWithId.length == 1 ? serversWithId[0].desc : "..."
    }

    function onOk(project) {
        var srvAccessType = serverAccessTypeItems.get(serverAccessType.currentIndex).value
        var isSshTunnel = srvAccessType === "SrvAccessSshTunnel"
        var port = isSshTunnel ? sshTunnelPort.value : null
        if (model.id) {
            serverEdit.model = getAppState().projectViewState.updateServer(
                origModel, serverDescription.text, ipAddress.text, txt.text,
                username.text, password.text, serverEdit.keyFilepath,
                serverTypeItems.get(serverType.currentIndex).value,
                srvAccessType, port, model.sshTunnelThroughServerId,
                group.editText);
        } else {
            getAppState().projectViewState.addServer(
                project.id, serverDescription.text, ipAddress.text,
                txt.text, username.text, password.text, serverEdit.keyFilepath,
                serverTypeItems.get(serverType.currentIndex).value,
                srvAccessType, port, model.sshTunnelThroughServerId,
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
        Button {
            id: sshTunnelThroughButton
            visible: isSshTunnelAccess()
            Layout.fillWidth: true
            onClicked: {
                // must init everytime because the OK button gets disconnected after use
                // and also to update the search view filter text
                initSshServerPickerPopup()
                popupSshServerPicker.visible = true
                popup.shadeHeader()
            }
        }
    }
    Popup {
        id: popupSshServerPicker
        visible: false
        embedLevel: 1
    }

    Component {
        id: sshServerPicker
        EntityPicker {
            entityType: "ServerEntityType"
            extraFilter: function(searchMatch) {
                var filteredServers = searchMatch.servers.filter(function (srv) {
                    return srv.server.accessType === "SrvAccessSsh"
                        || srv.server.accessType === "SrvAccessSshTunnel"
                })
                if (filteredServers.length === 0) {
                    // nothing for this project.
                    return null
                } else {
                    return {
                        project: searchMatch.project,
                        notes: [],
                        pois: [],
                        servers: filteredServers
                    }
                }
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

    function initSshServerPickerPopup() {
        popupSshServerPicker.setContents(
            "Pick an SSH-accessible server", sshServerPicker,
            function (sshServerPicker) {
                sshServerPicker.focusSearch(
                    sshTunnelThroughButton.text === "..." ? null : sshTunnelThroughButton.text)
                sshServerPicker.setSelectedItem(model.sshTunnelThroughServerId)
            },
            function (sshServerPicker) {
                var selectedServer = sshServerPicker.getSelectedItem()
                if (selectedServer) {
                    model.sshTunnelThroughServerId = selectedServer.id
                } else {
                    model.sshTunnelThroughServerId = null
                }
                updateSshServerButtonText()
            })
    }

    Component.onCompleted: {
        initSshServerPickerPopup()
        popupSshServerPicker.visible = false
    }
}
