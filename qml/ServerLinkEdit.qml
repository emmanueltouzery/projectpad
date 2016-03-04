import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils
import "core"

Rectangle {
    id: serverLinkEdit
    color: "light grey"
    property int preferredHeight: 130

    property variant model: getDefaultModel()
    property var origModel
    property string environment

    function getDefaultModel() {
        return {"desc": "New server link"}
    }

    function activate(parent, _model, _environment) {
        origModel = _model
        serverLinkEdit.model = Utils.deepCopy(_model)
        serverLinkEdit.environment = _environment
        var groups = getAppState().projectViewState.getProjectGroupNames(parent.id)
        group.model.clear()
        groups.forEach(function (grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)
        updateServerButtonText()
        serverLnkDescription.selectAll()
        serverLnkDescription.forceActiveFocus()
    }

    function updateServerButtonText() {
        var server = getAppState().projectListState.getServerById(model.serverId)
        serverButton.text = server ? server.desc : "..."
    }

    function onOk(project) {
        if (model.id) {
            serverLinkEdit.model = getAppState().projectViewState.updateServerLink(
                origModel, serverLnkDescription.text, model.serverId, group.editText);
        } else {
            getAppState().projectViewState.addServerLink(
                project.id, serverLnkDescription.text, model.serverId,
                serverLinkEdit.environment, group.editText)
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
            id: serverLnkDescription
            Layout.fillWidth: true
            text: serverLinkEdit.model.desc
        }

        Text {
            text: "Server:"
        }
        Button {
            id: serverButton
            Layout.fillWidth: true
            onClicked: {
                // must init everytime because the OK button gets disconnected after use
                // and also to update the search view filter text
                initServerPickerPopup()
                popupServerPicker.visible = true
                popup.shadeHeader()
            }
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
    }

    Popup {
        id: popupServerPicker
        visible: false
        embedLevel: 1
    }
    Component {
        id: serverPicker
        EntityPicker {
            entityType: "ServerEntityType"
        }
    }

    function initServerPickerPopup() {
        popupServerPicker.setContents(
            "Pick a server", serverPicker,
            function (serverPicker) {
                serverPicker.focusSearch(
                    serverButton.text === "..." ? null : serverButton.text)
                serverPicker.setSelectedItem(model.serverId)
            },
            function (serverPicker) {
                var selectedServer = serverPicker.getSelectedItem()
                if (selectedServer) {
                    model.serverId = selectedServer.id
                } else {
                    model.serverId = null
                }
                updateServerButtonText()
            })
    }

    Component.onCompleted: {
        initServerPickerPopup()
        popupServerPicker.visible = false
    }
}
