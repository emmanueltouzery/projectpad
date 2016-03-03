import QtQuick 2.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "core"

Rectangle {
    id: serverLinkEdit
    color: "light grey"
    property int preferredHeight: 130

    function updateServerButtonText() {
        var server = getAppState().projectListState.getServerById(model.serverId)
        serverButton.text = server ? server.desc : "..."
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
            id: txt
            Layout.fillWidth: true
            text: serverLinkEdit.model.description
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
