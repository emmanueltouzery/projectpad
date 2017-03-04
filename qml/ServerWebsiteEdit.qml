import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils
import "core"

Rectangle {
    id: srvWebsiteEdit
    color: "light grey"
    height: childrenRect.height
    property variant appContext: null

    property variant model: getDefaultModel()
    property var origModel

    function getDefaultModel() {
        return {"desc": "New server website", "url": "", "text": "",
            "username": "", "password": "", "serverDatabaseId": null}
    }

    function activate(server, _model, _appContext) {
        popupDbPicker.visible = false
        origModel = _model
        appContext = _appContext
        srvWebsiteEdit.model = Utils.deepCopy(_model)
        description.selectAll()
        description.forceActiveFocus()

        var groups = getAppState().serverViewState.getServerGroupNames(server.id)
        group.model.clear()
        groups.forEach(function(grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)

        updateDbButtonText()
    }

    function updateDbButtonText() {
        var db = getAppState().projectListState.getDatabaseById(model.serverDatabaseId)
        databaseButton.text = db ? db.desc : "..."
        databaseGotoButton.visible = db
    }

    function onOk(server) {
        if (model.id) {
            srvWebsiteEdit.model = getAppState().serverViewState.updateServerWebsite(
                origModel, description.text, url.text, txt.text,
                username.text, password.text, model.serverDatabaseId, group.editText)
        } else {
            getAppState().serverViewState.addServerWebsite(server.id,
                description.text, url.text, txt.text,
                username.text, password.text, model.serverDatabaseId, group.editText)
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
            text: srvWebsiteEdit.model.desc
        }

        Text {
            text: "URL:"
        }
        TextField {
            id: url
            Layout.fillWidth: true
            text: srvWebsiteEdit.model.url
        }

        Text {
            text: "Text:"
        }
        TextField {
            id: txt
            Layout.fillWidth: true
            text: srvWebsiteEdit.model.text
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
            text: srvWebsiteEdit.model.username
        }

        Text {
            text: "Password:"
        }
        PasswordField {
            id: password
            Layout.fillWidth: true
            text: srvWebsiteEdit.model.password
        }

        Text {
            text: "Database:"
        }
        Flow {
            Layout.fillWidth: true
            Button {
                id: databaseButton
                width: parent.width - (databaseGotoButton.visible ? databaseGotoButton.width : 0)
                onClicked: {
                    // must init everytime because the OK button gets disconnected after use
                    // and also to update the search view filter text
                    initDbPickerPopup()
                    popupDbPicker.visible = true
                    popup.shadeHeader()
                }
            }
            IconButton {
                id: databaseGotoButton
                width: 37
                iconSize: 19
                iconX: 9
                iconSmooth: false
                iconName: "glyphicons-390-new-window-alt"
                onClicked: {
                    goToDb.trigger()
                }
            }
        }
    }

    Action {
        id: goToDb
        shortcut: "Ctrl+g"
        onTriggered: {
            if (databaseGotoButton.visible) {
                appContext.closePopup()
                appContext.triggerSearch(databaseButton.text)
            }
        }
    }

    Popup {
        id: popupDbPicker
        visible: false
        embedLevel: 1
    }

    Component {
        id: dbPicker
        EntityPicker {
            entityType: "DatabaseEntityType"
        }
    }

    function initDbPickerPopup() {
        popupDbPicker.setContents(
            "Pick a database", dbPicker,
            function (databasePicker) {
                databasePicker.focusSearch(
                    databaseButton.text === "..." ? null : databaseButton.text)
                databasePicker.setSelectedItem(model.serverDatabaseId)
            },
            function (databasePicker) {
                var selectedDb = databasePicker.getSelectedItem()
                if (selectedDb) {
                    model.serverDatabaseId = selectedDb.id
                } else {
                    model.serverDatabaseId = null
                }
                updateDbButtonText()
            })
    }

    Component.onCompleted: {
        initDbPickerPopup()
        popupDbPicker.visible = false
    }
}
