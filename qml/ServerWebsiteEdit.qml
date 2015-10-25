import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils
import "core"

Rectangle {
    id: srvWebsiteEdit
    color: "light grey"
    property int preferredHeight: 260

    property variant model: getDefaultModel()
    property var origModel

    function getDefaultModel() {
        return {"desc": "New server website", "url": "", "text": "",
            "username": "", "password": "", "serverDatabaseId": -1}
    }

    function activate(server, _model) {
        popupDbPicker.visible = false
        origModel = _model
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
        var dbs = getAppState().serverViewState.getAllDatabases()
        var dbsWithId = dbs.filter(function (d) { return d.id == model.serverDatabaseId; });
        databaseButton.text = dbsWithId.length == 1 ? dbsWithId[0].desc : "..."
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
        Button {
            id: databaseButton
            Layout.fillWidth: true
            onClicked: {
                // must init everytime because the OK button gets disconnected after use
                initDbPickerPopup()
                popupDbPicker.visible = true
                popup.shadeHeader()
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
        }
    }

    function initDbPickerPopup() {
        popupDbPicker.setContents(
            "Pick a database", dbPicker,
            function (databasePicker) {
                databasePicker.setSelectedItem(model.serverDatabaseId)
            },
            function (databasePicker) {
                model.serverDatabaseId = databasePicker.getSelectedItem().id
                updateDbButtonText()
            })
    }

    Component.onCompleted: {
        initDbPickerPopup()
        popupDbPicker.visible = false
    }
}
