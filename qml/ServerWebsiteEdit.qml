import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

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
        origModel = _model
        srvWebsiteEdit.model = Utils.deepCopy(_model)
        description.selectAll()
        description.forceActiveFocus()

        var groups = serverViewState.getServerGroupNames(server.id)
        group.model.clear()
        groups.forEach(function(grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)

        var dbs = serverViewState.getAllDatabases()
        database.model.clear()
        database.model.append({"text": "No database", "value": -1})
        dbs.forEach(function(db) {
            database.model.append({"text": db.desc, "value": db.id})
        })
        var actualIndex = Utils.listModelGetValueIndex(database.model, _model.serverDatabaseId)
        database.currentIndex = Math.max(actualIndex, 0) // want "No db" if nothing.
    }

    function onOk(server) {
        var dbId = database.model.get(database.currentIndex).value
        if (dbId === -1) {
            dbId = null
        }
        if (model.id) {
            srvWebsiteEdit.model = serverViewState.updateServerWebsite(
                origModel, description.text, url.text, txt.text,
                username.text, password.text, dbId, group.editText)
        } else {
            serverViewState.addServerWebsite(server.id,
                description.text, url.text, txt.text,
                username.text, password.text, dbId, group.editText)
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
        ComboBox {
            id: database
            Layout.fillWidth: true
            textRole: "text"
            model: ListModel {}
        }
    }
}
