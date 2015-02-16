import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
	id: srvWebsiteEdit
	color: "light grey"
	property int preferredHeight: 200

	property variant model: getDefaultModel()

	function getDefaultModel() {
		return {"desc": "New server website", "url": "",
					"username": "", "password": "", "serverDatabaseId": -1}
	}

	function activate(_model) {
		srvWebsiteEdit.model = _model
		description.selectAll()
		description.forceActiveFocus()

		var dbs = serverViewState.getAllDatabases()
		database.model.clear()
			database.model.append({"text": "No database", "value": -1})
		for (var i=0;i<dbs.length;i++) {
			var db = dbs[i]
			database.model.append({"text": db.name, "value": db.id})
		}
		var actualIndex = Utils.listModelGetValueIndex(database.model, _model.serverDatabaseId)
		database.currentIndex = Math.max(actualIndex, 0) // want "No db" if nothing.
	}

	function onOk() {
		var dbId = database.model.get(database.currentIndex).value
		if (dbId === -1) {
			dbId = null
		}
		if (model.id) {
			srvWebsiteEdit.model = serverViewState.updateServerWebsite(
				model, description.text, url.text,
				username.text, password.text, dbId)
		} else {
			serverViewState.addServerWebsite(description.text, url.text,
				username.text, password.text, dbId)
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
