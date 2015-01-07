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
					"username": "", "password": ""}
	}

	function activate(_model) {
		srvWebsiteEdit.model = _model
		description.selectAll()
		description.forceActiveFocus()
	}

	function onOk() {
		if (model.id) {
			srvWebsiteEdit.model = serverViewState.updateServerWebsite(
				model, description.text, url.text,
				username.text, password.text)
		} else {
			serverViewState.addServerWebsite(description.text, url.text,
				username.text, password.text)
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
		TextField {
			id: password
			echoMode: TextInput.Password
			Layout.fillWidth: true
			text: srvWebsiteEdit.model.password
		}

	}
}
