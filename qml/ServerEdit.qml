import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
	id: serverEdit
	color: "light grey"
	property int preferredHeight: 280

	property variant model: getDefaultModel()
       
	function getDefaultModel() {
		return {"desc": "New server", "serverIp": "",
			"username": "", "password": "",
			"type": "", "accessType": ""}
	}

	function activate(_model) {
		serverEdit.model = _model
		serverType.currentIndex = Utils.listModelGetValueIndex(serverType.model, _model.type)
		serverAccessType.currentIndex = Utils.listModelGetValueIndex(serverAccessType.model, _model.accessType)
		serverDescription.selectAll()
		serverDescription.forceActiveFocus()
	}

	function onOk() {
		if (model.id) {
			serverEdit.model = projectViewState.updateServer(
				model, serverDescription.text, ipAddress.text,
				username.text, password.text,
				serverTypeItems.get(serverType.currentIndex).value,
				serverAccessTypeItems.get(serverAccessType.currentIndex).value);
		} else {
			projectViewState.addServer(serverDescription.text, ipAddress.text,
				username.text, password.text,
				serverTypeItems.get(serverType.currentIndex).value,
				serverAccessTypeItems.get(serverAccessType.currentIndex).value)
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
		TextField {
			id: password
			echoMode: TextInput.Password
			Layout.fillWidth: true
			text: serverEdit.model.password
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
}
