import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	id: serverEdit
	color: "light grey"
	property int preferredHeight: 280

	property variant model: {"desc": "New server", "serverIp": "",
					"username": "", "password": "",
					"type": "", "accessType": ""}

	function activate(_model) {
		serverEdit.model = _model
		serverType.currentIndex = listModelGetValueIndex(serverType.model, _model.type)
		serverAccessType.currentIndex = listModelGetValueIndex(serverAccessType.model, _model.accessType)
		serverDescription.selectAll()
		serverDescription.forceActiveFocus()
	}

	function listModelGetValueIndex(listModel, value) {
		for (var i=0;i<listModel.count;i++) {
			if (listModel.get(i).value === value) {
				return i;
			}
		}
		return 0
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
				ListElement { text: "SSH"; value: "SrvSsh"}
				ListElement { text: "Remote desktop (RDP)"; value: "SrvRdp"}
			}
		}
	}
}
