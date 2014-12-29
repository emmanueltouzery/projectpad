import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	color: "light grey"
	property int preferredHeight: 280

	function onOk() {
		projectViewState.addServer(serverDescription.text, ipAddress.text,
				username.text, password.text,
				serverTypeItems.get(serverType.currentIndex).value,
				serverAccessTypeItems.get(serverAccessType.currentIndex).value)
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
		}

		Text {
			text: "IP Address:"
		}
		TextField {
			id: ipAddress
			Layout.fillWidth: true
		}

		Text {
			text: "Username:"
		}
		TextField {
			id: username
			Layout.fillWidth: true
		}

		Text {
			text: "Password:"
		}
		TextField {
			id: password
			echoMode: TextInput.Password
			Layout.fillWidth: true
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
