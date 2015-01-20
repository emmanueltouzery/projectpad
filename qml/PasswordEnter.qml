import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
	id: poiEdit
	color: "light grey"
	property int preferredHeight: 100

	signal loadView(string name, variant model)

	function getPassword() {
		return passwordText.text
	}

	function activate() {
		passwordText.forceActiveFocus()
	}

	function onOk(passwdDialog, popup) {
		if (passwordText.text.length == 0) {
			// if we give to sqlcipher a 0-length password the first
			// time, then run our test query, it fails and never
			// recovers when we give passwords later.
			// No such problem when we simply give a wrong but non-empty
			// password.
			// Also, we force the user to enter a password anyway.
			introText.text = "You must enter a password"
			return
		}
		var unlockResult = setupPasswordAndUpgradeDb(passwordText.text)
		switch (unlockResult) {
			case "WrongPassword":
				introText.text = "Wrong password! Try again."
				break;
			case "Ok":
				popup.doClose()
				loadView("ProjectList.qml", null)
				break;
			case "DbNotEncrypted":
				introText.text = "FATAL ERROR: db is not encrypted!"
				break;
		}
	}

	Text {
		id: introText
		y: 10
		x: 10
		text: "Please enter the password to open the application."
	}

	GridLayout {
		anchors.top: introText.bottom
		anchors.left: parent.left
		anchors.right: parent.right
		anchors.margins: 10
		columns: 2

		Text {
			text: "Password:"
		}
		TextField {
			id: passwordText
			Layout.fillWidth: true
			echoMode: TextInput.Password
		}
	}
}
