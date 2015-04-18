import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Column {
    id: passwordEnter
    property int preferredHeight: 190

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
        var newPassword = ""
        if (checkboxChangePassword.checked) {
            if (newPasswordText.text != repeatPasswordText.text || newPasswordText.text.length == 0) {
                introText.text = "Invalid new password"
                return
            }
            newPassword = newPasswordText.text
        }
        var unlockResult = setupPasswordAndUpgradeDb(passwordText.text, newPassword)
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

    Rectangle {
        width: parent.width
        height: 110
        color: "light grey"

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

            CheckBox {
                id: checkboxChangePassword
                Layout.columnSpan: 2
                text: "Change the password"
                onClicked: {
                    changePasswordSection.visible = checked
                }
            }
        }
    }

    Rectangle {
        width: parent.width
        height: 80
        color: "transparent"

        Rectangle {
            id: changePasswordSection
            width: parent.width
            height: 80
            color: "light gray"
            visible: false

            GridLayout {
                anchors.left: parent.left
                anchors.right: parent.right
                anchors.margins: 10
                columns: 2
                Text {
                    text: "New password:"
                }
                TextField {
                    id: newPasswordText
                    Layout.fillWidth: true
                    echoMode: TextInput.Password
                }
                Text {
                    text: "Repeat new password:"
                }
                TextField {
                    id: repeatPasswordText
                    Layout.fillWidth: true
                    echoMode: TextInput.Password
                }
            }
        }
    }
}
