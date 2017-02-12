import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
    id: poiEdit
    color: "light grey"
    height: childrenRect.height

    signal loadView(string name, variant model)

    function getPassword() {
        return passwordText.text
    }

    function activate() {
        passwordText.forceActiveFocus()
    }

    function onOk(passwdDialog, popup) {
        if ((passwordText.text !== passwordConfirmText.text)) {
            introText.text = "The passwords don't match!"
            return
        }
        if (passwordText.text.length == 0) {
            introText.text = "You must enter a password"
            return
        }

        var setupResult = setupPasswordAndUpgradeDb(passwordText.text)
        switch (setupResult) {
            case "Ok":
                popup.doClose()
                loadView("ProjectList.qml", null)
                break;
            default:
                introText.text = "Error accessing the database! " + setupResult
                break;
        }
    }

    Text {
        id: introText
        x: 10
        y: 10
        text: "Please choose a password to protect your data."
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

        Text {
            text: "Confirm password:"
        }
        TextField {
            id: passwordConfirmText
            Layout.fillWidth: true
            echoMode: TextInput.Password
        }
    }
}
