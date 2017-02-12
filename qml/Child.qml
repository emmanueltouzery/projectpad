import QtQuick 2.3
import QtQuick.Window 2.2
import QtQuick.Controls 1.2
import QtQuick.Dialogs 1.2
import QtQuick.Layouts 1.1
import "core"

import "utils.js" as Utils

Dialog {
    id: root
    title: "Add item"
    // modality: Qt.WindowModal
    standardButtons: StandardButton.Ok | StandardButton.Cancel
    // standardButtons: StandardButton.NoButton
    // flags: Qt.FramelessWindowHint
    // width: 100; height: 100

    property var basicModel : {
        "name":       "Project name",
        "hasDev":     false,
        "hasUat":     false,
        "hasStaging": false,
        "hasProd":    true,
        "hasCustomIcon": false
    }
    property var model: Utils.deepCopy(basicModel)
    property var origModel

    GridLayout {
        y: 10
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 10
        columns: 2
        height: 40

        Text {
            text: "Project name:"
        }

        TextField {
            Layout.fillWidth: true
            id: projectNameEntry
            text: projectEdit.model.name
        }

        CheckBox {
            id: customIconCb
            text: "Custom icon"
            checked: projectEdit.model.hasCustomIcon
        }

        Button {
            id: projectIconButton
            text: projectEdit.model.hasCustomIcon ? "Change icon" : "Pick icon"
            enabled: customIconCb.checked
            onClicked: fileDialog.visible = true
        }

        GridLayout {
            Layout.fillWidth: true
            Layout.columnSpan: 2
            columns: 2

            IconButton {
                id: envDevelopment
                iconX: 10
                iconTextPadding: 5
                Layout.fillWidth: true
                iconName: "glyphicons-361-bug"
                btnText: "Development"
                checkable: true
            }
            IconButton {
                id: envUat
                iconX: 10
                iconTextPadding: 5
                Layout.fillWidth: true
                iconName: "glyphicons-534-lab"
                btnText: "UAT"
                checkable: true
            }
            IconButton {
                id: envStaging
                iconX: 10
                iconTextPadding: 5
                Layout.fillWidth: true
                iconName: "glyphicons-140-adjust-alt"
                btnText: "Staging"
                checkable: true
            }
            IconButton {
                id: envProd
                iconX: 10
                iconTextPadding: 5
                Layout.fillWidth: true
                iconName: "glyphicons-333-certificate"
                btnText: "PROD"
                checkable: true
            }
        }
    }

    FileDialog {
        id: fileDialog
        title: "Please choose a PNG file"
        visible: false
        onAccepted: {
            projectEdit.iconFilePath = fileDialog.fileUrls[0]
            projectIconButton.iconSource = projectEdit.iconFilePath
        }
    }
}
