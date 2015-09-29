import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "core"

import "utils.js" as Utils

Rectangle {
    id: projectEdit
    color: "light grey"
    property int preferredHeight: 160
    property variant appContext: null
    property string iconFilePath

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

    function activate(_model) {
        origModel = _model
        projectEdit.model = Utils.deepCopy(_model)
        var iconPath
        if (projectEdit.model.hasCustomIcon) {
            iconPath = Utils.projectGetCustomIcon(projectEdit.model)
        } else {
            iconPath = ""
        }
        projectIconButton.iconSource = iconPath
        projectEdit.iconFilePath = "file://" + iconPath
        envDevelopment.checked = projectEdit.model.hasDev
        envUat.checked = projectEdit.model.hasUat
        envStaging.checked = projectEdit.model.hasStaging
        envProd.checked = projectEdit.model.hasProd
        projectNameEntry.selectAll()
        projectNameEntry.forceActiveFocus()
    }

    function onOk() {
        var oneEnv = envDevelopment.checked || envUat.checked ||
            envStaging.checked || envProd.checked;
        if (!oneEnv) {
            appContext.errorMessage("Pick at least one environment! (Development, UAT, ...)");
            return
        }
        var iconPath = customIconCb.checked ? projectEdit.iconFilePath : ""
        if (model.id) {
            projectEdit.model = getAppState().projectListState.updateProject(
                origModel, projectNameEntry.text,
                iconPath,
                envDevelopment.checked, envUat.checked,
                envStaging.checked, envProd.checked)
        } else {
            getAppState().projectListState.addProject(
                projectNameEntry.text, iconPath,
                envDevelopment.checked, envUat.checked,
                envStaging.checked, envProd.checked)
        }
        getAppState().projectListState.copyProjectIcons()
        popup.doClose()
        /* TODO now directly open the new project */
    }

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
