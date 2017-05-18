import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "core"

import "utils.js" as Utils

Rectangle {
    id: projectEdit
    color: "light grey"
    property variant appContext: null
    height: childrenRect.height
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
        envPicker.setChecked({
            dev: projectEdit.model.hasDev,
            uat: projectEdit.model.hasUat,
            stg: projectEdit.model.hasStaging,
            prd: projectEdit.model.hasProd
        })
        projectNameEntry.selectAll()
        projectNameEntry.forceActiveFocus()
    }

    function onOk() {
        var checkedEnvs = envPicker.getChecked()
        var oneEnv = checkedEnvs.dev || checkedEnvs.uat ||
            checkedEnvs.stg || checkedEnvs.prd
        if (!oneEnv) {
            appContext.errorMessage("Pick at least one environment! (Development, UAT, ...)");
            return
        }
        var iconPath = customIconCb.checked ? projectEdit.iconFilePath : ""
        if (model.id) {
            projectEdit.model = getAppState().projectListState.updateProject(
                origModel, projectNameEntry.text,
                iconPath,
                checkedEnvs.dev, checkedEnvs.uat,
                checkedEnvs.stg, checkedEnvs.prd)
        } else {
            getAppState().projectListState.addProject(
                projectNameEntry.text, iconPath,
                checkedEnvs.dev, checkedEnvs.uat,
                checkedEnvs.stg, checkedEnvs.prd)
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

        EnvironmentPicker {
            id: envPicker
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
