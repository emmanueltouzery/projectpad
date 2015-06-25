import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

import "utils.js" as Utils

Rectangle {
    id: projectEdit
    color: "light grey"
    property int preferredHeight: 120
    property variant appContext: null

    property variant model : {
        "name": "Project name",
        "hasDev" : "False",
        "hasUat": "False",
        "hasStaging" : "False",
        "hasProd": "True"
    }
    property var origModel

    function activate(_model) {
        origModel = _model
        projectEdit.model = Utils.deepCopy(_model)
        envDevelopment.checked = projectEdit.model.hasDev === "True"
        envUat.checked = projectEdit.model.hasUat === "True"
        envStaging.checked = projectEdit.model.hasStaging === "True"
        envProd.checked = projectEdit.model.hasProd === "True"
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
        if (model.id) {
            projectEdit.model = getAppState().projectListState.updateProject(
                origModel, projectNameEntry.text,
                envDevelopment.checked, envUat.checked,
                envStaging.checked, envProd.checked)
        } else {
            getAppState().projectListState.addProject(projectNameEntry.text,
                                  envDevelopment.checked, envUat.checked,
                                  envStaging.checked, envProd.checked)
        }
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
}
