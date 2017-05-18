import QtQuick 2.0
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.3

import "utils.js" as Utils

Rectangle {
    id: projectNoteEdit
    color: "light grey"
    property bool widthResize: true
    property bool heightResize: true
    property variant appContext: null

    property variant model: getDefaultModel()
    property var origModel

    function getDefaultModel() {
        return {"title":"New note", "contents":"",
                "hasDev": false, "hasUat": false,
                "hasStaging": false, "hasProd": true}
    }

    function activate(parent, _model) {
        origModel = _model
        model = Utils.deepCopy(_model)
        richText.setEditMode(false)
        var groups = getAppState().projectViewState.getProjectGroupNames(parent.id)
        group.model.clear()
        groups.forEach(function (grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)
        envPicker.setChecked({
            dev: model.hasDev,
            uat: model.hasUat,
            stg: model.hasStaging,
            prd: model.hasProd
        })
        richText.editModeChanged()
        title.selectAll()
        title.forceActiveFocus()
    }

    function onOk(project) {
        var checkedEnvs = envPicker.getChecked()
        var oneEnv = checkedEnvs.dev || checkedEnvs.uat ||
            checkedEnvs.stg || checkedEnvs.prd
        if (!oneEnv) {
            appContext.errorMessage("Pick at least one environment! (Development, UAT, ...)");
            return
        }
        if (model.id) {
            model = getAppState().projectViewState.updateProjectNote(
                origModel, title.text, richText.getText(),
                checkedEnvs.dev, checkedEnvs.uat,
                checkedEnvs.stg, checkedEnvs.prd,
                group.editText);
        } else {
            getAppState().projectViewState.addProjectNote(
                project.id, title.text, richText.getText(),
                checkedEnvs.dev, checkedEnvs.uat,
                checkedEnvs.stg, checkedEnvs.prd,
                group.editText)
        }
        popup.doClose()
    }

    GridLayout {
        y: 10
        anchors.left: parent.left
        anchors.right: parent.right
        height: parent.height - 15
        anchors.margins: 10
        columns: 2

        Text {
            text: "Title:"
        }
        TextField {
            id: title
            Layout.fillWidth: true
            text: model.title
        }

        Text {
            text: "Group:"
        }

        ComboBox {
            id: group
            Layout.fillWidth: true
            textRole: "text"
            model: ListModel {}
            editable: true
        }

        EnvironmentPicker {
            id: envPicker
        }

        RichText {
            id: richText
            Layout.columnSpan: 2
            Layout.fillWidth: true
        }
    }
}
