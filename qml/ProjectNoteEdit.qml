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
        return {"title":"New note", "contents":""}
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
        richText.editModeChanged()
        title.selectAll()
        title.forceActiveFocus()
    }

    function onOk(project) {
        if (model.id) {
            model = getAppState().projectViewState.updateProjectNote(
                origModel, title.text, richText.getText(), group.editText);
        } else {
            getAppState().projectViewState.addProjectNote(
                project.id, title.text, richText.getText(), group.editText)
        }
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

        RichText {
            id: richText
            Layout.columnSpan: 2
            Layout.fillWidth: true
        }
    }
}
