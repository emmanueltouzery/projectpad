import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
    id: poiEdit
    color: "light grey"
    property int preferredHeight: 190
    property bool isServerPoi

    property variant model: getDefaultModel()

    function getDefaultModel() {
        return {"desc": "New point of interest", "path": "",
                    "text": "", "interestType": ""}
    }

    function getTextLabel() {
        switch (model.interestType) {
            case "PoiCommandToRun":
                return "Command:";
            default:
                return "Text:";
        }
    }

    function activate(parent, _model) {
        poiEdit.model = _model
        interestType.currentIndex = Math.max(0, Utils.listModelGetValueIndex(interestType.model, _model.interestType))
        poiDescription.selectAll()
        poiDescription.forceActiveFocus()

        var groups = isServerPoi
            ? serverViewState.getServerGroupNames(parent.id)
            : projectViewState.getProjectGroupNames(parent.id)
        group.model.clear()
        groups.forEach(function(grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)
    }

    function onOk(project) {
        if (model.id) {
            poiEdit.model = projectViewState.updateProjectPoi(
                model, poiDescription.text, path.text, text.text,
                interestTypeItems.get(interestType.currentIndex).value,
                group.editText)
        } else {
            projectViewState.addProjectPoi(
                project.id, poiDescription.text, path.text, text.text,
                interestTypeItems.get(interestType.currentIndex).value,
                group.editText)
        }
    }

    function onServerOk(server) {
        if (model.id) {
            poiEdit.model = serverViewState.updateServerPoi(
                model, poiDescription.text, path.text, text.text,
                interestTypeItems.get(interestType.currentIndex).value,
                group.editText)
        } else {
            serverViewState.addServerPoi(server.id,
                poiDescription.text, path.text,
                text.text, interestTypeItems.get(interestType.currentIndex).value,
                group.editText)
        }
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
            id: poiDescription
            Layout.fillWidth: true
            text: poiEdit.model.desc
        }

        Text {
            text: "Path:"
        }
        TextField {
            id: path
            Layout.fillWidth: true
            text: poiEdit.model.path
        }

        Text {
            text: getTextLabel()
        }
        TextField {
            id: text
            Layout.fillWidth: true
            text: poiEdit.model.text
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

        Text {
            text: "Interest type:"
        }
        ComboBox {
            id: interestType
            currentIndex: 0
            Layout.fillWidth: true
            model: ListModel {
                id: interestTypeItems
                ListElement { text: "Application"; value: "PoiApplication"}
                ListElement { text: "Log file"; value: "PoiLogFile"}
                ListElement { text: "Config file"; value: "PoiConfigFile"}
                ListElement { text: "Command to run"; value: "PoiCommandToRun"}
            }
        }
    }
}
