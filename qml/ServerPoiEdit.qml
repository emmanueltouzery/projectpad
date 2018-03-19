import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
    id: poiEdit
    color: "light grey"
    height: childrenRect.height

    property variant model: getDefaultModel()
    property var origModel

    function getDefaultModel() {
        return {"desc": "New point of interest", "path": "",
                    "runOn": "RunOnServer",
                    "text": "", "interestType": ""}
    }

    function getTextLabel() {
        switch (model.interestType) {
            case "PoiCommandToRun":
            case "PoiCommandTerminal":
                return "Command:";
            default:
                return "Text:";
        }
    }

    function activate(parent, _model) {
        origModel = _model
        poiEdit.model = Utils.deepCopy(_model)
        interestType.currentIndex =
            Math.max(0, Utils.listModelGetValueIndex(
                interestType.model, _model.interestType))
        runOn.currentIndex =
            Math.max(0, Utils.listModelGetValueIndex(
                runOn.model, _model.runOn))
        poiDescription.selectAll()
        poiDescription.forceActiveFocus()

        var groups = getAppState().serverViewState.getServerGroupNames(parent.id)
        group.model.clear()
        groups.forEach(function(grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)
    }

    function onOk(server) {
        if (model.id) {
            poiEdit.model = getAppState().serverViewState.updateServerPoi(
                origModel, poiDescription.text, path.text, text.text,
                interestTypeItems.get(interestType.currentIndex).value,
                runOnItems.get(runOn.currentIndex).value,
                group.editText)
        } else {
            getAppState().serverViewState.addServerPoi(server.id,
                poiDescription.text, path.text,
                text.text, interestTypeItems.get(interestType.currentIndex).value,
                runOnItems.get(runOn.currentIndex).value,
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
            id: runOnLabel
            text: "Run on:"
            visible: model.interestType === "PoiCommandToRun" ||
                      model.interestType === "PoiCommandTerminal"
        }
        ComboBox {
            id: runOn
            currentIndex: 0
            Layout.fillWidth: true
            visible: runOnLabel.visible
            model: ListModel {
                id: runOnItems
                ListElement { text: "Server"; value: "RunOnServer"}
                ListElement { text: "Client"; value: "RunOnClient"}
            }
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
                ListElement { text: "Command to run (terminal)"; value: "PoiCommandTerminal"}
                ListElement { text: "Backup/archive"; value: "PoiBackupArchive"}
            }
        }
    }
}
