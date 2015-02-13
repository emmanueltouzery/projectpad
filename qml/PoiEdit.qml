import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "utils.js" as Utils

Rectangle {
	id: poiEdit
	color: "light grey"
	property int preferredHeight: 200

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

	function activate(_model) {
		poiEdit.model = _model
		interestType.currentIndex = Math.max(0, Utils.listModelGetValueIndex(interestType.model, _model.interestType))
		poiDescription.selectAll()
		poiDescription.forceActiveFocus()
	}

	function onOk() {
		if (model.id) {
			poiEdit.model = projectViewState.updateProjectPoi(
				model, poiDescription.text, path.text,
				text.text,
				interestTypeItems.get(interestType.currentIndex).value)
		} else {
			projectViewState.addProjectPoi(poiDescription.text, path.text,
				text.text, interestTypeItems.get(interestType.currentIndex).value)
		}
	}

	function onServerOk() {
		if (model.id) {
			poiEdit.model = serverViewState.updateServerPoi(
				model, poiDescription.text, path.text,
				text.text,
				interestTypeItems.get(interestType.currentIndex).value)
		} else {
			serverViewState.addServerPoi(poiDescription.text, path.text,
				text.text, interestTypeItems.get(interestType.currentIndex).value)
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
