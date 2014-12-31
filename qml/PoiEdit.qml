import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	id: poiEdit
	color: "light grey"
	property int preferredHeight: 200

	property variant model: {"desc": "New point of interest", "path": "",
					"text": "", "interestType": ""}

	function activate(_model) {
		poiEdit.model = _model
		serverType.currentIndex = listModelGetValueIndex(serverType.model, _model.type)
		poiDescription.selectAll()
		poiDescription.forceActiveFocus()
	}

	function listModelGetValueIndex(listModel, value) {
		for (var i=0;i<listModel.count;i++) {
			if (listModel.get(i).value === value) {
				return i;
			}
		}
		return 0
	}

	function onOk() {
		if (model.id) {
			// poiEdit.model = projectViewState.updateServer(
			// 	model, serverDescription.text, ipAddress.text,
			// 	username.text, password.text,
			// 	serverTypeItems.get(serverType.currentIndex).value,
			// 	serverAccessTypeItems.get(serverAccessType.currentIndex).value);
			// // need also the project name in the breadcrumbs!
			// var project;
			// for (var i=0;i<projectListState.projects.length;i++) {
			// 	var curPrj = projectListState.projects[i];
			// 	if (curPrj.id === parseInt(poiEdit.model.projectId)) {
			// 		project = curPrj;
			// 		break;
			// 	}
			// }
			// loadView("ServerView.qml", poiEdit.model, [project.name, poiEdit.model.desc])
		} else {
			projectViewState.addProjectPoi(poiDescription.text, path.text,
				text.text, interestTypeItems.get(interestType.currentIndex).value)
		}
	}

	function onServerOk() {
		if (model.id) {
			// poiEdit.model = projectViewState.updateServer(
			// 	model, serverDescription.text, ipAddress.text,
			// 	username.text, password.text,
			// 	serverTypeItems.get(serverType.currentIndex).value,
			// 	serverAccessTypeItems.get(serverAccessType.currentIndex).value);
			// // need also the project name in the breadcrumbs!
			// var project;
			// for (var i=0;i<projectListState.projects.length;i++) {
			// 	var curPrj = projectListState.projects[i];
			// 	if (curPrj.id === parseInt(poiEdit.model.projectId)) {
			// 		project = curPrj;
			// 		break;
			// 	}
			// }
			// loadView("ServerView.qml", poiEdit.model, [project.name, poiEdit.model.desc])
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
			text: "Text:"
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
				ListElement { text: "Data file"; value: "PoiDataFile"}
				ListElement { text: "Command to run"; value: "PoiCommandToRun"}
			}
		}
	}
}
