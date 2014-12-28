import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	anchors.fill: parent
	id: projectEdit
	color: "light grey"

	property variant model : {"name": "Project name"}

	function activate(_model) {
		projectEdit.model = _model
		projectNameEntry.selectAll()
		projectNameEntry.forceActiveFocus()
	}

	function onOk() {
		if (model.id) {
			projectEdit.model = projectListState.updateProject(model, projectNameEntry.text)
			loadView("ProjectView.qml", projectEdit.model, [projectEdit.model.name])
		} else {
			projectListState.addProject(projectNameEntry.text)
		}
		/* TODO now directly open the new project */
	}

	GridLayout {
		anchors.fill: parent
		columns: 2

		Text {
			text: "Project name:"
		}

		Rectangle {
			color: "white"
			width: 200
			height: 40
			TextInput {
				font.pointSize: 22
				id: projectNameEntry
				text: projectEdit.model.name
				anchors.fill: parent
			}
		}
	}
}
