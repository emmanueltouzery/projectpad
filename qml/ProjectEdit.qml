import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	anchors.left: parent.left
	anchors.right: parent.right
	id: projectEdit
	color: "light grey"
	property int preferredHeight: 120

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
	}
}
