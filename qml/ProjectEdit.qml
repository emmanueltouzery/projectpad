import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	width: 180; height: 180
	x: -400
	id: projectEdit
	color: "light grey"

	property variant model : {"name": "Project name"}

	function activate(_model) {
		projectEdit.model = _model
		projectNameEntry.selectAll()
		projectNameEntry.forceActiveFocus()
	}

	function closePopup() {
		projectEdit.width = 180
		projectEdit.x = -400
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

		Button {
			text: "OK"
			onClicked: {
				projectListState.addProject(projectNameEntry.text)
				projectEdit.closePopup()
				/* TODO now directly open the new project */
			}
		}

		Button {
			text: "Cancel"
			onClicked: projectEdit.closePopup()
		}
	}
}
