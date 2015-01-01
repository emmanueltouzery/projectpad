import QtQuick 2.0
import QtQuick.Controls 1.2

Rectangle {
	id: toolbarRoot
	color: {
		if (editMode) {
			return '#4a90d9';
		} else {
			return 'light gray';
		}
	}
	width: parent.width
	height: 32
	property bool editMode: false

	signal loadView(string name, variant model)

	/**
	 * actions to display in the toolbar
	 * each action is [name, icon, text]
	 * When an action is clicked, the
	 * actionTriggered signal is emitted.
	 */
	property variant actions: []

	property variant pathLinks: []

	property string title: ""

	signal actionTriggered(string name);

	Flow {
		width: parent.width-x
		height: parent.height

		IconButton {
			btnText: 'home'
			iconName: 'glyphicons-21-home'
			onClicked: loadView("ProjectList.qml", null)
			visible: !toolbarRoot.editMode
			style: normalButtonStyle
			height: toolbarRoot.height
		}
		Repeater {
			model: pathLinks

			Button {
				text: modelData.display
				height: toolbarRoot.height
				visible: !toolbarRoot.editMode
				onClicked: loadView(modelData.screen, modelData.model)
			}
		}
		Text {
			text: title
			height: toolbarRoot.height
			verticalAlignment: Text.AlignVCenter
			visible: !toolbarRoot.editMode
		}
	}

	Flow {
		anchors.right: parent.right
		Repeater {
			id: rightActions
			model: actions
			IconButton {
				iconName: modelData[1]
				btnText: modelData[2]
				onClicked: actionTriggered(modelData[0])
				visible: !toolbarRoot.editMode
				style: normalButtonStyle
				height: toolbarRoot.height
			}
		}
		IconButton {
			id: editModeBtn
			iconName: 'glyphicons-31-pencil'
			btnText: 'Edit mode'
			checkable: true
			style: checked ? defaultButtonStyle : normalButtonStyle
			onClicked: toolbarRoot.editMode = editModeBtn.checked
			height: toolbarRoot.height
		}
	}

	onEditModeChanged: editModeBtn.checked = editMode

	Component {
		id: defaultButtonStyle
		DefaultButtonStyle {}
	}
	Component {
		id: normalButtonStyle
		NormalButtonStyle {}
	}
}
