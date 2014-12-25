import QtQuick 2.0

Rectangle {
	id: toolbarRoot
	color: 'light gray'
	width: parent.width
	height: 32

	signal loadView(string name, int displayId, variant displayPath, variant actions)

	/**
	 * List of sublevels in the hierarchy
	 * (breadcrumbs) -- string[]
	 */
	property variant displayPath: []

	/**
	 * actions to display in the toolbar
	 * each action is [name, icon, text]
	 * When an action is clicked, the
	 * actionTriggered signal is emitted.
	 */
	property variant actions: []

	signal actionTriggered(string name);

	Flow {
		x: 5
		width: parent.width-x
		height: parent.height
		spacing: 5

		IconButton {
			text: 'home'
			iconName: 'glyphicons-21-home'
			onClicked: loadView("ProjectList.qml", null, [])
		}
		Repeater {
			model: displayPath

			Text {
				text: modelData
				height: toolbarRoot.height
				verticalAlignment: Text.AlignVCenter
			}
		}
	}

	Flow {
		anchors.right: parent.right
		Repeater {
			id: rightActions
			model: actions
			IconButton {
				iconName: modelData[1]
				text: modelData[2]
				onClicked: actionTriggered(modelData[0])
			}
		}
	}
}
