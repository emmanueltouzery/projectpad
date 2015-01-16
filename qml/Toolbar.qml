import QtQuick 2.0
import QtQuick.Controls 1.2

Rectangle {
	id: toolbarRoot
	color: 'light gray';
	width: parent.width
	height: 32

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
			style: breadcbrumbsButton
			height: toolbarRoot.height
		}
		Repeater {
			model: pathLinks

			Button {
				text: modelData.display
				height: toolbarRoot.height
				visible: !toolbarRoot.editMode
				onClicked: loadView(modelData.screen, modelData.model)
				style: breadcbrumbsButton
			}
		}
		ExclusiveGroup { id: tabPositionGroup }
		Button {
			text: title
			height: toolbarRoot.height
			visible: title.length > 0 && !toolbarRoot.editMode
			checkable: true
			checked: true
			exclusiveGroup: tabPositionGroup
			style: breadcbrumbsButton
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
	}

	Component {
		id: defaultButtonStyle
		DefaultButtonStyle {}
	}
	Component {
		id: normalButtonStyle
		NormalButtonStyle {}
	}
	Component {
		id: breadcbrumbsButton
		BreadcrumbsButton {}
	}
}
