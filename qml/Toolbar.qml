import QtQuick 2.0
import QtQuick.Controls 1.3

Rectangle {
	id: toolbarRoot
	color: 'light gray';
	width: parent.width
	height: 32+toolbarPadding*2

	property int toolbarPadding: 3

	signal loadView(string name, variant model)

	signal toggleMenu()
	signal toggleSearch()

	/**
	 * actions to display in the toolbar
	 * each action is [name, icon, text]
	 * When an action is clicked, the
	 * actionTriggered signal is emitted.
	 */
	property variant actions: []

	property variant pathLinks: []

	property string title: ""

	function setMenuDisplayed(displayed) {
		menuBtn.checked = displayed
	}

	signal actionTriggered(string name);

	Flow {
		y: toolbarPadding
		x: toolbarPadding
		width: parent.width-x-toolbarPadding*2
		height: parent.height-toolbarPadding*2

		IconButton {
			btnText: 'home'
			iconName: 'glyphicons-21-home'
			onClicked: loadView("ProjectList.qml", null)
			style: breadcbrumbsButton
			height: parent.height
			checked: pathLinks.length === 0 && title.length === 0
		}
		Repeater {
			model: pathLinks

			Button {
				text: modelData.display
				height: parent.height
				onClicked: loadView(modelData.screen, modelData.model)
				style: breadcbrumbsButton
			}
		}
		ExclusiveGroup { id: tabPositionGroup }
		Button {
			text: title
			height: parent.height
			visible: title.length > 0
			checkable: true
			checked: true
			exclusiveGroup: tabPositionGroup
			style: breadcbrumbsButton
		}
	}

	Flow {
		id: flow
		y: toolbarPadding
		anchors.right: parent.right
		anchors.rightMargin: toolbarPadding
		height: parent.height-toolbarPadding*2
		Repeater {
			id: rightActions
			model: actions
			IconButton {
				iconName: modelData[1]
				btnText: modelData[2]
				onClicked: actionTriggered(modelData[0])
		//		style: normalButtonStyle
				height: flow.height
			}
		}
		IconButton {
			id: searchBtn
			iconName: 'glyphicons-28-search'
			iconSize: 20
			onClicked: toggleSearch()
			height: parent.height
			checkable: true
		}
		IconButton {
			id: menuBtn
			width: 30
			iconX: 12
			iconName: 'glyphicons-518-option-vertical'
			iconSize: 20
			onClicked: toggleMenu()
			height: parent.height
			style: menuButton
			checkable: true
		}
	}

	Action {
		id: searchAction
		shortcut: "Ctrl+s"
		onTriggered: { searchBtn.checked = !searchBtn.checked; toggleSearch() }
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
	Component {
		id: menuButton
		MenuButton {}
	}
}
