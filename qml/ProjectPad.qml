import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Window {
	width: 800; height: 600;
	title: 'ProjectPAD';
	visible: true;
	id: window

	function loadViewAction(name, model, displayPath) {
		loader.setSource(name, {"model": model})
		toolbar.displayPath = displayPath
	}

	Toolbar {
		id: toolbar
		onLoadView: loadViewAction(name, model, displayPath)
		onActionTriggered: loader.item.actionTriggered(name)
	}

	Loader {
		width: parent.width
		y: toolbar.height
		height: {
			var baseHeight = parent.height-toolbar.height
			if (toolbar.editMode) {
				return baseHeight-editModeActionBar.height
			} else {
				return baseHeight
			}
		}
		id: loader
		source: "ProjectList.qml"
		onLoaded: {
			// putting it here ensures it's called
			// also for the first screen of the app
			// which is not displayed as the result
			// of a click.
			toolbar.actions = loader.item.actions
		}
	}

	EditModeActionBar {
		id: editModeActionBar
		y: loader.y + loader.height
		visible: toolbar.editMode
	}

	signal loadView(string name, variant model, variant displayPath)

	Connections {
		target: loader.item
		onLoadView: {
			loadViewAction(name, model, displayPath)
		}
	}

	Popup {
		id: popup
		visible: false
	}
}
