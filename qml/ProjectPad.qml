import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Window {
	width: 800; height: 600;
	title: 'ProjectPAD';
	visible: true;
	id: window

	function loadViewAction(name, model) {
		loader.setSource(name, {"model": model})
	}

	Toolbar {
		id: toolbar
		onLoadView: loadViewAction(name, model)
		onActionTriggered: loader.item.actionTriggered(name)
		onEditModeChanged: loader.item.editMode = toolbar.editMode
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
			var breadcrumbsInfo = loader.item.getBreadCrumbs()
			toolbar.pathLinks = breadcrumbsInfo.pathLinks
			toolbar.title = breadcrumbsInfo.title
		}
	}

	EditModeActionBar {
		id: editModeActionBar
		y: loader.y + loader.height
		visible: toolbar.editMode
		onModeActionBarAction: loader.item.actionTriggered(type)
		onActionExecuted: toolbar.editMode = false
	}

	signal loadView(string name, variant model)

	Connections {
		target: loader.item
		onLoadView: {
			loadViewAction(name, model)
		}
		onSelectionChange: editModeActionBar.selectionCount = selection.length
	}

	Popup {
		id: popup
		visible: false
	}
}
