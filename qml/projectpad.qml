import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Window {
	width: 800; height: 600;
	title: 'ProjectPAD';
	visible: true;
	id: window

	function loadViewAction(name, displayId, displayPath) {
		console.log("load view")
		loader.source=name
		loader.item.displayId = displayId
		toolbar.displayPath = displayPath
	}

	Toolbar {
		id: toolbar
		onLoadView: loadViewAction(name, displayId, displayPath)
	}

	Loader {
		width: parent.width
		y: toolbar.height
		height: parent.height-toolbar.height
		id: loader
		source: "projectlist.qml"
	}
	signal loadView(string name, int displayId, variant displayPath)

	Connections {
		target: loader.item
		onLoadView: loadViewAction(name, displayId, displayPath)
	}
}
