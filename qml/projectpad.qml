import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Window {
	width: 800; height: 600;
	title: 'ProjectPAD';
	visible: true;
	id: window

	Toolbar {
		id: toolbar
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
		onLoadView: {
			loader.source=name
			loader.item.displayId = displayId
			toolbar.displayPath = displayPath
		}
	}
}
