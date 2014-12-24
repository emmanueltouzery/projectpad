import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Window {
	width: 800; height: 600;
	title: 'ProjectPAD';
	visible: true;
	id: window

	Loader {
		anchors.fill: parent
		id: loader
		source: "projectlist.qml"
	}
	signal loadView(string name, int displayId)

	Connections {
		target: loader.item
		onLoadView: { loader.source=name; loader.item.displayId = displayId }
	}
}
