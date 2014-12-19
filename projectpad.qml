import QtQuick 2.0
import QtQuick.Window 2.0

Window {
	width: 800; height: 600;
	title: 'ProjectPAD';
	visible: true;

	Flow {
		anchors.fill: parent
		anchors.margins: 4
		spacing: 10

		Text {
			width: 20; height: 20
			text: name
		}


		Repeater {
			id: itemsrepeater
			model: projects

			Text {
				width: 20; height: 20
				text: modelData.name
			}
		}
	}
}
