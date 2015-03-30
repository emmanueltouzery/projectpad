import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	id: searchView
	anchors.fill: parent
	signal loadView(string name, variant model)
	property variant model

	function getBreadCrumbs() {
		return {pathLinks: [], title: 'Search'};
	}
	ScrollView {
		anchors.fill: parent
		Flickable {
			anchors.fill: parent
			contentHeight: flow.implicitHeight
			Flow {
				anchors.fill: parent
				anchors.margins: 4
				spacing: 10
				id: flow
				Repeater {
					model: searchView.model.matches
					Label {
						width: parent.width
						height: 40
						text: modelData.project.name
					}
				}
			}
		}
	}
}