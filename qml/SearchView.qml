import QtQuick 2.0

Rectangle {
	anchors.fill: parent
	signal loadView(string name, variant model)

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
					id: projectsRepeater
					model: projectViewState.getSearchResults()
				}
			}
		}
	}
}
