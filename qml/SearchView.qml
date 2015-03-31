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
					Flow {
						id: projectFlow
						width: searchView.width
						Text {
							width: searchView.width
							height: 40
							text: modelData.project.name
						}
						Repeater {
							model: modelData.pois
							TileProjectPoi {}
						}
						Repeater {
							model: modelData.servers
							Flow {
								id: serverFlow
								width: searchView.width
								Text {
									width: searchView.width
									height: 40
									text: modelData.server.desc
								}
								Repeater {
									model: modelData.extraUsers
									TileExtraUserAccount {}
								}
								Repeater {
									model: modelData.websites
									TileServerWebsite {}
								}
								Repeater {
									model: modelData.pois
									TileServerPoi {}
								}
							}
						}
					}
				}
			}
		}
	}
}
