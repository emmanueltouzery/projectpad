import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "server-menu.js" as ServerMenu

Rectangle {
	id: searchView
	anchors.fill: parent
	signal loadView(string name, variant model)
	property variant model
	property variant appContext: null

	function getBreadCrumbs() {
		return {pathLinks: [], title: 'Search'};
	}

	function refreshSearch() {
		loadView("SearchView.qml", {matches: search(searchView.model.query), query: searchView.model.query })
	}
	function refreshPois() { refreshSearch() }
	function refreshWwws() { refreshSearch() }
	function refreshDbs() { refreshSearch() }
	function refreshUsers() { refreshSearch() }
	function refreshProjectPois() { refreshSearch() }

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
							TileProjectPoi {
								global: searchView
							}
						}
						Repeater {
							id: serverRepeater
							model: modelData.servers
							Flow {
								id: serverFlow
								width: searchView.width
								spacing: 10
								Row {
									height: 40
									width: searchView.width
									IconButton {
										width: 30
										iconX: 12
										iconName: 'glyphicons-518-option-vertical'
										iconSize: 20
										onClicked: ServerMenu.showSelectMenu(modelData.server, undefined, refreshSearch)
										height: parent.height
									}
									Text {
										text: modelData.server.desc
										height: parent.height
										verticalAlignment: Text.AlignVCenter
									}
								}
								Repeater {
									model: modelData.extraUsers
									TileExtraUserAccount {
										model: modelData.child
										global: searchView
									}
								}
								Repeater {
									model: modelData.websites
									TileServerWebsite {
										model: modelData.child
										global: searchView
									}
								}
								Repeater {
									model: modelData.databases
									TileServerDatabase {
										model: modelData.child
										global: searchView
									}
								}
								Repeater {
									model: modelData.pois
									TileServerPoi {
										model: modelData.child
										server: modelData.server
										global: searchView
									}
								}
							}
						}
					}
				}
			}
		}
	}
	SelectMenu {
		id: selectMenu
		visible: false
		z: 3
	}
	Component {
		id: serverEditComponent
		ServerEdit {
			id: serverEdit
		}
	}
}
