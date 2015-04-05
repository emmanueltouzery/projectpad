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
			id: flickable
			anchors.fill: parent
			contentHeight: rootFlow.implicitHeight
			Flow {
				anchors.fill: parent
				anchors.margins: 4
				spacing: 10
				id: rootFlow
				Repeater {
					model: searchView.model.matches
					Flow {
						id: projectFlow
						width: searchView.width
						Rectangle {
							color: "gray"
							width: searchView.width
							height: 40
							Text {
								x: 5
								text: modelData.project.name
								height: parent.height
								verticalAlignment: Text.AlignVCenter
							}
						}
						Repeater {
							model: modelData.pois
							TileProjectPoi {
								global: rootFlow
							}
						}
						Repeater {
							id: serverRepeater
							model: modelData.servers
							Flow {
								id: serverFlow
								width: searchView.width
								spacing: 10
								Rectangle {
									height: index > 0 ? 5 : 0
									width: searchView.width
								}
								Rectangle {
									color: "dark gray"
									height: 40
									width: searchView.width
									Image {
										x: 5
										source: {
											var envIcon;
											switch (modelData.server.environment) {
												case "EnvDevelopment":
												envIcon = 'glyphicons-361-bug';
												break;
											case "EnvUat":
												envIcon = 'glyphicons-534-lab';
												break;
											case "EnvStage":
												envIcon = 'glyphicons-140-adjust-alt';
												break;
											case "EnvProd":
												envIcon = 'glyphicons-333-certificate';
												break;
											}
											return '../glyphicons-free/' + envIcon + '.png'
										}
										verticalAlignment: Image.AlignVCenter
										fillMode: Image.Pad
										height: parent.height
									}
									Text {
										x: 35
										text: modelData.server.desc
										height: parent.height
										verticalAlignment: Text.AlignVCenter
									}
									IconButton {
										width: 30
										x: parent.width - 50
										iconX: 12
										iconName: 'glyphicons-518-option-vertical'
										iconSize: 20
										onClicked: ServerMenu.showSelectMenu(modelData.server,
											parent, refreshSearch, lineSelectMenu, rootFlow)
										height: parent.height
									}
								}
								Repeater {
									model: modelData.extraUsers
									TileExtraUserAccount {
										model: modelData.child
										global: rootFlow
									}
								}
								Repeater {
									model: modelData.websites
									TileServerWebsite {
										model: modelData.child
										global: rootFlow
									}
								}
								Repeater {
									model: modelData.databases
									TileServerDatabase {
										model: modelData.child
										global: rootFlow
									}
								}
								Repeater {
									model: modelData.pois
									TileServerPoi {
										model: modelData.child
										server: modelData.server
										global: rootFlow
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
			LineSelectMenu {
				id: lineSelectMenu
				visible: false
				z: 3
			}
		}
	}
	Component {
		id: serverEditComponent
		ServerEdit {
			id: serverEdit
		}
	}
}
