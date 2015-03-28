import QtQuick 2.0
import QtQuick.Window 2.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "utils.js" as Utils
import "poiactions.js" as PoiActions

Rectangle {
	id: pv
	anchors.fill: parent
	signal loadView(string name, variant model)
	property variant model
	property variant appContext: null

	property bool editMode

	property variant actions: [
		["addsrv", "glyphicons-470-server-new", "Add server"],
		["addpoi", "glyphicons-336-pushpin", "Add point of interest"]]

	function getBreadCrumbs() {
		return {pathLinks: [], title: model.project.name + " " + PoiActions.envDesc(model.environment)};
	}

	function editServer(curServer) {
		popup.setContents("Edit server", serverEditComponent,
			function (serverEdit) {
				serverEdit.activate(curServer, model.environment)
			},
			function (serverEdit) {
				serverEdit.onOk()
				// force refresh
				itemsrepeater.model = projectViewState.getServers(pv.model.project.id, pv.model.environment)
			})
	}

	function editPoi(curPoi) {
		popup.setContents("Edit point of interest", poiEditComponent,
			function (poiEdit) {
				poiEdit.activate(curPoi)
			},
			function (poiEdit) {
				poiEdit.onOk()
				// force refresh
				poisrepeater.model = projectViewState.getPois(pv.model.project.id)
			})
	}

	function actionTriggered(name) {
		switch (name) {
			case "addsrv":
				popup.setContents("Add server", serverEditComponent,
						function (serverEdit) {
							serverEdit.activate(serverEdit.getDefaultModel(), model.environment)
						},
						function (serverEdit) {
							serverEdit.onOk()
							// force refresh
							itemsrepeater.model = projectViewState.getServers(pv.model.project.id, model.environment)
						})
				break;
			case "addpoi":
				popup.setContents("Add point of interest", poiEditComponent,
						function (poiEdit) {
							poiEdit.activate(poiEdit.getDefaultModel())
						},
						function (poiEdit) {
							poiEdit.onOk();
							// force refresh
							poisrepeater.model = projectViewState.getPois(pv.model.project.id)
						})
				break;
		}
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
					id: itemsrepeater
					model: projectViewState.getServers(pv.model.project.id, pv.model.environment)

					TileServer {
					}
				}

				Repeater {
					id: poisrepeater
					model: projectViewState.getPois(pv.model.project.id)

					ItemTile {
						itemDesc: modelData.desc
						icon: PoiActions.actions[modelData.interestType].icon
						property int modelId: modelData.id
						property bool selected: false
						color: "light gray"
						border.width: selected ? 4 : 0
						border.color: "green"

						MouseArea {
							anchors.fill: parent
							onClicked: {
								selectMenu.options = [[PoiActions.actions[modelData.interestType].icon, function() {
									var info = projectViewState.runPoiAction(modelData)
									appContext.progressMessage("\nStarted program\n")
									}],
									["glyphicons-151-edit", function() {editPoi(modelData)}],
									["glyphicons-193-circle-remove", function() {
										appContext.confirmDelete(function() {
											projectViewState.deleteProjectPois([modelData.id])
											// force refresh
											poisrepeater.model = projectViewState.getPois(pv.model.project.id)
										})
									}]]
								selectMenu.show(parent)
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
				id: projectEditComponent
				ProjectEdit {
					id: projectEdit
				}
			}
			Component {
				id: serverEditComponent
				ServerEdit {
					id: serverEdit
				}
			}
			Component {
				id: poiEditComponent
				PoiEdit {
					id: poiEdit
				}
			}

			FileDialog {
				id: saveAuthKeyDialog
				title: "Please choose a destination"
				property variant server
				visible: false
				selectFolder: true
				onAccepted: {
					projectViewState.saveAuthKey(fileUrls[0]
						+ "/" + server.authKeyFilename, server)
					appContext.successMessage("Saved file to "
						+ fileUrls[0] + "/" + server.authKeyFilename)
				}
			}
		}
	}
}
