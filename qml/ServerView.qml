import QtQuick 2.0
import QtQuick.Window 2.0
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
	property string _popupToDisplay

	property variant actions: [["add", "glyphicons-191-circle-plus", "Add..."]]

	function getBreadCrumbs() {
		var projectModel = Utils.findById(projectListState.projects, model.projectId)
		return {pathLinks:
			[
				{screen: "ProjectView.qml",
				model: {"project": projectModel, "environment": model.environment},
				display: projectModel.name + " " + PoiActions.envDesc(model.environment)}
			],
			title: model.desc}
	}

	function editPoi(curPoi) {
		popup.setContents("Edit point of interest", editPoiComponent,
				function (poiEdit) {
					poiEdit.activate(curPoi)
				},
				function (poiEdit) {
					poiEdit.onServerOk()
					// force refresh
					poisrepeater.model = serverViewState.getPois(pv.model.id)
				})
	}

	function editSrvWww(curPoi) {
		popup.setContents("Edit website", editSrvWwwComponent,
				function (wwwEdit) {
					wwwEdit.activate(curPoi)
				},
				function (wwwEdit) {
					wwwEdit.onOk()
					// force refresh
					wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
				})
	}

	function editDb(curDb) {
		popup.setContents("Edit database", editDatabaseComponent,
				function (dbEdit) {
					dbEdit.activate(curDb)
				},
				function (dbEdit) {
					dbEdit.onOk()
					// force refresh
					dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
				})
	}

	function editExtraUserAccount(curUserAcct) {
		popup.setContents("Edit extra user account", editExtraUserAccountComponent,
				function (userEdit) {
					userEdit.activate(curUserAcct)
				},
				function (userEdit) {
					userEdit.onOk()
					// force refresh
					useraccountsrepeater.model = serverViewState.getServerExtraUserAccounts(pv.model.id)
				})
	}

	function actionTriggered(name) {
		switch (name) {
			case "addpoi":
				popup.setContents("Add point of interest", editPoiComponent,
						function (poiEdit) {
							poiEdit.activate(poiEdit.getDefaultModel())
						},
						function (poiEdit) {
							poiEdit.onServerOk();
							poisrepeater.model = serverViewState.getPois(pv.model.id)
						}, {noOpacity: true})
				break;
			case "addwww":
				popup.setContents("Add website", editSrvWwwComponent,
						function (wwwEdit) {
							wwwEdit.activate(wwwEdit.getDefaultModel())
						},
						function (wwwEdit) {
							wwwEdit.onOk();
							wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
						}, {noOpacity: true})
				break;
			case "adddb":
				popup.setContents("Add database", editDatabaseComponent,
						function (dbEdit) {
							dbEdit.activate(dbEdit.getDefaultModel())
						},
						function (dbEdit) {
							dbEdit.onOk();
							dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
						}, {noOpacity: true})
				break;
    case "addaccount":
				popup.setContents("Add extra user account", editExtraUserAccountComponent,
						function (editUser) {
							editUser.activate(editUser.getDefaultModel())
						},
						function (editUser) {
							editUser.onOk();
							useraccountsrepeater.model = serverViewState.getServerExtraUserAccounts(pv.model.id)
						}, {noOpacity: true})
        break;
			case "add":
				popup.implicitClose = false
				popup.setContents("Add...", addServerComponent,
						function (srvAdd) {
							srvAdd.init()
						},
						function (srvAdd) {
							  var matches = {1: "addpoi", 2: "addwww", 3: "adddb", 4: "addaccount"}
							_popupToDisplay = matches[srvAdd.next()]
							displayPopupTimer.start()
						}, {okBtnText: "Next"})
				break;
		}
	}

	Timer {
		id: displayPopupTimer
		interval: 0
		onTriggered: {
			actionTriggered(_popupToDisplay)
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
					id: useraccountsrepeater
					model: serverViewState.getServerExtraUserAccounts(pv.model.id)

					TileExtraUserAccount {
					}
				}

				Repeater {
					id: wwwsrepeater
					model: serverViewState.getServerWebsites(pv.model.id)

					ItemTile {
						property int modelId: modelData.id
						property bool selected: false
						color: "light slate gray"
						border.width: selected ? 4 : 0
						border.color: "green"
						itemDesc: modelData.desc
						icon: "glyphicons-372-global"
						MouseArea {
							anchors.fill: parent
							onClicked: {
								selectMenu.options = [["glyphicons-151-edit", function() { editSrvWww(modelData)}],
									["glyphicons-372-global", function() { openAssociatedFile(modelData.url)}],
									["glyphicons-512-copy", function() { appContext.copyItem(modelData.password, true) }],
									["glyphicons-193-circle-remove", function() {
										appContext.confirmDelete(function() {
											serverViewState.deleteServerWebsites([modelData.id])
											wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
										})
									}]]
								selectMenu.show(parent)
							}
						}
					}
				}

				Repeater {
					id: dbsrepeater
					model: serverViewState.getServerDatabases(pv.model.id)

					ItemTile {
						property int modelId: modelData.id
						property bool selected: false
						width: 180; height: 180
						color: "gray"
						border.width: selected ? 4 : 0
						border.color: "green"
						itemDesc: modelData.desc
						icon: "glyphicons-528-database"

						MouseArea {
							anchors.fill: parent
							onClicked: {
								selectMenu.options = [["glyphicons-151-edit", function() { editDb(modelData)}],
									["glyphicons-512-copy", function() { appContext.copyItem(modelData.password, true) }],
									["glyphicons-193-circle-remove", function() {
										appContext.confirmDelete(function() {
											var msg = serverViewState.canDeleteServerDatabase(modelData)
											if (msg !== null) {
												appContext.errorMessage(msg)
												return
											}
											serverViewState.deleteServerDatabases([modelData.id])
											dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
										})
									}]]
								selectMenu.show(parent)
							}
						}
					}
				}

				Repeater {
					id: poisrepeater
					model: serverViewState.getPois(pv.model.id)

					TileServerPoi {
					}
				}
			}
			SelectMenu {
				id: selectMenu
				visible: false
				z: 3
			}

			Component {
				id: editPoiComponent
				PoiEdit {
					id: poiEdit
				}
			}
			Component {
				id: editSrvWwwComponent
				ServerWebsiteEdit {
					id: wwwEdit
				}
			}
			Component {
				id: editDatabaseComponent
				ServerDatabaseEdit {
					id: dbEdit
				}
			}
			Component {
				id: addServerComponent
				ServerAddPopup {
					id: srvAddPopup
				}
			}
			Component {
				id: editExtraUserAccountComponent
				ServerExtraUserAccountEdit {
					id: srvExtraUserAccountEdit
				}
			}
			FileDialog {
				id: saveAuthKeyDialog
				title: "Please choose a destination"
				property variant userAcct
				visible: false
				selectFolder: true
				onAccepted: {
					serverViewState.saveAuthKey(fileUrls[0]
						+ "/" + userAcct.authKeyFilename, userAcct)
					appContext.successMessage("Saved file to "
						+ fileUrls[0] + "/" + userAcct.authKeyFilename)
				}
			}
		}
	}
}
