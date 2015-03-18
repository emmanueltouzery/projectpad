import QtQuick 2.0
import QtQuick.Controls 1.3

Rectangle {
    color: "light grey"
    property int preferredHeight: 200

    property variant popupContext: null

    function init() {
        popupContext.implicitClose = false
        popupContext.setOkButtonText("Next")
    }

    function onNext() {
        if (addServerPoi.checked) {
				popupContext.wizardReplaceContents("Add point of interest", editPoiComponent,
						function (poiEdit) {
							poiEdit.activate(poiEdit.getDefaultModel())
						},
						function (poiEdit) {
							poiEdit.onServerOk();
							//poisrepeater.model = serverViewState.getPois(pv.model.id)
						})
        } else if (addServerWebsite.checked) {
				popupContext.wizardReplaceContents("Add website", editSrvWwwComponent,
						function (wwwEdit) {
							wwwEdit.activate(wwwEdit.getDefaultModel())
						},
						function (wwwEdit) {
							wwwEdit.onOk();
							wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
						})
        } else if (addServerDatabase.checked) {
				popupContext.wizardReplaceContents("Add database", editDatabaseComponent,
						function (dbEdit) {
							dbEdit.activate(dbEdit.getDefaultModel())
						},
						function (dbEdit) {
							idbEdit.onOk();
							dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
						})
        }
        //popupContext.implicitClose = false // is that really needed?
    }

    ExclusiveGroup { id: addGroup }

    Column {
        spacing: 10
        anchors.verticalCenter: parent.verticalCenter
        x: 10

        RadioButton {
            id: addServerPoi
            text: "Add point of interest"
            exclusiveGroup: addGroup
            checked: true
        }
        RadioButton {
            id: addServerWebsite
            text: "Add website"
            exclusiveGroup: addGroup
        }
        RadioButton {
            id: addServerDatabase
            text: "Add database"
            exclusiveGroup: addGroup
        }
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
}
