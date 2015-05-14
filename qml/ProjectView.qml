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

    function refreshProjectPois() {
        poisrepeater.model = projectViewState.getPois(pv.model.project.id)
    }

    function actionTriggered(name) {
        switch (name) {
            case "addsrv":
                popup.setContents("Add server", serverEditComponent,
                        function (serverEdit) {
                            serverEdit.activate(serverEdit.getDefaultModel(), model.environment)
                        },
                        function (serverEdit) {
                            serverEdit.onOk(pv.model.project)
                            // force refresh
                            itemsrepeater.model = projectViewState.getServers(pv.model.project.id, model.environment)
                        })
                break;
            case "addpoi":
                popup.setContents("Add point of interest", poiEditComponent,
                        function (poiEdit) {
                            poiEdit.activate(model, poiEdit.getDefaultModel())
                        },
                        function (poiEdit) {
                            poiEdit.onOk(pv.model.project);
                            // force refresh
                            refreshProjectPois()
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

                    TileProjectPoi {
                        project: pv.model.project
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
                    isServerPoi: false
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
