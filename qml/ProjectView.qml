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
        ["addpoi", "glyphicons-336-pushpin", "Add point of interest"],
        ["addnote", "glyphicons-336-pushpin", "Add note"]]

    function getBreadCrumbs() {
        return {pathLinks: [], title: model.project.name + " " + PoiActions.envDesc(model.environment)};
    }

    function refreshProjectView() {
        projectSectionRepeater.model = projectViewState.getProjectDisplaySections(
                model.project.id, model.environment)
    }

    function actionTriggered(name) {
        switch (name) {
        case "addsrv":
            popup.setContents(
                "Add server", serverEditComponent,
                function (serverEdit) {
                    serverEdit.activate(
                        pv.model.project, serverEdit.getDefaultModel(),
                        model.environment)
                },
                function (serverEdit) {
                    serverEdit.onOk(pv.model.project)
                    refreshProjectView()
                })
            break;
        case "addpoi":
            popup.setContents(
                "Add point of interest", poiEditComponent,
                function (poiEdit) {
                    poiEdit.activate(pv.model.project, poiEdit.getDefaultModel())
                },
                function (poiEdit) {
                    poiEdit.onOk(pv.model.project);
                    refreshProjectView()
                })
            break;
        case "addnote":
            popup.setContents(
                "Add note", noteEditComponent,
                function (noteEdit) {
                    noteEdit.activate(pv.model.project, noteEdit.getDefaultModel())
                },
                function (noteEdit) {
                    noteEdit.onOk(pv.model.project);
                    refreshProjectView()
                })
            break;
        }
    }

    ScrollView {
        anchors.fill: parent
        Flickable {
            anchors.fill: parent
            contentHeight: flow.implicitHeight
            pixelAligned: true
            Flow {
                anchors.fill: parent
                anchors.topMargin: 7
                anchors.leftMargin: 4
                anchors.rightMargin: 4
                anchors.bottomMargin: 4
                spacing: 10
                id: flow

                Repeater {
                    id: projectSectionRepeater
                    width: parent.width
                    model: projectViewState.getProjectDisplaySections(pv.model.project.id, pv.model.environment)

                    Flow {
                        width: projectSectionRepeater.width
                        anchors.margins: 4
                        spacing: 10

                        Text {
                            width: parent.width
                            text: modelData.groupName || ""
                            visible: modelData.groupName !== null
                        }

                        Repeater {
                            id: notesrepeater
                            model: modelData.notes
                            TileNote {
                                project: pv.model.project
                                global: parent.parent
                            }
                        }

                        Repeater {
                            id: serversrepeater
                            model: modelData.servers
                            TileServer {
                                global: parent.parent
                            }
                        }

                        Repeater {
                            id: poisrepeater
                            model: modelData.pois

                            TileProjectPoi {
                                project: pv.model.project
                                global: parent.parent
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
                    isServerPoi: false
                }
            }
            Component {
                id: noteEditComponent
                NoteEdit {
                    id: noteEdit
                    appContext: pv.appContext
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
