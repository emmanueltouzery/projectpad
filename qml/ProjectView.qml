import QtQuick 2.0
import QtQuick.Window 2.2
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import "core"
import "tiles"
import "utils.js" as Utils
import "poiactions.js" as PoiActions
import "keyboard-helpers.js" as KeyboardHelpers

Rectangle {
    id: pv
    anchors.fill: parent
    signal loadView(string name, variant model, var selectedTile, var selectedTileAfter)
    property variant model
    property variant appContext: null
    property string _popupToDisplay

    property bool editMode

    property variant actions: [["add", "glyphicons-191-circle-plus", "Add..."]]
    property variant environments: getEnvs()

    function getEnvs() {
        var result = [model.environment];
        if (pv.model.project.hasDev && model.environment !== "EnvDevelopment") {
            result.push("EnvDevelopment")
        }
        if (pv.model.project.hasUat && model.environment !== "EnvUat") {
            result.push("EnvUat")
        }
        if (pv.model.project.hasStage && model.environment !== "EnvStage") {
            result.push("EnvStage")
        }
        if (pv.model.project.hasProd && model.environment !== "EnvProd") {
            result.push("EnvProd")
        }
        console.error("getEnvs returning " + result + " -- " + model.environment)
        return result;
    }

    function getBreadCrumbs() {
        return {pathLinks: [], title: model.project.name};
    }

    function flowToFocus() {
        return flow
    }

    function refreshProjectView() {
        projectSectionRepeater.model = getAppState().projectViewState.getProjectDisplaySections(
                model.project.id, model.environment)
    }

    function actionTriggered(name) {
        // using noOpacity in these calls to avoid triggering the
        // shade animation a second time when the user goes to the
        // second step of the wizard which is aesthetically displeasing.
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
                }, {noOpacity: true})
            break;
        case "addsrvlink":
            popup.setContents(
                "Add link to server", serverLinkEditComponent,
                function (serverLinkEdit) {
                    serverLinkEdit.activate(
                        pv.model.project, serverLinkEdit.getDefaultModel(),
                        model.environment)
                },
                function (serverLinkEdit) {
                    serverLinkEdit.onOk(pv.model.project)
                    refreshProjectView()
                }, {noOpacity: true});
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
                }, {noOpacity: true})
            break;
        case "addnote":
            popup.setContents(
                "Add note", projectNoteEditComponent,
                function (projectNoteEdit) {
                    popup.implicitClose = false
                    projectNoteEdit.activate(pv.model.project, projectNoteEdit.getDefaultModel())
                },
                function (projectNoteEdit) {
                    projectNoteEdit.onOk(pv.model.project);
                    refreshProjectView()
                }, {noOpacity: true})
            break;
        case "add":
            popup.implicitClose = false
            popup.setContents("Add...", addProjectComponent,
                              function (prjAdd) {
                                  prjAdd.init()
                              },
                              function (prjAdd) {
                                  _popupToDisplay = prjAdd.next()
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
        id: projectScrollView
        anchors.fill: parent

        Flickable {
            id: projectFlickable
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

                Keys.onPressed: {
                    KeyboardHelpers.handleKey(event, flow, selectMenu)
                }

                Repeater {
                    id: projectSectionRepeater
                    width: parent.width
                    model: getAppState().projectViewState.getProjectDisplaySections(
                        pv.model.project.id, pv.model.environment)

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
                            TileProjectNote {
                                model: modelData
                                project: pv.model.project
                                global: parent.parent
                                onActivated: {
                                    Utils.scrollInView(
                                        tile, projectScrollView, projectFlickable)
                                    tile.focus = true
                                }
                            }
                        }

                        Repeater {
                            id: serversrepeater
                            model: modelData.servers
                            TileServer {
                                global: parent.parent
                                onActivated: {
                                    Utils.scrollInView(
                                        tile, projectScrollView, projectFlickable)
                                    tile.focus = true
                                }
                            }
                        }

                        Repeater {
                            id: serverlinksrepeater
                            model: modelData.linkedServers
                            TileServerLink {
                                model: modelData
                                project: pv.model.project
                                global: parent.parent
                                onActivated: {
                                    Utils.scrollInView(
                                        tile, projectScrollView, projectFlickable)
                                    tile.focus = true
                                }
                            }
                        }

                        Repeater {
                            id: poisrepeater
                            model: modelData.pois

                            TileProjectPoi {
                                model: modelData
                                project: pv.model.project
                                global: parent.parent
                                onActivated: {
                                    Utils.scrollInView(
                                        tile, projectScrollView, projectFlickable)
                                    tile.focus = true
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
                id: serverLinkEditComponent
                ServerLinkEdit {
                    id: serverLinkEdit
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
                id: projectNoteEditComponent
                ProjectNoteEdit {
                    id: projectNoteEdit
                    appContext: pv.appContext
                }
            }
            Component {
                id: addProjectComponent
                ProjectAddPopup {
                    id: prjAddPopup
                }
            }
        }
    }
}
