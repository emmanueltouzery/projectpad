import QtQuick 2.0
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

    property variant actions: [["add", "glyphicons-191-circle-plus", "Add..."]]

    function getBreadCrumbs() {
        var projectModel = Utils.findById(
            getAppState().projectListState.projects, model.projectId)
        return {pathLinks:
            [
                {screen: "ProjectView.qml",
                model: {"project": projectModel, "environment": model.environment},
                display: projectModel.name + " " + PoiActions.envDesc(model.environment)}
            ],
            title: model.desc}
    }

    function refreshServerView() {
        serverSectionRepeater.model = getAppState().serverViewState
            .getServerDisplaySections(pv.model.id)
    }

    function flowToFocus() {
        return flow
    }

    function actionTriggered(name) {
        // using noOpacity in these calls to avoid triggering the
        // shade animation a second time when the user goes to the
        // second step of the wizard which is aesthetically displeasing.
        switch (name) {
            case "addpoi":
                popup.setContents("Add point of interest", editPoiComponent,
                        function (poiEdit) {
                            poiEdit.activate(pv.model, poiEdit.getDefaultModel())
                        },
                        function (poiEdit) {
                            poiEdit.onServerOk(pv.model);
                            refreshServerView()
                        }, {noOpacity: true})
                break;
            case "addwww":
                popup.setContents("Add website", editSrvWwwComponent,
                        function (wwwEdit) {
                            wwwEdit.activate(pv.model, wwwEdit.getDefaultModel())
                        },
                        function (wwwEdit) {
                            wwwEdit.onOk(pv.model)
                            refreshServerView()
                        }, {noOpacity: true})
                break;
            case "adddb":
                popup.setContents("Add database", editDatabaseComponent,
                        function (dbEdit) {
                            dbEdit.activate(pv.model, dbEdit.getDefaultModel())
                        },
                        function (dbEdit) {
                            dbEdit.onOk(pv.model);
                            refreshServerView()
                        }, {noOpacity: true})
                break;
            case "addaccount":
                popup.setContents("Add extra user account", editExtraUserAccountComponent,
                        function (editUser) {
                            editUser.activate(pv.model, editUser.getDefaultModel())
                        },
                        function (editUser) {
                            editUser.onOk(pv.model);
                            refreshServerView()
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
        id: serverScrollView
        anchors.fill: parent
        Flickable {
            id: serverFlickable
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

                ServerHeader {
                    project: Utils.findById(
                        getAppState().projectListState.projects, model.projectId)
                    server: pv.model
                    rootFlowInParent: flow
                    iconType: 'server'
                    onShouldRefresh: {
                        var allServers = getAppState().projectViewState
                            .getServers(pv.model.projectId, pv.model.environment).map(
                                             function(se) { return se.server })
                        var updatedServer = Utils.findById(allServers, pv.model.id)
                        if (updatedServer === null) {
                            // the server was deleted! Go to the parent project.
                            var projectModel = Utils.findById(
                                getAppState().projectListState.projects, model.projectId)
                            loadView("ProjectView.qml", {"project": projectModel, "environment": pv.model.environment}, null)
                        } else {
                            loadView("ServerView.qml", updatedServer, null)
                        }
                    }
                }

                Repeater {
                    id: serverSectionRepeater
                    width: parent.width
                    model: getAppState().serverViewState
                        .getServerDisplaySections(pv.model.id)

                    Flow {
                        width: serverSectionRepeater.width
                        anchors.margins: 4
                        spacing: 10

                        Text {
                            width: parent.width
                            text: modelData.groupName || ""
                            visible: modelData.groupName !== null
                        }

                        Repeater {
                            id: useraccountsrepeater
                            model: modelData.extraUsers

                            TileExtraUserAccount {
                                model: modelData
                                server: pv.model
                                global: parent.parent
                                onActivated: {
                                    Utils.scrollInView(
                                        tile, serverScrollView, serverFlickable)
                                    tile.focus = true
                                }
                            }
                        }

                        Repeater {
                            id: wwwsrepeater
                            model: modelData.websites

                            TileServerWebsite {
                                model: modelData
                                server: pv.model
                                global: parent.parent
                                onActivated: {
                                    Utils.scrollInView(
                                       tile, serverScrollView, serverFlickable)
                                    tile.focus = true
                                }
                            }
                        }

                        Repeater {
                            id: dbsrepeater
                            model: modelData.databases

                            TileServerDatabase {
                                model: modelData
                                server: pv.model
                                global: parent.parent
                                onActivated: {
                                    Utils.scrollInView(
                                        tile, serverScrollView, serverFlickable)
                                    tile.focus = true
                                }
                            }
                        }

                        Repeater {
                            id: poisrepeater
                            model: modelData.pois

                            TileServerPoi {
                                server: pv.model
                                model: modelData
                                global: parent.parent
                                onActivated: {
                                    Utils.scrollInView(
                                        tile, serverScrollView, serverFlickable)
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
                id: editPoiComponent
                PoiEdit {
                    id: poiEdit
                    isServerPoi: true
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
            ExclusiveGroup {id: serverOptionsGroup}
            LineSelectMenu {
                id: lineSelectMenu
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
    }
}
