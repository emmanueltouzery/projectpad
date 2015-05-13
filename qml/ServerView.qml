import QtQuick 2.0
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

    function refreshPois() {
        poisrepeater.model = serverViewState.getPois(pv.model.id)
    }

    function refreshWwws() {
        wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
    }

    function refreshDbs() {
        dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
    }

    function refreshUsers() {
        useraccountsrepeater.model = serverViewState.getServerExtraUserAccounts(pv.model.id)
    }

    function actionTriggered(name) {
        switch (name) {
            case "addpoi":
                popup.setContents("Add point of interest", editPoiComponent,
                        function (poiEdit) {
                            poiEdit.activate(pv.model, poiEdit.getDefaultModel())
                        },
                        function (poiEdit) {
                            poiEdit.onServerOk();
                            refreshPois()
                        }, {noOpacity: true})
                break;
            case "addwww":
                popup.setContents("Add website", editSrvWwwComponent,
                        function (wwwEdit) {
                            wwwEdit.activate(pv.model, wwwEdit.getDefaultModel())
                        },
                        function (wwwEdit) {
                            wwwEdit.onOk();
                            refreshWwws()
                        }, {noOpacity: true})
                break;
            case "adddb":
                popup.setContents("Add database", editDatabaseComponent,
                        function (dbEdit) {
                            dbEdit.activate(pv.model, dbEdit.getDefaultModel())
                        },
                        function (dbEdit) {
                            dbEdit.onOk();
                            refreshDbs()
                        }, {noOpacity: true})
                break;
            case "addaccount":
                popup.setContents("Add extra user account", editExtraUserAccountComponent,
                        function (editUser) {
                            editUser.activate(pv.model, editUser.getDefaultModel())
                        },
                        function (editUser) {
                            editUser.onOk();
                            refreshUsers()
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

                ServerHeader {
                    server: pv.model
                    rootFlowInParent: flow
                    onShouldRefresh: {
                        var allServers = Utils.map(projectViewState.getServers(pv.model.projectId, pv.model.environment),
                                             function(se) { return se.server })
                        var updatedServer = Utils.findById(allServers, pv.model.id)
                        if (updatedServer === null) {
                            // the server was deleted! Go to the parent project.
                            var projectModel = Utils.findById(projectListState.projects, model.projectId)
                            loadView("ProjectView.qml", {"project": projectModel, "environment": pv.model.environment})
                        } else {
                            loadView("ServerView.qml", updatedServer)
                        }
                    }
                }

                Repeater {
                    id: useraccountsrepeater
                    model: serverViewState.getServerExtraUserAccounts(pv.model.id)

                    TileExtraUserAccount {
                        model: modelData
                        server: pv.model
                    }
                }

                Repeater {
                    id: wwwsrepeater
                    model: serverViewState.getServerWebsites(pv.model.id)

                    TileServerWebsite {
                        model: modelData
                        server: pv.model
                    }
                }

                Repeater {
                    id: dbsrepeater
                    model: serverViewState.getServerDatabases(pv.model.id)

                    TileServerDatabase {
                        model: modelData
                        server: pv.model
                    }
                }

                Repeater {
                    id: poisrepeater
                    model: serverViewState.getPois(pv.model.id)

                    TileServerPoi {
                        server: pv.model
                        model: modelData
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
