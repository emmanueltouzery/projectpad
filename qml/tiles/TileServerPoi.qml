import QtQuick 2.0
import ".."
import "../poiactions.js" as PoiActions
import "../utils.js" as Utils

ItemTile {
    property int modelId: model.id
    property variant model
    color: "light gray"
    itemDesc: model.desc
    property variant server
    icon: PoiActions.actions[model.interestType].icon
    property variant global: undefined
    signal activated(variant tile)

    function editPoi(curPoi) {
        popup.setContents("Edit point of interest", editPoiComponent,
                function (poiEdit) {
                    poiEdit.activate(server, curPoi)
                },
                function (poiEdit) {
                    poiEdit.onServerOk(server)
                    // force refresh
                    refreshServerView()
                })
    }

    function tileId() {
        return { type: "TileServerPoi", id: modelId }
    }

    onFocusChanged: {
        if (focus) {
            showMenu(this)
        }
    }

    function showMenu(item) {
        var options = [
            ["glyphicons-151-edit", function() { editPoi(model)}],
            ["glyphicons-512-copy", function() {
                appContext.copyItemEntity("ServerPoiEntityType",
                                          model.id, false)
            }],
            ["glyphicons-193-circle-remove", function() {
                appContext.confirmDelete(function() {
                    Utils.handleEitherVoid(getAppState().
                                           serverViewState.deleteServerPois([model.id]))
                    refreshServerView()
                })
            }]]
        if ((server.accessType === "SrvAccessSsh" || server.accessType === "SrvAccessSshTunnel")
            && server.serverIp.length > 0
            && server.username.length > 0
            && server.password.length > 0) {
            switch (model.interestType) {
            case "PoiCommandToRun":
            case "PoiCommandTerminal":
                options.push(["glyphicons-138-cogwheels", function() {
                    Utils.runIfSshHostTrusted(server, function() {
                        var info = getAppState().
                            serverViewState.executePoiAction(server, model)
                        appContext.progressMessage("\nStarted program\n")
                    })
                }])
                break
            case "PoiLogFile":
                options.push(["glyphicons-283-cardio", function() {
                    Utils.runIfSshHostTrusted(server, function() {
                        getAppState()
                            .serverViewState.executePoiAction(server, model)
                    })
                }])
                options.push(["glyphicons-52-eye-open", function() {
                    Utils.runIfSshHostTrusted(server, function() {
                        getAppState().serverViewState
                            .executePoiSecondaryAction(server, model)
                    })
                }])
                options.push(["glyphicons-182-download-alt", function() {
                    Utils.runIfSshHostTrusted(server, function() {
                        getAppState().
                            serverViewState.executePoiThirdAction(server, model)
                    })
                }])
                break
            case "PoiConfigFile":
                options.push(["glyphicons-52-eye-open", function() {
                    Utils.runIfSshHostTrusted(server, function() {
                        getAppState().
                            serverViewState.executePoiAction(server, model)
                    })
                }])
                options.push(["glyphicons-182-download-alt", function() {
                    Utils.runIfSshHostTrusted(server, function() {
                        getAppState().
                            serverViewState.executePoiThirdAction(server, model)
                    })
                }])
                break
            }
        }
        selectMenu.options = options
        selectMenu.show(item, global)
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            showMenu(parent)
            activated(parent)
        }
    }
    Component {
        id: editPoiComponent
        PoiEdit {
            id: poiEdit
            isServerPoi: true
        }
    }
}
