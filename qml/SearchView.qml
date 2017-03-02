import QtQuick 2.4
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "core"
import "tiles"
import "utils.js" as Utils
import "keyboard-helpers.js" as KeyboardHelpers

Rectangle {
    id: searchView
    signal loadView(string name, variant model, var selectedTile)
    property variant model
    property variant appContext: null
    property bool selectorMode: false
    property bool isPickingServers: false
    property variant allTiles: []
    property variant serverEditComponent

    function flowToFocus() {
        return rootFlow
    }

    // shared with SearchServerView
    function addTilesToFlow(tileName, items, flow) {
        var tile = Qt.createComponent("tiles/" + tileOrSelector(tileName) + ".qml")
        for (var i=0;i<items.length;i++) {
            var modelData = items[i]
            var obj = tile.createObject(flow, modelData)
            allTiles.push(obj)
            if (obj.ticked) {
                // will go here only on picker mode.
                obj.ticked.connect(function(tilePicker, onOrOff) {
                    tileTicked(tilePicker)
                })
            }
            obj.activated.connect(function(tile_) {
                Utils.scrollInView(tile_, scrollView, flickable)
                tile_.focus = true
            })
        }
    }

    function tileOrSelector(tileName) {
        if (selectorMode) {
            return "TilePicker"
        } else {
            return tileName
        }
    }

    function tileTicked(tile) {
        for (var i=0;i<allTiles.length;i++) {
            var curTile = allTiles[i]
            if (curTile.model.id !== tile.model.id) {
                curTile.selected = false
            }
        }
    }

    Component.onCompleted: {
        serverEditComponent = selectorMode
            ? null
            : Qt.createComponent("ServerEdit.qml")
    }

    function getBreadCrumbs() {
        return {pathLinks: [], title: 'Search'};
    }

    onModelChanged: { allTiles = [] }

    function refreshSearch() {
        loadView("SearchView.qml", {
            matches: getAppState().search("AllEntityTypes", searchView.model.query),
            query: searchView.model.query}, null)
    }
    function refreshServerView() { refreshSearch() }
    function refreshProjectView() { refreshSearch() }
    function refreshProjectPois() { refreshSearch() }

    // only makes sense in selector mode.
    function getSelectedItem() {
        var selectedItems = allTiles.filter(function(item) { return item.selected });
        if (selectedItems.length > 1) {
            console.error("Internal error: several selected items in selector!!")
            return null
        }
        if (selectedItems.length == 1) {
            return selectedItems[0].model
        }
        return null
    }

    // only makes sense in selector mode.
    function setSelectedItem(itemId) {
        for (var i=0;i<allTiles.length;i++) {
            var curTile = allTiles[i]
            curTile.selected = curTile.model.id === itemId
        }
    }

    ScrollView {
        id: scrollView
        anchors.fill: parent

        Flickable {
            id: flickable
            anchors.fill: parent
            pixelAligned: true
            contentHeight: rootFlow.implicitHeight
            ExclusiveGroup {id: serverOptionsGroup}
            Flow {
                anchors.fill: parent
                anchors.margins: 4
                spacing: 10
                id: rootFlow

                Keys.onPressed: {
                    KeyboardHelpers.handleKey(event, rootFlow, selectMenu)
                }

                Repeater {
                    model: searchView.model.matches
                    Flow {
                        id: projectFlow
                        width: searchView.width
                        spacing: 10
                        Rectangle {
                            color: "gray"
                            width: searchView.width
                            height: 40
                            Flow {
                                height: parent.height - 10
                                y: 5
                                x: 5
                                spacing: 5
                                Image {
                                    height: parent.height
                                    mipmap: true
                                    fillMode: Image.PreserveAspectFit
                                    source: {
                                        if (modelData.project.hasCustomIcon) {
                                            return Utils.projectGetCustomIcon(modelData.project)
                                        } else {
                                            return "../glyphicons-free/glyphicons-441-folder-closed.png"
                                        }
                                    }
                                }
                                Text {
                                    text: modelData.project.name
                                    height: parent.height
                                    verticalAlignment: Text.AlignVCenter
                                }
                            }
                        }
                        Repeater {
                            model: modelData.notes
                            TileNote {
                                global: rootFlow
                                model: modelData.child
                                project: modelData.parent
                                onActivated: {
                                    Utils.scrollInView(tile, scrollView, flickable)
                                    tile.focus = true
                                }
                            }
                        }
                        Repeater {
                            model: modelData.pois
                            TileProjectPoi {
                                global: rootFlow
                                model: modelData.child
                                project: modelData.parent
                                onActivated: {
                                    Utils.scrollInView(tile, scrollView, flickable)
                                    tile.focus = true
                                }
                            }
                        }
                        Repeater {
                            id: serverRepeater
                            model: modelData.servers
                            SearchServerView {
                                visible: !isPickingServers
                                width: rootFlow.width
                            }
                        }
                        Repeater {
                            id: srvLinkRepeater
                            model: modelData.srvLinks
                            TileServerLink {
                                global: rootFlow
                                project: modelData.parent
                                model: modelData.child
                                onActivated: {
                                    Utils.scrollInView(tile, scrollView, flickable)
                                    tile.focus = true
                                }
                            }
                        }
                        Flow {
                            id: serversPickingFlow
                            width: parent.width
                            spacing: 10
                            visible: isPickingServers
                            Component.onCompleted: {
                                if (!isPickingServers) {
                                    return
                                }
                                var makeSrvTileModel = function (srv) {
                                    return { model: srv.server }
                                }
                                addTilesToFlow(
                                    null,
                                    modelData.servers.map(makeSrvTileModel),
                                    serversPickingFlow)
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
        id: noteEditComponent
        NoteEdit {
            id: noteEdit
            appContext: searchView.appContext
        }
    }
}
