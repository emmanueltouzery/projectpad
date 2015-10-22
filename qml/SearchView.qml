import QtQuick 2.4
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "core"
import "tiles"

import "utils.js" as Utils

Rectangle {
    id: searchView
    anchors.fill: parent
    signal loadView(string name, variant model)
    property variant model
    property variant appContext: null

    function getBreadCrumbs() {
        return {pathLinks: [], title: 'Search'};
    }

    function refreshSearch() {
        loadView("SearchView.qml", {
            matches: getAppState().search(searchView.model.query),
            query: searchView.model.query})
    }
    function refreshServerView() { refreshSearch() }
    function refreshProjectView() { refreshSearch() }
    function refreshProjectPois() { refreshSearch() }

    function addTilesToFlow(tileName, items, flow) {
        var tile = Qt.createComponent("tiles/" + tileName + ".qml")
        for (var i=0;i<items.length;i++) {
            var modelData = items[i]
            tile.createObject(flow, {
                model: modelData.child,
                server: modelData,
                global: rootFlow})
        }
    }

    function createRepeaterChildren(serverModel, serverFlow, index, extraUserFlow, websiteFlow,
                                    databaseFlow, poiFlow) {
        addTilesToFlow("TileExtraUserAccount",
                       serverModel[index].extraUsers, extraUserFlow)
        addTilesToFlow("TileServerWebsite",
                       serverModel[index].websites, websiteFlow)
        addTilesToFlow("TileServerDatabase",
                       serverModel[index].databases, databaseFlow)
        addTilesToFlow("TileServerPoi",
                       serverModel[index].pois, poiFlow)
    }

    ScrollView {
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
                            }
                        }
                        Repeater {
                            model: modelData.pois
                            TileProjectPoi {
                                global: rootFlow
                                model: modelData.child
                                project: modelData.parent
                            }
                        }
                        Repeater {
                            id: serverRepeater
                            model: modelData.servers
                            Flow {
                                id: serverFlow
                                width: rootFlow.width
                                spacing: 10
                                Rectangle {
                                    height: index > 0 ? 5 : 0
                                    width: searchView.width
                                }
                                ServerHeader {
                                    project: modelData.project
                                    server: modelData.server
                                    rootFlowInParent: rootFlow
                                    onShouldRefresh: {
                                        refreshSearch()
                                    }
                                }
                                Flow {
                                    id: extraUserFlow
                                    spacing: 10
                                }
                                Flow {
                                    id: websiteFlow
                                    spacing: 10
                                }
                                Flow {
                                    id: databaseFlow
                                    spacing: 10
                                }
                                Flow {
                                    id: poiFlow
                                    spacing: 10
                                }
                                Item {
                                    Component.onCompleted: createRepeaterChildren(
                                        serverRepeater.model, serverFlow, index,
                                        extraUserFlow, websiteFlow, databaseFlow, poiFlow)
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
            LineSelectMenu {
                id: lineSelectMenu
                visible: false
                z: 3
            }
        }
    }
    Component {
        id: serverEditComponent
        ServerEdit {
            id: serverEdit
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
