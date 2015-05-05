import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

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
        loadView("SearchView.qml", {matches: search(searchView.model.query), query: searchView.model.query })
    }
    function refreshPois() { refreshSearch() }
    function refreshWwws() { refreshSearch() }
    function refreshDbs() { refreshSearch() }
    function refreshUsers() { refreshSearch() }
    function refreshProjectPois() { refreshSearch() }

    ScrollView {
        anchors.fill: parent
        Flickable {
            id: flickable
            anchors.fill: parent
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
                            Text {
                                x: 5
                                text: modelData.project.name
                                height: parent.height
                                verticalAlignment: Text.AlignVCenter
                            }
                        }
                        Repeater {
                            model: modelData.pois
                            TileProjectPoi {
                                global: rootFlow
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
                                    server: modelData.server
                                    rootFlowInParent: rootFlow
                                    onShouldRefresh: {
                                        refreshSearch()
                                    }
                                }
                                Repeater {
                                    model: modelData.extraUsers
                                    TileExtraUserAccount {
                                        model: modelData.child
                                        global: rootFlow
                                    }
                                }
                                Repeater {
                                    model: modelData.websites
                                    TileServerWebsite {
                                        model: modelData.child
                                        global: rootFlow
                                    }
                                }
                                Repeater {
                                    model: modelData.databases
                                    TileServerDatabase {
                                        model: modelData.child
                                        global: rootFlow
                                    }
                                }
                                Repeater {
                                    model: modelData.pois
                                    TileServerPoi {
                                        model: modelData.child
                                        server: modelData.server
                                        global: rootFlow
                                    }
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
}
