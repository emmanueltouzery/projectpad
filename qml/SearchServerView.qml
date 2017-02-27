import QtQuick 2.4
import "utils.js" as Utils

Flow {
    id: serverFlow
    width: parent.width
    spacing: 10
    property bool isFlow: true // needed for keyboard navigation

    function makeTileModel() {
        return function(modelData) {
            return {
                model: modelData.child,
                server: modelData,
                global: rootFlow
            }
        }
    }

    function createRepeaterChildren(serverModel, serverFlow, index, extraUserFlow, websiteFlow,
                                    databaseFlow, poiFlow) {
        var makeTileModels = function (list) {
            return list.map(makeTileModel())
        }
        addTilesToFlow("TileExtraUserAccount",
                       makeTileModels(serverModel[index].extraUsers), extraUserFlow)
        addTilesToFlow("TileServerWebsite",
                       makeTileModels(serverModel[index].websites), websiteFlow)
        addTilesToFlow("TileServerDatabase",
                       makeTileModels(serverModel[index].databases), databaseFlow)
        addTilesToFlow("TileServerPoi",
                       makeTileModels(serverModel[index].pois), poiFlow)
    }

    Rectangle {
        height: index > 0 ? 5 : 0
        width: parent.width
    }
    ServerHeader {
        project: modelData.project
        server: modelData.server
        rootFlowInParent: rootFlow
        hasOptionMenu: !selectorMode
        width: parent.width
        onShouldRefresh: {
            refreshSearch()
        }
    }
    Flow {
        id: extraUserFlow
        width: rootFlow.width
        spacing: 10
    }
    Flow {
        id: websiteFlow
        width: rootFlow.width
        spacing: 10
    }
    Flow {
        id: databaseFlow
        width: rootFlow.width
        spacing: 10
    }
    Flow {
        id: poiFlow
        width: rootFlow.width
        spacing: 10
    }
    Item {
        Component.onCompleted: createRepeaterChildren(
            serverRepeater.model, serverFlow, index,
            extraUserFlow, websiteFlow, databaseFlow, poiFlow)
    }
}
