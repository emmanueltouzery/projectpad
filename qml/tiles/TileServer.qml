import QtQuick 2.4
import QtQuick.Window 2.2
import QtGraphicalEffects 1.0
import ".."
import "../server-menu.js" as ServerMenu

ItemTile {
    property int modelId: modelData.server.id
    color: "light blue"
    itemDesc: modelData.server.desc
    icon: ServerMenu.getServerIcon(modelData.server)
    property variant global: undefined
    signal activated(variant tile)

    Image {
        anchors.fill: parent
        visible: modelData.server.isRetired
        source: "../../pics/retired.png"
        fillMode: Image.PreserveAspectFit
        opacity: 0.25
        transform: Rotation { origin.x: width/2; origin.y: height/2; angle: -25}
    }

    onFocusChanged: {
        if (focus) {
            showMenu(this)
        } else if (typeof lineSelectMenu !== "undefined" && lineSelectMenu.visible) {
            // select menu & lineSelectMenu are exclusive (one or the other)
            selectMenu.visible = false
        }
    }

    function tileId() {
        return { type: "TileServer", id: modelId }
    }

    function projectId() {
        return modelData.server.projectId
    }

    Flow {
        x: 5
        y: 145
        spacing: 2
        opacity: 0.6
        Image {
            height: 16
            fillMode: Image.PreserveAspectFit
            smooth: true
            source: '../../glyphicons-free/glyphicons-526-user-key.png'
            visible: modelData.userCount > 0
        }
        Text {
            text: modelData.userCount
            visible: modelData.userCount > 0
        }
        Image {
            height: 16
            fillMode: Image.PreserveAspectFit
            smooth: true
            source: '../../glyphicons-free/glyphicons-372-global.png'
            visible: modelData.wwwCount > 0
        }
        Text {
            text: modelData.wwwCount
            visible: modelData.wwwCount > 0
        }
        Image {
            height: 16
            fillMode: Image.PreserveAspectFit
            smooth: true
            source: '../../glyphicons-free/glyphicons-528-database.png'
            visible: modelData.dbCount > 0
        }
        Text {
            text: modelData.dbCount
            visible: modelData.dbCount > 0
        }
        Image {
            height: 16
            fillMode: Image.PreserveAspectFit
            smooth: true
            source: '../../glyphicons-free/glyphicons-149-folder-flag.png'
            visible: modelData.poiCount > 0
        }
        Text {
            text: modelData.poiCount
            visible: modelData.poiCount > 0
        }
    }

    function showMenu(item) {
        var desktopSize = {width: Screen.desktopAvailableWidth,
                           height: Screen.desktopAvailableHeight}
        ServerMenu.showSelectMenu(
            pv.model.project, modelData.server, item, desktopSize,
            function() { refreshProjectView() }, selectMenu, global)
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            showMenu(parent)
            activated(parent)
        }
    }
    Component {
        id: serverEditComponent
        ServerEdit {
            id: serverEdit
        }
    }
}
