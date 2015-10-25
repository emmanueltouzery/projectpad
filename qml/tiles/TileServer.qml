import QtQuick 2.0
import QtQuick.Window 2.2
import ".."
import "../server-menu.js" as ServerMenu

ItemTile {
    property int modelId: modelData.server.id
    color: "light blue"
    itemDesc: modelData.server.desc
    icon: ServerMenu.getServerIcon(modelData.server)
    property variant global: undefined

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

    MouseArea {
        anchors.fill: parent
        onClicked: {
            var desktopSize = {width: Screen.desktopAvailableWidth,
                               height: Screen.desktopAvailableHeight}
            ServerMenu.showSelectMenu(
                pv.model.project, modelData.server, parent, desktopSize,
                function() { refreshProjectView() }, selectMenu, global)
        }
    }
    Component {
        id: serverEditComponent
        ServerEdit {
            id: serverEdit
        }
    }
}