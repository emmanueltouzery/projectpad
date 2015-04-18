import QtQuick 2.0
import QtQuick.Window 2.2
import "server-menu.js" as ServerMenu

ItemTile {
    property int modelId: modelData.server.id
    property bool selected: false
    color: "light blue"
    border.width: selected ? 4 : 0
    border.color: "green"
    itemDesc: modelData.server.desc
    icon: {
        if (modelData.server.accessType === "SrvAccessRdp") {
            return "../pics/windows_logo"
        } else {
            return "glyphicons-464-server"
        }
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
            source: '../glyphicons-free/glyphicons-526-user-key.png'
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
            source: '../glyphicons-free/glyphicons-372-global.png'
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
            source: '../glyphicons-free/glyphicons-528-database.png'
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
            source: '../glyphicons-free/glyphicons-149-folder-flag.png'
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
            var desktopSize = {width: Screen.desktopAvailableWidth, height: Screen.desktopAvailableHeight}
            ServerMenu.showSelectMenu(modelData.server, parent, desktopSize, function() {
                itemsrepeater.model = projectViewState.getServers(pv.model.project.id, pv.model.environment)
            }, selectMenu)
        }
    }
    Component {
        id: serverEditComponent
        ServerEdit {
            id: serverEdit
        }
    }
}
