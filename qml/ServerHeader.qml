import QtQuick 2.0
import QtQuick.Window 2.0
import "core"
import "server-menu.js" as ServerMenu

Rectangle {
    property variant project
    property variant server
    property variant rootFlowInParent
    property variant iconType: 'environment'
    property bool hasOptionMenu: true
    color: "dark gray"
    height: 40
    width: parent.width
    signal shouldRefresh()

    // hide the line select menu on resize because
    // it's pixel-anchored and won't follow the
    // rest of the layout on resize.
    onWidthChanged: hideLineSelectMenu()
    function hideLineSelectMenu() {
        lineSelectMenu.visible = false
        serverOptionsGroup.current = null
        lineSelectMenu.displayedServer = null
    }

    // we're not a tile, but we often load views from server headers,
    // and someone will ask us our tileId() to manipulate history.
    // just return an empty one... hackish but...
    function tileId() {
        return null
    }

    Image {
        x: 5
        source: {
            if (iconType === 'environment') {
                var iconName;
                switch (server.environment) {
                case "EnvDevelopment":
                    iconName = 'glyphicons-361-bug';
                    break;
                case "EnvUat":
                    iconName = 'glyphicons-534-lab';
                    break;
                case "EnvStage":
                    iconName = 'glyphicons-140-adjust-alt';
                    break;
                case "EnvProd":
                    iconName = 'glyphicons-333-certificate';
                    break;
                }
            } else {
                iconName = ServerMenu.getServerIcon(server)
            }
            return '../glyphicons-free/' + iconName + '.png'
        }
        verticalAlignment: Image.AlignVCenter
        fillMode: Image.Pad
        height: parent.height
    }
    Text {
        x: 35
        text: server.desc
        height: parent.height
        verticalAlignment: Text.AlignVCenter
    }
    IconButton {
        width: 30
        x: parent.width - 35
        iconX: 12
        iconName: 'glyphicons-518-option-vertical'
        iconSize: 20
        exclusiveGroup: serverOptionsGroup
        onClicked: {
            if (lineSelectMenu.displayedServer !== server) {
                var desktopSize = {width : Screen.desktopAvailableWidth,
                                   height: Screen.desktopAvailableHeight}
                ServerMenu.showSelectMenu(
                    project, server, parent, desktopSize, shouldRefresh,
                    lineSelectMenu, rootFlowInParent)
                lineSelectMenu.displayedServer = server
                lineSelectMenu.hideFunction = hideLineSelectMenu
            } else {
                hideLineSelectMenu()
            }
        }
        height: parent.height
        checkable: true
        visible: hasOptionMenu
    }
}
