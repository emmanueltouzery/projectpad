import QtQuick 2.0
import QtQuick.Window 2.0
import "server-menu.js" as ServerMenu

Rectangle {
    property variant server
    property variant rootFlowInParent
    color: "dark gray"
    height: 40
    width: parent.width
    signal shouldRefresh()
    Image {
        x: 5
        source: {
            var envIcon;
            switch (server.environment) {
            case "EnvDevelopment":
                envIcon = 'glyphicons-361-bug';
                break;
            case "EnvUat":
                envIcon = 'glyphicons-534-lab';
                break;
            case "EnvStage":
                envIcon = 'glyphicons-140-adjust-alt';
                break;
            case "EnvProd":
                envIcon = 'glyphicons-333-certificate';
                break;
            }
            return '../glyphicons-free/' + envIcon + '.png'
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
                var desktopSize = {width: Screen.desktopAvailableWidth, height: Screen.desktopAvailableHeight}
                ServerMenu.showSelectMenu(server, parent, desktopSize, shouldRefresh, lineSelectMenu, rootFlowInParent)
                lineSelectMenu.displayedServer = server
            } else {
                lineSelectMenu.visible = false
                serverOptionsGroup.current = null
                lineSelectMenu.displayedServer = null
            }
        }
        height: parent.height
        checkable: true
    }
}
