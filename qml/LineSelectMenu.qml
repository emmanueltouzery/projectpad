import QtQuick 2.0

Row {
    id: lineSelectMenuRoot
    height: 40
    spacing: 8
    property variant options;
    property variant displayedServer;

    function show(parnt, global) {
        if (global !== undefined) {
            var globalCoords = parnt.mapToItem(global, 0, 0)
            lineSelectMenuRoot.x = globalCoords.x + parnt.width - 60 - options.length*32
            lineSelectMenuRoot.y = globalCoords.y + 13
        } else {
            lineSelectMenuRoot.y = parnt.y
            lineSelectMenuRoot.x = parnt.x
        }
        lineSelectMenuRoot.visible = true
    }

    Repeater {
        model: lineSelectMenuRoot.options
        Image {
            source: '../glyphicons-free/' + modelData[0] + '.png'
            MouseArea {
                anchors.fill: parent
                onClicked: options[index][1]()
            }
        }
    }
}
