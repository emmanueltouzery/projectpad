import QtQuick 2.0

Rectangle {
    width: 180; height: 180
    property string itemDesc
    property string icon

    Text {
        text: itemDesc
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        wrapMode: Text.Wrap
        anchors.margins: 5
    }

    Rectangle {
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        height: 45
        color: "#20000000"
    }

    Image {
        anchors.bottom: parent.bottom
        anchors.right: parent.right
        anchors.margins: 10
        source: {
            if (icon.length > 0 && icon.indexOf("/") === 0) {
                return icon
            } else {
                return '../glyphicons-free/' + icon + '.png'
            }
        }
    }
}
