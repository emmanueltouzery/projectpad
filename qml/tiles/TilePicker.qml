import QtQuick 2.4

Rectangle {
    width: 140; height: 70
    property variant model
    property bool selected: true
    color: "light slate gray"
    signal ticked(variant self, bool onOrOff)
    signal activated(variant title)

    Text {
        text: model.desc
        anchors.top: parent.top
        anchors.left: parent.left
        anchors.right: parent.right
        height: 45
        wrapMode: Text.Wrap
        anchors.margins: 5
    }

    Rectangle {
        id: shadedRect
        anchors.bottom: parent.bottom
        anchors.left: parent.left
        anchors.right: parent.right
        height: 25
        color: "#20000000"
    }

    Image {
        anchors.top: shadedRect.top
        anchors.bottom: parent.bottom
        anchors.right: parent.right
        anchors.margins: 2
        fillMode: Image.PreserveAspectFit
        mipmap: true
        visible: parent.selected
        source: '../../glyphicons-free/glyphicons-153-check.png'
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            parent.selected = !parent.selected
            parent.ticked(parent, parent.selected)
            activated(parent)
        }
    }
}
