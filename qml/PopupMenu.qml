import QtQuick 2.0
import QtQuick.Controls 1.3
import "utils.js" as Utils

Column {
    id: popupMenu
    width: 150
    property int cellHeight: 30
    property variant menuItems: []
    Canvas {
        id: canvas
        width: 150
        height: 10

        property variant fontSpec: "14px sans-serif"
        property int arrowStartOffsetFromEnd: 25
        property int arrowEndOffsetFromEnd: 5
        property int topOffset: 10

        onPaint: {
            var ctx = canvas.getContext("2d")
            ctx.fillStyle = "light gray"
            ctx.clearRect(0, 0, canvas.width, canvas.height);
            ctx.beginPath()
            ctx.moveTo(0, topOffset)
            ctx.lineTo(canvas.width - arrowStartOffsetFromEnd, topOffset)
            ctx.lineTo(canvas.width - arrowStartOffsetFromEnd
                             + (arrowStartOffsetFromEnd-arrowEndOffsetFromEnd)/2, 0)
            ctx.lineTo(canvas.width - arrowEndOffsetFromEnd, topOffset)
            ctx.lineTo(canvas.width, topOffset)
            ctx.fill()
            ctx.stroke()
        }
    }
    Rectangle {
        id: menuRect
        width: parent.width
        height: cellHeight*menuItems.length
        color: "black"
        Rectangle {
            anchors.fill: parent
            anchors.leftMargin: 1
            anchors.rightMargin: 1
            anchors.bottomMargin: 1
            color: "light gray"
            Column {
                id: menuColumn
                anchors.fill: parent
                height: menuRect.height
                anchors.leftMargin: 5
                Repeater {
                    model: menuItems
                    Label {
                        height: cellHeight
                        width: parent.width
                        text: modelData[0]
                        verticalAlignment: Text.AlignVCenter
                        MouseArea {
                            anchors.fill: parent
                            onClicked: {
                                popupMenu.visible = false
                                // apparently when I set the menuItems as the model, the items
                                // get copied and mangled and the functions are lost
                                // => find back the original item in the original list.
                                var item = Utils.filter(menuItems, function(item) { return item[0] === modelData[0]})[0];
                                item[1]()
                            }
                        }
                    }
                }
            }
        }
    }
}
