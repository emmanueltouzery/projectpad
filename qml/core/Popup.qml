import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "../buttonstyles"

Rectangle {
    id: popupHost
    anchors.fill: parent
    color: "black"
    z: 1
    property var curCallback
    property bool implicitClose: true
    property int embedLevel: 0
    width: window.width

    function escPressedCallback() {
        if (cancelButton.visible) {
            doClose()
        }
    }

    function initCallbacks() {
        Keys.returnPressed.connect(curCallback)
        Keys.enterPressed.connect(curCallback)
        Keys.escapePressed.connect(escPressedCallback)
    }
    function dropCallbacks() {
        Keys.returnPressed.disconnect(curCallback)
        Keys.enterPressed.disconnect(curCallback)
        Keys.escapePressed.disconnect(escPressedCallback)
    }

    function setContents(title, contents, initCallback, okCallback, options) {
        implicitClose = true
        okButton.text = (options && options.okBtnText) || "OK"
        okButton.style = defaultButtonStyle
        popupTitle.text = title
        popupContentsLoader.sourceComponent = contents
        initCallback(popupContentsLoader.item)
        var f = function() {
            okCallback(popupContentsLoader.item)
            if (implicitClose) {
                okButton.clicked.disconnect(curCallback)
                popupHost.visible = false
                if (embedLevel > 0) {
                    // remove the shade on the header of the parent popup.
                    popup.unshadeHeader()
                    // restore the focus on the parent popup so the ESC key can work.
                    popup.forceActiveFocus()
                }
            }
        }
        okButton.clicked.connect(f)
        curCallback = f
        initCallbacks()
        cancelButton.visible = true
        var noOpacity = options && options.noOpacity
        if (!noOpacity) {
            shadeOpacity.start()
        }
        popupHost.visible = true
    }

    function setContentsDanger(title, contents, btnText, initCallback, okCallback) {
        implicitClose = true
        cancelButton.visible = true
        setContents(title, contents, initCallback, okCallback);
        okButton.text = btnText
        okButton.style = dangerButtonStyle
    }

    function setContentsNoCancel(title, contents, initCallback, okCallback) {
        setContents(title, contents, initCallback, okCallback);
        cancelButton.visible = false
    }

    function doClose() {
        okButton.clicked.disconnect(curCallback)
        popupHost.visible = false
        dropCallbacks()
        if (embedLevel > 0) {
            // remove the shade on the header of the parent popup.
            popup.unshadeHeader()
            // restore the focus on the parent popup so the ESC key can work.
            popup.forceActiveFocus()
        }
    }

    function shadeHeader() {
        headerShadeOpacity.start()
        headerShadeMouseArea.visible = headerShade.visible = true
    }

    function unshadeHeader() {
        headerShadeMouseArea.visible = headerShade.visible = false
    }

    Rectangle {
        id: popupWindow
        anchors.horizontalCenter: parent.horizontalCenter
        y: 40
        width: popupContentsLoader.item.widthResize
            ? window.width - 220
            : 580
        color: Qt.lighter("light gray", 1.15)
        height: popupHeader.height
        z: 2
        radius: 5

        // this rectangle is only to cover the
        // lower rounded corners of the header.
        Rectangle {
            width: parent.width
            color: parent.color
            y: 10
            z: -1
            height: parent.height - 10
        }

        Rectangle {
            id: popupHeader
            width: parent.width
            color: Qt.lighter("light gray", 1.1)
            height: 40
            radius: 5

            Button {
                id: cancelButton
                text: "Cancel"
                x: 5
                style: NormalButtonStyle {}
                anchors.verticalCenter: parent.verticalCenter
                onClicked: doClose()
            }

            Text {
                id: popupTitle
                text: "Title"
                anchors.horizontalCenter: parent.horizontalCenter
                anchors.verticalCenter: parent.verticalCenter
                font.bold: true
            }

            Button {
                id: okButton
                text: "OK"
                anchors.right: parent.right
                anchors.rightMargin: 5
                style: defaultButtonStyle
                anchors.verticalCenter: parent.verticalCenter
            }

            Rectangle {
                id: headerShade
                anchors.fill: parent
                color: "#aa000000"
                visible: false
            }
            /* this mouse area catches the clicks outside of the popup,
             * preventing the user from clicking in the greyed out areas. */
            MouseArea {
                id: headerShadeMouseArea
                anchors.fill: parent
                visible: false
            }
        }

        Loader {
            id: popupContentsLoader
            width: parent.width
            y: popupHeader.height
            height: item.preferredHeight < 0
               ? window.height - mapToItem(popupHost, 0, 0).y - 60 - embedLevel*60
               : item.preferredHeight
        }
    }

    /* this mouse area catches the clicks outside of the popup,
     * preventing the user from clicking in the greyed out areas. */
    MouseArea {
        anchors.fill: parent
    }

    Component {
        id: defaultButtonStyle
        DefaultButtonStyle {}
    }

    Component {
        id: dangerButtonStyle
        DangerButtonStyle {}
    }

    // TODO I think this can be made more compact using state transitions, behaviours etc.
    ColorAnimation {
        id: shadeOpacity
        property: "color"
        easing.type: Easing.Linear
        duration: 500
        from: "#00000000"
        to: "#aa000000"
        target: popupHost
    }
    ColorAnimation {
        id: headerShadeOpacity
        property: "color"
        easing.type: Easing.Linear
        duration: 500
        from: "#00000000"
        to: "#aa000000"
        target: headerShade
    }
}
