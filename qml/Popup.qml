import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Rectangle {
	id: popupHost
	anchors.fill: parent
	color: "black"
	z: 1
	property var curCallback
	property bool implicitClose: true
	Keys.onReturnPressed: curCallback()
	Keys.onEnterPressed: curCallback()
	Keys.onEscapePressed: {
		if (cancelButton.visible) {
			doClose()
		}
	}

    function setOkButtonText(txt) {
        okButton.text = txt
    }

    function disconnectOk() {
				okButton.clicked.disconnect(curCallback)
    }

	function setContents(title, contents, initCallback, okCallback) {
		implicitClose = true
      replaceContents(title, contents, initCallback, okCallback)
		cancelButton.visible = true
		shadeOpacity.start()
		popup.visible = true
	}

	function wizardReplaceContents(title, contents, initCallback, okCallback) {
      disconnectOk()
      replaceContents(title, contents, initCallback, function(x) {
          implicitClose = true
          okCallback(x)
      })
  }

    // maybe make a second button and fuck it.
    // but if i want three level wizards?
	function replaceContents(title, contents, initCallback, okCallback) {
		okButton.text = "OK"
		okButton.style = defaultButtonStyle
		popupTitle.text = title
		popupContentsLoader.sourceComponent = contents
		initCallback(popupContentsLoader.item)
		var f = function() {
			okCallback(popupContentsLoader.item)
			if (implicitClose) {
          disconnectOk()
				popup.visible = false
			}
		}
		okButton.clicked.connect(f)
		curCallback = f
	}

	function setContentsDelete(title, contents, initCallback, okCallback) {
		implicitClose = true
		cancelButton.visible = true
		setContents(title, contents, initCallback, okCallback);
		okButton.text = "Delete"
		okButton.style = dangerButtonStyle
	}

	function setContentsNoCancel(title, contents, initCallback, okCallback) {
		setContents(title, contents, initCallback, okCallback);
		cancelButton.visible = false
	}

	function doClose() {
		okButton.clicked.disconnect(curCallback)
		popup.visible = false
	}

	Rectangle {
		id: popupWindow
		anchors.horizontalCenter: parent.horizontalCenter
		y: 40
		width: 580
		color: Qt.lighter("light gray", 1.15)
		height: popupHeader.height + popupContentsLoader.height
		z: 2
		radius: 5

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
		}

		Loader {
			id: popupContentsLoader
			width: parent.width
			y: popupHeader.height
			  onLoaded: {
            height = item.preferredHeight
            if (popupContentsLoader.item.popupContext !== undefined) {
                popupContentsLoader.item.popupContext = popupHost
            }
        }
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
}
