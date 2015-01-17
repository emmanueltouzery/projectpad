import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1

Window {
	width: 800; height: 600;
	title: 'ProjectPAD';
	visible: true;
	id: window

	function loadViewAction(name, model) {
		loader.setSource(name, {"model": model})
	}

	function confirmDelete(callback) {
		popup.setContentsDelete("Sure to delete?", confirmDeleteComponent,
			function (deleteDialog) { },
			function (deleteDialog) { callback() })
	}

	Toolbar {
		id: toolbar
		onLoadView: loadViewAction(name, model)
		onActionTriggered: loader.item.actionTriggered(name)
		onToggleMenu: {
			popupMenu.visible = !popupMenu.visible
			toolbar.setMenuDisplayed(popupMenu.visible)
		}
	}

	Loader {
		width: parent.width
		y: toolbar.height
		height: parent.height-toolbar.height
		id: loader
		onLoaded: {
			// putting it here ensures it's called
			// also for the first screen of the app
			// which is not displayed as the result
			// of a click.
			toolbar.actions = loader.item.actions
			var breadcrumbsInfo = loader.item.getBreadCrumbs()
			toolbar.pathLinks = breadcrumbsInfo.pathLinks
			toolbar.title = breadcrumbsInfo.title
			if (loader.item.appContext !== undefined) {
				loader.item.appContext = window
			}
		}
	}

	signal loadView(string name, variant model)

	Connections {
		target: loader.item
		onLoadView: {
			loadViewAction(name, model)
		}
	}

	Popup {
		id: popup
		visible: false
	}

	Component.onCompleted: {
		var popupComponent;
		if (isDbInitialized()) {
			popupComponent = enterPasswordComponent
		} else {
			popupComponent = firstPasswordComponent
		}
		popup.setContentsNoCancel("Welcome", popupComponent,
			function (passwdDialog) {
				popup.implicitClose = false
				passwdDialog.activate()
			},
			function (passwdDialog) {
				passwdDialog.onOk(passwdDialog, popup)
			})
	}

	Component {
		id: enterPasswordComponent
		PasswordEnter {
			onLoadView: loadViewAction(name, model)
		}
	}

	Component {
		id: firstPasswordComponent
		FirstPasswordEnter {
			onLoadView: loadViewAction(name, model)
		}
	}
	TextField {
		id: passwordCopy
		visible: false
		readOnly: true
	}

	function copyItem(text) {
		passwordCopy.text = text
		passwordCopy.selectAll()
		passwordCopy.copy()
		passwordCopy.text = ""
	}

	function errorMessage(txt) {
		toast.color = "#c9302c"
		toast.msgText = txt
		toastOpacity.running = true
	}

	function successMessage(txt) {
		toast.color = "green"
		toast.msgText = txt
		toastOpacity.running = true
	}

	PopupMenu {
		anchors.top: toolbar.bottom
		anchors.right: parent.right
		id: popupMenu
		z: 1
		menuItems: [["About...", function() {console.log("about")}]]
		visible: false
	}

	Rectangle {
		id: toast
		opacity: 0
		z: 2
		anchors.horizontalCenter: parent.horizontalCenter
		width: 450
		height: errorText.height
		property string msgText: ""
		radius: 10
		Text {
			id: errorText
			text: parent.msgText
			x: 15
			width: parent.width-15
			verticalAlignment: Text.AlignVCenter
			wrapMode: Text.WordWrap
		}
	}
	NumberAnimation {
		id: toastOpacity
		property: "opacity"
		easing.type: Easing.InExpo
		duration: 5000
		from: 1.0
		to: 0.0
		target: toast
	}

	Component {
		id: confirmDeleteComponent
		Rectangle {
			color: "dark grey"
			height: 60
			property int preferredHeight: 60
			Text {
				id: deleteText
				x: 15
				width: parent.width-15
				height: parent.height
				text: "Are you sure to delete?"
				verticalAlignment: Text.AlignVCenter
			}
		}
	}
}
