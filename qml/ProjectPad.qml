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

	Toolbar {
		id: toolbar
		onLoadView: loadViewAction(name, model)
		onActionTriggered: loader.item.actionTriggered(name)
		onEditModeChanged: loader.item.editMode = toolbar.editMode
	}

	Loader {
		width: parent.width
		y: toolbar.height
		height: {
			var baseHeight = parent.height-toolbar.height
			if (toolbar.editMode) {
				return baseHeight-editModeActionBar.height
			} else {
				return baseHeight
			}
		}
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

	EditModeActionBar {
		id: editModeActionBar
		y: loader.y + loader.height
		visible: toolbar.editMode
		onModeActionBarAction: loader.item.actionTriggered(type)
		onActionExecuted: toolbar.editMode = false
	}

	signal loadView(string name, variant model)

	Connections {
		target: loader.item
		onLoadView: {
			loadViewAction(name, model)
		}
		onSelectionChange: editModeActionBar.selectionCount = selection.length
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
		toast.msgText = txt
		toastOpacity.running = true
	}

	Rectangle {
		id: toast
		opacity: 0
		color: "#c9302c"
		z: 2
		anchors.horizontalCenter: parent.horizontalCenter
		width: 450
		height: 80
		property string msgText: ""
		radius: 10
		Text {
			id: errorText
			text: parent.msgText
			x: 15
			width: parent.width-15
			height: parent.height
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
}
