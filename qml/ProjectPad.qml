import QtQuick 2.0
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import QtQml 2.2
import QtGraphicalEffects 1.0
import "core"

Window {
    width: 800; height: 650;
    title: 'ProjectPAD';
    visible: true;
    id: window

    function loadViewAction(name, model) {
        if (name !== "SearchView.qml") {
            // when we change view from the search view,
            // that would mean for instance from search
            // display of a server we open that server,
            // go out of search mode.
            toolbar.disableSearch()
        }
        loader.setSource(name, {"model": model})
    }

    function confirmDelete(callback) {
        confirmDanger("Sure to delete?", "Are you sure to delete?", "Delete", callback)
    }

    function confirmDanger(title, contents, btnText, callback) {
        popup.setContentsDanger(title, confirmComponent, btnText,
                                function (deleteDialog) {
                                    deleteDialog.setContents(contents)
                                },
                                function (deleteDialog) { callback() })
    }

    function searchFieldHeight() {
        if (searchField.visible) {
            return searchField.height
        }
        return 0
    }

    function finishPasswordCopy() {
        toast.opacity = 0.0
        passwordCopyTimer.running = false
        toastButton.clicked.disconnect(finishPasswordCopy)
        toastButton.visible = false
    }

    Toolbar {
        id: toolbar
        onLoadView: loadViewAction(name, model)
        onActionTriggered: loader.item.actionTriggered(name)
        onToggleMenu: {
            popupMenu.visible = !popupMenu.visible
            toolbar.setMenuDisplayed(popupMenu.visible)
        }
        onToggleSearch: {
            searchField.text = ""
            searchField.visible = !searchField.visible
            if (!searchField.visible) {
                loadView("ProjectList.qml", null)
            } else {
                searchField.forceActiveFocus()
            }
        }
    }

    TextField {
        y: toolbar.height
        id: searchField
        visible: false
        width: parent.width
        Image {
            anchors { top: parent.top; right: parent.right; margins: 7 }
            source: '../glyphicons-free/glyphicons-28-search.png'
            fillMode: Image.PreserveAspectFit
            height: parent.height - 7 * 2
            width: parent.height - 7 * 2
        }
        onTextChanged: {
            // if the current loader view is already the search,
            // just tell him the search text changed.
            // otherwise use loadViewAction() to load the search
            // view and give him the search text.
            loadViewAction(
                "SearchView.qml",
                {
                    matches: getAppState().search("AllEntityTypes", searchField.text),
                    query: searchField.text
                })
        }
    }

    // slight drop shadow from the toolbar for a 3d effect
    // and a nice separation between toolbar & contents.
    LinearGradient {
        y: toolbar.height + searchFieldHeight()
        z: 1
        width: parent.width
        height: 5
        start: Qt.point(0, 0)
        end: Qt.point(0, height)
        gradient: Gradient {
            GradientStop { position: 0.0; color: "#77000000" }
            GradientStop { position: 1.0; color: "#00000000" }
        }
    }

    SplitView {
        width: parent.width
        y: toolbar.height + searchFieldHeight()
        height: parent.height-toolbar.height-searchFieldHeight()
        orientation: Qt.Vertical

        Loader {
            id: loader
            Layout.fillWidth: true
            Layout.fillHeight: true
            height: parent.height - outputText.height
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

        TextArea {
            id: outputText
            height: 150
            Layout.fillWidth: true
            readOnly: true

            MouseArea {
                anchors.fill: parent
                acceptedButtons: Qt.RightButton
                onClicked: outputMenu.popup()
            }
        }
    }

    Menu {
        id: outputMenu
        MenuItem {
            text: "Copy"
            onTriggered: {
                if (outputText.selectedText.length === 0) {
                    outputText.selectAll()
                }
                outputText.copy()
            }
        }
        MenuItem {
            text: "Clear"
            onTriggered: outputText.text = ""
        }
    }

    signal loadView(string name, variant model)

    Connections {
        target: loader.item
        onLoadView: {
            loadViewAction(name, model)
        }
    }

    function processRunCommandSignal(result) {
        var text
        if (result[0] === "succeeded") {
            text = "Execution terminated!"
            successMessage(text)
            text = "\n" + text
        } else if (result[0] === "failed") {
            text = "Execution failed!\n" + result[1]
            errorMessage(text)
            text = "\n" + text
        } else {
            text =  result[1]
        }
        progressMessage(text)
    }

    Connections {
        id: projectViewStateSignalConn
        target: null
        onGotOutput: processRunCommandSignal(output)
    }

    Connections {
        id: serverViewStateSignalConn
        target: null
        onGotOutput: processRunCommandSignal(output)
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
            onLoadView: {
                progressMessage("Welcome!\n")
                serverViewStateSignalConn.target = getAppState().serverViewState
                projectViewStateSignalConn.target = getAppState().projectViewState
                loadViewAction(name, model)
            }
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

    function _copyItem(text) {
        passwordCopy.text = text
        passwordCopy.selectAll()
        passwordCopy.copy()
        passwordCopy.text = ""
    }

    function copyItem(text, isPassword) {
        _copyItem(text)
        if (isPassword) {
            passwordCopyTimer.secondsLeft = 15
            toast.opacity = 1.0
            toastButton.text = "Don't clear"
            toastButton.visible = true
            toastButton.clicked.connect(finishPasswordCopy)
            passwordCopyTimer.start()
        } else {
            successMessage("Text copied to the clipboard")
        }
    }

    function errorMessage(txt) {
        toast.color = "#c9302c"
        toast.msgText = txt
        toastButton.visible = false
        toastOpacity.running = true
    }

    function progressMessage(txt) {
        var atEnd = true // outputText.cursorPosition == outputText.text.length
        outputText.text += txt
        if (atEnd) {
            outputText.cursorPosition = outputText.text.length
        }
    }

    function successMessage(txt) {
        toast.color = "green"
        toast.msgText = txt
        toastButton.visible = false
        toastOpacity.running = true
    }

    PopupMenu {
        anchors.top: toolbar.bottom
        anchors.right: parent.right
        id: popupMenu
        z: 1
        menuItems: [["About...", function() {
            popup.setContentsNoCancel("About", aboutDialogComponent,
                function (aboutDlg) { },
                function (aboutDlg) { })
        }]]
        visible: false
        onVisibleChanged: {
            toolbar.setMenuDisplayed(popupMenu.visible)
        }
    }

    Rectangle {
        id: toast
        opacity: 0
        y: 40
        z: 2
        anchors.horizontalCenter: parent.horizontalCenter
        width: 450
        height: Math.max(toastButton.height, errorText.height)
        property string msgText: ""
        radius: 4
        Text {
            id: errorText
            text: parent.msgText
            //height: parent.height // <-- if I put this, it gets broken for multiline/wrapped toasts
            x: 15
            y: {
                // is the text is smaller in height than the toast button?
                var offset = toastButton.height - errorText.height
                if (offset > 0) {
                    // YES => center the text vertically with the button's center.
                    return offset/2
                } else {
                    // NO => position the text from the top of the toast.
                    return 0
                }
            }
            width: toastButton.x - x
            verticalAlignment: Text.AlignVCenter
            wrapMode: Text.Wrap
        }
        MouseArea {
            enabled: toast.opacity != 0.0
            anchors.fill: parent
            onClicked: {
                toastOpacity.running = false
                toast.opacity = 0.0
                toastButton.visible = false
            }
        }
        Button {
            // the toast button causes problems
            // when it's visible but the toast is
            // hidden => it takes focus from other
            // things, preventing clicks elsewhere
            // where it's positioned.
            visible: toast.opacity !== 0
            id: toastButton
            x: parent.width - width
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

    Timer {
        id: passwordCopyTimer
        property int secondsLeft
        interval: 1000
        repeat: true
        triggeredOnStart: true
        onTriggered: {
            toast.color = "green"
            toast.msgText = "Copied password, clipboard will be cleared in " + secondsLeft + "s."
            if (secondsLeft-- <= 0) {
                _copyItem(".") // doesn't work with ""...
                finishPasswordCopy()
            }
        }
    }

    Component {
        id: confirmComponent

        Rectangle {
            function setContents(contents) {
                confirmContentsText.text = contents
            }
            color: "dark grey"
            height: 60
            property int preferredHeight: 60
            Text {
                id: confirmContentsText
                x: 15
                width: parent.width-15
                height: parent.height
                text: "Are you sure to delete?"
                verticalAlignment: Text.AlignVCenter
            }
        }
    }

    // the file dialog is reachable from search view, project, server...
    // put it here so it works from everywhere.
    FileDialog {
        id: saveExtraUserAuthKeyDialog
        title: "Please choose a destination"
        property variant extraUser
        visible: false
        selectFolder: true
        onAccepted: {
            getAppState().serverViewState.saveAuthKey(
                fileUrls[0] + "/" + extraUser.authKeyFilename, extraUser)
            successMessage(
                "Saved file to " + fileUrls[0] + "/" + extraUser.authKeyFilename)
        }
    }

    // the file dialog is reachable from search view, project, server...
    // put it here so it works from everywhere.
    FileDialog {
        id: saveServerAuthKeyDialog
        title: "Please choose a destination"
        property variant server
        visible: false
        selectFolder: true
        onAccepted: {
            getAppState().projectViewState.saveAuthKey(
                fileUrls[0] + "/" + server.authKeyFilename, server)
            successMessage(
                "Saved file to " + fileUrls[0] + "/" + server.authKeyFilename)
        }
    }

    Component {
        id: aboutDialogComponent
        Rectangle {
            property int preferredHeight: 180
            Text {
                anchors.fill: parent
                anchors.margins: 10
                text: "<html><body><h2>ProjectPad</h2> License: BSD3 (except for the icons).<br/>"
                    + "<a href='https://github.com/emmanueltouzery/projectpad'>Website</a><br/>"
                    + "<ul><li>Uses <a href='http://sqlcipher.net/'>SQLcipher</a>"
                    + " (<a href='https://www.zetetic.net/sqlcipher/license/'>license</a>)"
                    + " for secure encrypted data storage.</li>"
                    + "<li>Uses <a href='http://glyphicons.com/'>Glyphicons</a> FREE"
                    + " (<a href='http://glyphicons.com/license/'>license</a>) icons.</li>"
                    + "</ul></body></html>"
                onLinkActivated: Qt.openUrlExternally(link)
            }
        }
    }
}
