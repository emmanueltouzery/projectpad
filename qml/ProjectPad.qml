import QtQuick 2.7
import QtQuick.Window 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import QtQuick.Dialogs 1.0
import QtQml 2.2
import QtGraphicalEffects 1.0
import "core"
import "keyboard-helpers.js" as KeyboardHelpers

Window {
    width: 800; height: 650;
    title: 'ProjectPAD';
    visible: true;
    id: window

    // keep around info about the last data that was copied to the clipboard.
    // We support the feature to repeat a clipboard copy, so we need that.
    // The info can be entity type+id, or the text that was copied, and we
    // always remember whether that was a password or not, since we handle those
    // differently.
    // In some cases we must store passwords as strings in this variable, which
    // makes us more vulnerable to memory dump attacks. But we're vulnerable
    // anyway...
    property var lastCopyInfo

    property var history: []
    property int historyFromLast: 1

    function loadViewAction(name, model, selectedTile) {
        loadViewActionEx(name, model, false, selectedTile)
    }

    // TODO the history doesn't really behave well
    // when something is deleted... Should be removed
    // from history I guess.
    function loadViewActionEx(name, model, isBackFwd, selectedTile) {
        if (!isBackFwd) {
            // during a search session we overwrite the history
            // each time, to avoid having in the history:
            // [search "", search "t", search "te", ..., search "test"]
            if (history.length > 0 &&
                history[history.length-1][0] === "SearchView.qml" &&
                name === "SearchView.qml") {
                history[history.length-1] = [name, model, null]
            } else {
                if (history.length > 0) {
                    history[history.length-historyFromLast][2] = selectedTile
                }
                history.push([name, model, null])
            }
        }
        if (isBackFwd && name === "SearchView.qml") {
            searchTriggered(true)
            searchField.visible = true
            searchField.text = model.query
        }
        toolbar.setBackActive(history.length > historyFromLast)
        toolbar.setForwardActive(historyFromLast > 1)
        if (name !== "SearchView.qml") {
            // when we change view from the search view,
            // that would mean for instance from search
            // display of a server we open that server,
            // go out of search mode.
            toolbar.disableSearch()
            searchField.visible = false
        }
        loader.setSource(name, {"model": model})
    }

    function confirmDelete(callback) {
        confirmDanger("Sure to delete?", "Are you sure to delete?", "Delete", callback)
    }

    function confirmDanger(title, contents, btnText, callback) {
        popup.setContentsDanger(
            title, confirmComponent, btnText,
            function (deleteDialog) {
                deleteDialog.setContents(contents);
                // if I don't force the focus, the <esc> key doesn't close the dialog
                popup.cancelButton().focus = true
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

    function closePopup() {
        popup.doClose()
    }

    function triggerSearch(txt) {
        history.push(["SearchView.qml", { query: txt }, null])
        if (searchField.text === txt) {
            // just make sure the screen is refreshed.
            refreshSearch()
        } else {
            searchField.text = txt
        }
        searchField.visible = true
        searchField.forceActiveFocus()
    }

    function searchTriggered(isSearchActive) {
        if (isSearchActive) {
            if (searchField.visible) {
                searchField.selectAll()
            } else {
                searchField.text = ""
                searchField.visible = true
            }
            searchField.forceActiveFocus()
        } else {
            loadViewAction("ProjectList.qml", null, null)
        }
    }

    function refreshSearch() {
        // if the current loader view is already the search,
        // just tell him the search text changed.
        // otherwise use loadViewAction() to load the search
        // view and give him the search text.
        loadViewAction(
            "SearchView.qml",
            {
                matches: getAppState().search("AllEntityTypes", searchField.text),
                query: searchField.text,
                isPickingServers: false
            }, null)
    }

    Toolbar {

        Timer {
            id: selectTile
            interval: 50
            repeat: false
            onTriggered: {
                var toGo = history[history.length-historyFromLast]
                var selectedTileInfo = toGo[2]
                if (selectedTileInfo) {
                    var selectedTile = KeyboardHelpers.getAllItems(loader.item.flowToFocus())
                        .filter(function(item) {
                            return item && item.tileId().type === selectedTileInfo.type &&
                                item.tileId().id === selectedTileInfo.id
                        })[0]
                    selectedTile.focus = true
                    selectedTile.activated(selectedTile)
                }
            }
        }

        id: toolbar
        onLoadView: loadViewAction(name, model, selectedTile)
        onActionTriggered: loader.item.actionTriggered(name)
        onToggleMenu: {
            popupMenu.visible = !popupMenu.visible
            toolbar.setMenuDisplayed(popupMenu.visible)
        }
        onSearchTrigger: searchTriggered(isSearchActive)
        onBackAction: {
            if (history.length > historyFromLast) {
                ++historyFromLast
                var toGo = history[history.length-historyFromLast]
                var selectedTileInfo = toGo[2]
                loadViewActionEx(toGo[0], toGo[1], true, selectedTileInfo)
                // i would immediately select the tile, but if I do that,
                // there is some bug and I get flipped X & Y coordinates :-)
                // with a little sleep, all is OK. sleep time of 0 is not enough,
                // 50ms is OK though.
                selectTile.start()
            }
        }
        onForwardAction: {
            if (historyFromLast > 1) {
                --historyFromLast
                var toGo = history[history.length-historyFromLast]
                var selectedTileInfo = toGo[2]
                loadViewActionEx(toGo[0], toGo[1], true, selectedTileInfo)
                // i would immediately select the tile, but if I do that,
                // there is some bug and I get flipped X & Y coordinates :-)
                // with a little sleep, all is OK. sleep time of 0 is not enough,
                // 50ms is OK though.
                selectTile.start()
            }
        }
    }

    TextField {
        y: toolbar.height
        id: searchField
        visible: false
        width: parent.width
        Keys.onPressed: {
            switch (event.key) {
            case Qt.Key_Escape:
                toolbar.disableSearch()
                searchTriggered(false)
                break
            case Qt.Key_Tab:
            case Qt.Key_Down:
                loader.item.flowToFocus().forceActiveFocus()
                break
            }
        }
        Image {
            anchors { top: parent.top; right: parent.right; margins: 7 }
            source: '../glyphicons-free/glyphicons-28-search.png'
            fillMode: Image.PreserveAspectFit
            height: parent.height - 7 * 2
            width: parent.height - 7 * 2
        }
        onTextChanged: refreshSearch()
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
                if (!searchField.visible) {
                    loader.item.flowToFocus().forceActiveFocus()
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

    signal loadView(string name, variant model, var selectedTile)

    Connections {
        target: loader.item
        onLoadView: {
            loadViewAction(name, model, selectedTile)
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
        onClose: {
            if (loader.item) {
                loader.item.flowToFocus().forceActiveFocus()
            }
            if (selectedTile) {
                selectedTile.focus = true
                selectedTile.activated(selectedTile)
            }
        }
    }

    Component.onCompleted: {
        toolbar.setBackActive(false)
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
                loadViewAction(name, model, null)
            }
        }
    }

    Component {
        id: firstPasswordComponent
        FirstPasswordEnter {
            onLoadView: loadViewAction(name, model, selectedTile)
        }
    }
    TextField {
        id: passwordCopy
        visible: false
        readOnly: true
    }
    TextField {
        id: passwordPaste
        visible: false
    }

    function _copyItem(text) {
        passwordCopy.text = text
        passwordCopy.selectAll()
        passwordCopy.copy()
        passwordCopy.text = ""
    }

    function _getClipboardContents() {
        passwordPaste.text = ""
        passwordPaste.paste()
        return passwordPaste.text
    }

    // unless called with dontOverwriteCopyInfo, copyItem
    // will pin the text in memory, because of the "repeat
    // clipboard copy" feature, which is unfortunate because
    // of memory dump attacks. Currently only note passwords
    // are vulnerable.
    function copyItem(text, isPassword, dontOverwriteCopyInfo) {
        _copyItem(text)
        if (!dontOverwriteCopyInfo) {
            lastCopyInfo = { text: text, isPassword: isPassword}
        }
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

    // when copying text repeatedly, i don't want to remember
    // the text that was copied (often it's passwords and then
    // i'm more vulnerable to memory dump attacks), but rather
    // the entity type & item id, then i'll go to fetch the
    // text again on my own if the user asks to copy again.
    function copyItemEntity(entityType, itemId, isPassword) {
        var text = getAppState().projectListState
            .getTextToCopyForEntity(entityType, itemId)
        copyItem(text, isPassword, true)
        lastCopyInfo = {
            entityType: entityType,
            itemId: itemId,
            isPassword: isPassword
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

    function getLastCopiedText() {
        if (lastCopyInfo.entityType) {
            return getAppState().projectListState
                .getTextToCopyForEntity(lastCopyInfo.entityType, lastCopyInfo.itemId)
        } else {
            return lastCopyInfo.text
        }
    }

    Action {
        id: repeatCopyAction
        shortcut: "Ctrl+y"
        onTriggered: {
            if (!lastCopyInfo) {
                return
            }
            if (lastCopyInfo.entityType) {
                copyItemEntity(lastCopyInfo.entityType,
                                   lastCopyInfo.itemId, lastCopyInfo.isPassword)
            } else {
                copyItem(lastCopyInfo.text, lastCopyInfo.isPassword)
            }
        }
    }

    Action {
        id: openHelp
        shortcut: "?"
        onTriggered: {
            getAppState().openAssociatedFile(getDataPath("help/index.html"))
        }
    }

    PopupMenu {
        anchors.top: toolbar.bottom
        anchors.right: parent.right
        id: popupMenu
        z: 1
        menuItems: [
            ["About...", function() {
                popup.setContentsNoCancel(
                    "About", aboutDialogComponent,
                    function (aboutDlg) { },
                    function (aboutDlg) { })
            }],
            ["Repeat clipboard copy (ctrl-y)", function() {
                repeatCopyAction.trigger()
            }],
            ["Help", function() {
                openHelp.trigger()
            }]
        ]
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
                // only overwrite the clipboard if the current text in
                // the clipboard is what we copied previously.
                // Don't touch it if the clipboard was overwritten since then!
                if (_getClipboardContents() === getLastCopiedText()) {
                    _copyItem(".") // doesn't work with ""...
                }
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
            height: childrenRect.height
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

    Component {
        id: aboutDialogComponent
        Rectangle {
            height: childrenRect.height
            Text {
                padding: 10
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
