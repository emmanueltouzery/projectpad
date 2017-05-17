import QtQuick 2.0
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.3

import "utils.js" as Utils

GridLayout {
    columns: 2

    function setEditMode(editMode) {
        editAction.checked = editMode
    }

    function getText() {
        return textArea.text
    }

    function toggleSnippet(before, after) {
        var pos = textArea.cursorPosition
        var startPos = pos
        var endPos = pos
        if (textArea.selectedText.length > 0) {
            // has selection
            startPos = textArea.selectionStart
            endPos = textArea.selectionEnd
        }
        if (startPos >= before.length
            && textArea.getText(startPos - before.length, startPos) === before
            && textArea.getText(endPos, endPos + after.length) === after)
        {
            textArea.remove(endPos, endPos+after.length)
            textArea.remove(startPos-before.length, startPos)
            if (startPos != endPos) {
                // restore the selection
                textArea.select(startPos-before.length, endPos-before.length)
            }
        } else {
            textArea.insert(endPos, after)
            textArea.insert(startPos, before)
            textArea.cursorPosition = startPos + before.length
            textArea.forceActiveFocus()
            if (startPos != endPos) {
                // restore the selection
                textArea.select(startPos+before.length, endPos+before.length)
            }
        }
    }

    // Toggle between '#', '##', '###', "-" and no header
    function toggleHeader() {
        var headerCycle = ['#', '##', '###', '-']
        var curPos = textArea.cursorPosition
        var curChar
        while (curChar !== '\n' && curPos !== 0) {
            curPos -= 1
            curChar = textArea.getText(curPos-1, curPos)
        }
        var toInsert = ' # '
        var clear = 0
        for (var i=0;i<headerCycle.length;i++) {
            var header = ' ' + headerCycle[i] + ' '
            if (textArea.getText(curPos, curPos + header.length) === header) {
                toInsert = (i+1 >= headerCycle.length)
                    ? ''
                    : ' ' + headerCycle[i+1] + ' '
                clear = header.length
                break
            }
        }
        if (clear) {
            textArea.remove(curPos, curPos + clear)
        }
        textArea.insert(curPos, toInsert)
    }

    function togglePassword() {
        var passwordSeparators = ['|', '#', '$', '/', '+', '!', '%', '.', ':']
        var i=0
        if (textArea.selectedText.length > 0) {
            // search for a free password separator in case the password
            // contains the separator as a letter.
            while (i < passwordSeparators.length &&
                   textArea.selectedText.indexOf(passwordSeparators[i]) >= 0) {
                ++i
            }
        }
        toggleSnippet("[pass" + passwordSeparators[i],
                      passwordSeparators[i] + "]")
    }

    function togglePreformat() {
        var multiline = (textArea.selectedText.indexOf("\n") >= 0)
        if (multiline) {
            var startPos = textArea.selectionStart
            var movedStart = false
            if (startPos > 0 && textArea.getText(startPos-1, startPos) === "\n") {
                movedStart = true
                --startPos
            }
            var curPos = textArea.selectionEnd
            // first, do we comment or uncomment?
            var isUncomment = textArea.selectedText.indexOf("\n    ") >= 0
            while (curPos >= startPos && isUncomment) {
                var curChar = textArea.getText(curPos, curPos+1)
                if (curChar === "\n") {
                    if (curPos+5>textArea.text.length ||
                        (textArea.getText(curPos, curPos + 5) !== "\n    ")) {
                        isUncomment = false
                    }
                }
                --curPos
            }
            // ok now do it!
            curPos = textArea.selectionEnd
            while (curPos >= startPos) {
                var curChar = textArea.getText(curPos, curPos+1)
                if (curChar === "\n") {
                    if (isUncomment) {
                        textArea.remove(curPos+1, curPos+5)
                    } else {
                        textArea.insert(curPos+1, "    ")
                    }
                }
                --curPos
            }
            if (movedStart && !isUncomment) {
                textArea.select(textArea.selectionStart-4, textArea.selectionEnd)
            }
        } else {
            toggleSnippet("`", "`")
        }
    }

    function getRichText(rawText) {
        return Utils.fromEither(
            "Error: the note text is not properly formatted",
            getAppState().noteTextToHtml(rawText))
    }

    function replaceSelection(newText) {
        textArea.remove(textArea.selectionStart, textArea.selectionEnd)
        textArea.insert(textArea.selectionStart, newText)
    }

    function blockquoteNeedCr(startOffset) {
        var dontNeedCr = startOffset == 0 ||
            textArea.getText(startOffset-1, startOffset) === "\n"
        return !dontNeedCr
    }

    function toggleBlockquote() {
        if (textArea.selectedText.length > 0) {
            var lines = textArea.selectedText.split("\n")
            var newText = null
            if (lines.every(function (l) { return l.indexOf("> ") == 0})) {
                // remove the blockquote
                newText = textArea.selectedText.replace(/\n> /g, "\n")
                if (newText.indexOf("> ") == 0) {
                    newText = newText.substring(2)
                }
            } else {
                // add the blockquote
                var addCr = blockquoteNeedCr(textArea.selectionStart)
                newText = (addCr ? "\n> " : "> ") +
                    textArea.selectedText.replace(/\n/g, "\n> ")
            }
            replaceSelection(newText)
        } else {
            var pos = textArea.cursorPosition
            if (textArea.cursorPosition >= 2 &&
                textArea.getText(pos - 2, pos) === "> ") {
                textArea.remove(pos - 2, pos)
            } else {
                var addCr = blockquoteNeedCr(pos)
                textArea.insert(pos, addCr ? "\n> " : "> ")
            }
        }
    }

    ToolBar {
        Layout.columnSpan: 2
        Layout.fillWidth: true
        Flow {
            spacing: 5
            y: (parent.height - height)/2
            ToolButton {
                id: editModeBtn
                action: editAction
            }
            ToolButton {
                Image {
                    x: 5
                    // as of Qt 5.4, if I use visible to hide,
                    // all the bottoms appear with the bevel
                    // visible which is ugly => rather use the
                    // y coordinate to hide or show.
                    y: editAction.checked ? 3 : 100
                    source: "../glyphicons-free/glyphicons-103-bold.png"
                    fillMode: Image.Pad
                }
                onClicked: toggleSnippet("**", "**")
                // second part of the hack: since I don't use
                // visible to hide, but the y coord, the bevel
                // is still visible on hover with the y=100.
                // => combine with enabled to get rid of that too.
                enabled: editAction.checked
            }
            ToolButton {
                Image {
                    x: 8
                    y: editAction.checked ? 3 : 100
                    source: "../glyphicons-free/glyphicons-102-italic.png"
                    fillMode: Image.Pad
                }
                onClicked: toggleSnippet("*", "*")
                enabled: editAction.checked
            }
            ToolButton {
                Image {
                    x: 4
                    y: editAction.checked ? 4 : 100
                    source: "../glyphicons-free/glyphicons-460-header.png"
                    fillMode: Image.Pad
                }
                onClicked: toggleHeader()
                enabled: editAction.checked
            }
            ToolButton {
                Image {
                    x: 5
                    y: editAction.checked ? 2 : 100
                    source: "../glyphicons-free/glyphicons-51-link.png"
                    fillMode: Image.Pad
                }
                onClicked: toggleSnippet("[", "](url)")
                enabled: editAction.checked
            }
            ToolButton {
                Image {
                    x: 5
                    y: editAction.checked ? 2 : 100
                    source: "../glyphicons-free/glyphicons-204-lock.png"
                    fillMode: Image.Pad
                }
                onClicked: togglePassword()
                enabled: editAction.checked
            }
            ToolButton {
                width: 33
                Image {
                    x: 3
                    y: editAction.checked ? 8 : 100
                    source: "../glyphicons-free/glyphicons-69-ruler.png"
                    fillMode: Image.Pad
                }
                onClicked: togglePreformat()
                enabled: editAction.checked
            }
            ToolButton {
                width: 33
                Image {
                    x: 3
                    y: editAction.checked ? 8 : 100
                    source: "../glyphicons-free/glyphicons-547-quote.png"
                    fillMode: Image.Pad
                }
                onClicked: toggleBlockquote()
                enabled: editAction.checked
            }
        }
    }

    TextArea {
        id: preview
        Layout.columnSpan: 2
        Layout.fillWidth: true
        Layout.fillHeight: true
        textFormat: TextEdit.RichText
        text: getRichText(model.contents)
        readOnly: true
        onLinkActivated: {
            if (link.indexOf("pass://") === 0) {
                var pass = link.substring("pass://".length)
                if (passwordActions.visible && passwordActions.curPass === pass) {
                    closePasswordActions()
                } else {
                    passwordActions.curPass = pass
                    passwordActions.visible = true
                }
            } else {
                Qt.openUrlExternally(link)
            }
        }
    }
    TextArea {
        id: textArea
        Layout.fillWidth: true
        Layout.fillHeight: true
        Layout.columnSpan: 2
        visible: false
        text: model.contents
    }

    Action {
        id: editAction
        checkable: true
        iconSource: "../glyphicons-free/glyphicons-151-edit.png"
        onTriggered: editModeChanged()
    }

    function editModeChanged() {
        if (editAction.checked) {
            preview.visible = false
            textArea.visible = true
        } else {
            preview.visible = true
            textArea.visible = false
            var html = getRichText(textArea.text)
            preview.text = "<html><body>" + html + "</body></html>"
        }
    }

    function closePasswordActions() {
        passwordActions.curPass = ""
        passwordActions.visible = false
    }

    Rectangle {
        id: passwordActions
        color: "light grey"
        width: parent.width
        height: 35
        property string curPass
        z: 2
        visible: false
        y: parent.height - height
        Button {
            id: copyPass
            text: "Copy password"
            x: 10
            anchors.verticalCenter: parent.verticalCenter
            onClicked: {
                appContext.copyItem(passwordActions.curPass, true)
                closePasswordActions()
            }
        }
        Button {
            id: revealPassBtn
            x: copyPass.x + copyPass.width + 5
            anchors.verticalCenter: parent.verticalCenter
            text: "Reveal password"
            onClicked: {
                appContext.successMessage(
                    "The password is: " + passwordActions.curPass)
                closePasswordActions()
            }
        }
        Button {
            x: parent.width - width - 10
            text: "Close"
            anchors.verticalCenter: parent.verticalCenter
            onClicked: closePasswordActions()
        }
    }
}
