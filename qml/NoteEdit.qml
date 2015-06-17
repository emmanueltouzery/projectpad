import QtQuick 2.0
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.3

Rectangle {
    id: noteEdit
    color: "light grey"
    property int preferredHeight: 290

    property variant model: getDefaultModel()

    function getDefaultModel() {
        return {}
    }

    function activate(parent, _model) {
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

    Column {
        anchors.fill: parent

        ToolBar {
            RowLayout {
                ToolButton {
                    id: editModeBtn
                    action: editAction
                }
                ToolButton {
                    iconSource: "../glyphicons-free/glyphicons-103-bold.png"
                    onClicked: toggleSnippet("**", "**")
                    visible: editAction.checked
                }
                ToolButton {
                    iconSource: "../glyphicons-free/glyphicons-102-italic.png"
                    onClicked: toggleSnippet("*", "*")
                    visible: editAction.checked
                }
                ToolButton {
                    iconSource: "../glyphicons-free/glyphicons-460-header.png"
                    onClicked: toggleHeader()
                    visible: editAction.checked
                }
                ToolButton {
                    iconSource: "../glyphicons-free/glyphicons-51-link.png"
                    onClicked: toggleSnippet("[", "](url)")
                    visible: editAction.checked
                }
                ToolButton {
                    iconSource: "../glyphicons-free/glyphicons-204-lock.png"
                    onClicked: toggleSnippet("[pass|", "|]")
                    visible: editAction.checked
                }
            }
        }

        TextArea {
            id: textArea
            width: parent.width
            height: parent.height
            text: "test"
            readOnly: !editAction.checked
        }

        Action {
            id: editAction
            checkable: true
            iconSource: "../glyphicons-free/glyphicons-151-edit.png"
            onTriggered: {
                if (editAction.checked) {
                    textArea.text = ""
                    textArea.textFormat = TextEdit.PlainText
                } else {
                    var html = noteTextToHtml(textArea.text)[1]
                    textArea.textFormat = TextEdit.RichText
                    textArea.text = "<html><body>" + html + "</body></html>"
                }
            }
        }
    }
}
