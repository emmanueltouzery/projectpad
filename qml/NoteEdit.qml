import QtQuick 2.0
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.3

Rectangle {
    id: noteEdit
    color: "light grey"
    property int preferredHeight: 290

    property variant model: getDefaultModel()

    function getDefaultModel() {
        return {"title":"New note", "contents":""}
    }

    function activate(parent, _model) {
        model = _model
        var groups = projectViewState.getProjectGroupNames(parent.id)
        group.model.clear()
        groups.forEach(function (grp) {
            group.model.append({"text": grp})
        })
        group.currentIndex = groups.indexOf(_model.groupName)
        title.selectAll()
        title.forceActiveFocus()
    }

    function onOk(project) {
        if (model.id) {
            model = projectViewState.updateProjectNote(
                model, title.text, textArea.text, group.editText);
        } else {
            projectViewState.addProjectNote(
                project.id, title.text, textArea.text, group.editText)
        }
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

    GridLayout {
        y: 10
        anchors.left: parent.left
        anchors.right: parent.right
        anchors.margins: 10
        columns: 2

        Text {
            text: "Title:"
        }
        TextField {
            id: title
            Layout.fillWidth: true
            text: model.title
        }

        Text {
            text: "Group:"
        }
        ComboBox {
            id: group
            Layout.fillWidth: true
            textRole: "text"
            model: ListModel {}
            editable: true
        }

        ToolBar {
            Layout.columnSpan: 2
            Layout.fillWidth: true
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
            Layout.columnSpan: 2
            Layout.fillWidth: true
            width: parent.width
            height: parent.height
            text: model.contents
            readOnly: !editAction.checked
            onLinkActivated: Qt.openUrlExternally(link)
        }

        Action {
            id: editAction
            checkable: true
            iconSource: "../glyphicons-free/glyphicons-151-edit.png"
            onTriggered: {
                if (editAction.checked) {
                    textArea.textFormat = TextEdit.PlainText
                    textArea.text = ""
                } else {
                    var html = noteTextToHtml(textArea.text)[1]
                    textArea.textFormat = TextEdit.RichText
                    textArea.text = "<html><body>" + html + "</body></html>"
                }
            }
        }
    }
}
