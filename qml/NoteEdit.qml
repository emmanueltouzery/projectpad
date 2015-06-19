import QtQuick 2.0
import QtQuick.Layouts 1.1
import QtQuick.Controls 1.3

Rectangle {
    id: noteEdit
    color: "light grey"
    property int preferredHeight: 490
    property variant appContext: null

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

    GridLayout {
        y: 10
        anchors.left: parent.left
        anchors.right: parent.right
        height: parent.height - 15
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
                    onClicked: togglePassword()
                    visible: editAction.checked
                }
            }
        }

        TextArea {
            id: preview
            Layout.columnSpan: 2
            Layout.fillWidth: true
            Layout.fillHeight: true
            textFormat: TextEdit.RichText
            text: noteTextToHtml(model.contents)[1] // #### should check for errors
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
            onTriggered: {
                if (editAction.checked) {
                    preview.visible = false
                    textArea.visible = true
                } else {
                    preview.visible = true
                    textArea.visible = false
                    var html = noteTextToHtml(textArea.text)[1] // ### should check for errors
                    preview.text = "<html><body>" + html + "</body></html>"
                }
            }
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
