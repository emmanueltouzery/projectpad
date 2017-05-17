import QtQuick 2.0
import "../utils.js" as Utils

ItemTile {
    property variant global: undefined
    property variant model
    color: "lightsteelblue"
    icon: "glyphicons-40-notes"
    itemDesc: model.title
    property variant server
    signal activated(variant tile)

    onFocusChanged: {
        if (focus) {
            showMenu(this)
        } else if (typeof lineSelectMenu !== "undefined" && lineSelectMenu.visible) {
            // select menu & lineSelectMenu are exclusive (one or the other)
            selectMenu.visible = false
        }
    }

    function tileId() {
        return { type: "TileServerNote", id: model.id }
    }

    function showMenu(item) {
        selectMenu.options = [
            ["glyphicons-151-edit", function() {
                popup.setContents(
                    "Edit note", editServerNoteComponent,
                    function (serverNoteEdit) {
                        serverNoteEdit.activate(server, model)
                    },
                    function (serverNoteEdit) {
                        serverNoteEdit.onOk()
                        refreshServerView()
                    })
            }],
            ["glyphicons-193-circle-remove", function() {
                appContext.confirmDelete(function() {
                    Utils.handleEitherVoid(
                        getAppState().serverViewState.deleteServerNotes([model.id]))
                    // force refresh
                    refreshServerView()
                })
            }]]
        selectMenu.show(item, global)
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            showMenu(parent)
            activated(parent)
        }
    }
}
