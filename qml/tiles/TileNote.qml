import QtQuick 2.0
import "../utils.js" as Utils

ItemTile {
    property variant global: undefined
    property variant model
    color: "lightsteelblue"
    icon: "glyphicons-40-notes"
    itemDesc: model.title
    property variant project
    signal activated(variant tile)

    onFocusChanged: {
        if (focus) {
            showMenu(this)
        }
    }

    function tileId() {
        return { type: "TileNode", id: model.id }
    }

    function showMenu(item) {
        selectMenu.options = [
            ["glyphicons-151-edit", function() {
                popup.setContents(
                    "Edit note", noteEditComponent,
                    function (noteEdit) {
                        noteEdit.activate(project, model)
                    },
                    function (noteEdit) {
                        noteEdit.onOk()
                        refreshProjectView()
                    })
            }],
            ["glyphicons-193-circle-remove", function() {
                appContext.confirmDelete(function() {
                    Utils.handleEitherVoid(
                        getAppState().projectViewState.deleteProjectNotes([model.id]))
                    // force refresh
                    refreshProjectView()
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
