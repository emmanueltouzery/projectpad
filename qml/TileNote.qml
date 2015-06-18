import QtQuick 2.0
import "utils.js" as Utils

ItemTile {
    property variant global: undefined
    color: "orange"
    icon: "glyphicons-40-notes"
    itemDesc: modelData.title
    property variant project

    MouseArea {
        anchors.fill: parent
        onClicked: {
            selectMenu.options = [
                ["glyphicons-151-edit", function() {
                    popup.setContents(
                        "Edit note", noteEditComponent,
                        function (noteEdit) {
                            noteEdit.activate(project, modelData)
                        },
                        function (noteEdit) {
                            noteEdit.onOk()
                            refreshProjectView()
                        })
                }],
                ["glyphicons-193-circle-remove", function() {
                    appContext.confirmDelete(function() {
                        Utils.handleEither(projectViewState.deleteProjectNotes([modelData.id]))
                        // force refresh
                        refreshProjectView()
                    })
                }]]
            selectMenu.show(parent)
        }
    }
}
