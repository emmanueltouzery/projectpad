import QtQuick 2.0
import ".."
import "../utils.js" as Utils

ItemTile {
    id: tileServerDatabase
    property int modelId: model.id
    property variant model
    property variant server
    width: 180; height: 180
    color: "gray"
    itemDesc: model.desc
    icon: "glyphicons-528-database"
    property variant global: undefined
    signal activated(variant tile)

    function editDb(curDb) {
        popup.setContents("Edit database", editDatabaseComponent,
                function (dbEdit) {
                    dbEdit.activate(server, curDb)
                },
                function (dbEdit) {
                    dbEdit.onOk(server)
                    // force refresh
                    refreshServerView()
                })
    }

    onFocusChanged: {
        if (focus) {
            showMenu(this)
        }
    }

    function showMenu(item) {
        selectMenu.options = [
            ["glyphicons-151-edit", function() { editDb(model)}],
            ["glyphicons-512-copy", function() {
                appContext.copyItemEntity("DatabaseEntityType", model.id, true)
            }],
            ["glyphicons-193-circle-remove", function() {
                appContext.confirmDelete(function() {
                    var msg = getAppState().serverViewState.canDeleteServerDatabase(model)
                    if (msg !== null) {
                        appContext.errorMessage(msg)
                        return
                    }
                    Utils.handleEitherVoid(getAppState()
                                           .serverViewState.deleteServerDatabases([model.id]))
                    refreshServerView()
                })
            }]]
        selectMenu.show(item, tileServerDatabase.global)
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            showMenu(parent)
            activated(parent)
        }
    }
    Component {
        id: editDatabaseComponent
        ServerDatabaseEdit {
            id: dbEdit
        }
    }
}
