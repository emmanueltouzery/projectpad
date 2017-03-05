import QtQuick 2.0
import ".."
import "../utils.js" as Utils

ItemTile {
    id: tileServerWebsite
    property int modelId: model.id
    color: "light slate gray"
    itemDesc: model.desc
    property variant model
    property variant server
    icon: "glyphicons-372-global"
    property variant global: undefined
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
        return { type: "TileServerWebsite", id: modelId }
    }

    function editSrvWww(curPoi) {
        popup.setContents("Edit website", editSrvWwwComponent,
                function (wwwEdit) {
                    wwwEdit.activate(server, curPoi, appContext)
                },
                function (wwwEdit) {
                    wwwEdit.onOk(server)
                    // force refresh
                    refreshServerView()
                })
    }

    function showMenu(item) {
        selectMenu.options = [
            ["glyphicons-151-edit", function() { editSrvWww(model)}],
            ["glyphicons-372-global", function() { getAppState().openAssociatedFile(model.url)}],
            ["glyphicons-512-copy", function() {
                appContext.copyItemEntity("ServerWebsiteEntityType",
                                          model.id, true)
            }],
            ["glyphicons-193-circle-remove", function() {
                appContext.confirmDelete(function() {
                    Utils.handleEitherVoid(getAppState().serverViewState
                                           .deleteServerWebsites([model.id]))
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
    Component {
        id: editSrvWwwComponent
        ServerWebsiteEdit {
            id: wwwEdit
        }
    }
}
