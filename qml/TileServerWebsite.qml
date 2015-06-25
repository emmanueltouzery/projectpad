import QtQuick 2.0
import "utils.js" as Utils

ItemTile {
    id: tileServerWebsite
    property int modelId: model.id
    color: "light slate gray"
    itemDesc: model.desc
    property variant model
    property variant server
    icon: "glyphicons-372-global"
    property variant global: undefined

    function editSrvWww(curPoi) {
        popup.setContents("Edit website", editSrvWwwComponent,
                function (wwwEdit) {
                    wwwEdit.activate(server, curPoi)
                },
                function (wwwEdit) {
                    wwwEdit.onOk(server)
                    // force refresh
                    refreshServerView()
                })
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            selectMenu.options = [
                ["glyphicons-151-edit", function() { editSrvWww(model)}],
                ["glyphicons-372-global", function() { getAppState().openAssociatedFile(model.url)}],
                ["glyphicons-512-copy", function() { appContext.copyItem(model.password, true) }],
                ["glyphicons-193-circle-remove", function() {
                    appContext.confirmDelete(function() {
                        Utils.handleEither(getAppState().serverViewState
                                           .deleteServerWebsites([model.id]))
                        refreshServerView()
                    })
                }]]
            selectMenu.show(parent, global)
        }
    }
    Component {
        id: editSrvWwwComponent
        ServerWebsiteEdit {
            id: wwwEdit
        }
    }
}
