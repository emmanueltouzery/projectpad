import QtQuick 2.0
import "utils.js" as Utils

ItemTile {
    property int modelId: model.id
    property bool selected: false
    color: "light slate gray"
    border.width: selected ? 4 : 0
    border.color: "green"
    itemDesc: model.desc
    property variant model
    icon: "glyphicons-372-global"
    property variant global: undefined

    function editSrvWww(curPoi) {
        popup.setContents("Edit website", editSrvWwwComponent,
                function (wwwEdit) {
                    wwwEdit.activate(curPoi)
                },
                function (wwwEdit) {
                    wwwEdit.onOk()
                    // force refresh
                    refreshWwws()
                })
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            selectMenu.options = [["glyphicons-151-edit", function() { editSrvWww(model)}],
                ["glyphicons-372-global", function() { openAssociatedFile(model.url)}],
                ["glyphicons-512-copy", function() { appContext.copyItem(model.password, true) }],
                ["glyphicons-193-circle-remove", function() {
                    appContext.confirmDelete(function() {
                        Utils.handleEither(serverViewState.deleteServerWebsites([model.id]))
                        refreshWwws()
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
