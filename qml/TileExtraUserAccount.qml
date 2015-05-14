import QtQuick 2.0
import "utils.js" as Utils

ItemTile {
    property int modelId: model.id
    property variant model
    property variant server
    property bool selected: false
    color: "dark gray"
    border.width: selected ? 4 : 0
    border.color: "green"
    itemDesc: model.desc
    icon: "glyphicons-526-user-key"
    property variant global: undefined

    function editExtraUserAccount(curUserAcct) {
        popup.setContents("Edit extra user account", editExtraUserAccountComponent,
                function (userEdit) {
                    userEdit.activate(server, curUserAcct)
                },
                function (userEdit) {
                    userEdit.onOk(server)
                    // force refresh
                    refreshServerView()
                })
    }
    MouseArea {
        anchors.fill: parent
        onClicked: {
            var options = [["glyphicons-151-edit", function() { editExtraUserAccount(model)}],
                ["glyphicons-512-copy", function() { appContext.copyItem(model.password, true) }],
                ["glyphicons-193-circle-remove", function() {
                    appContext.confirmDelete(function() {
                        Utils.handleEither(serverViewState.deleteServerExtraUserAccounts([model.id]))
                        refreshServerView()
                    })
                }]]
            if (model.authKeyFilename !== "...") {
                options.push(["glyphicons-45-keys", function() {
                    saveAuthKeyDialog.userAcct = model
                    saveAuthKeyDialog.visible = true
                }])
            }
            selectMenu.options = options
            selectMenu.show(parent, global)
        }
    }
    Component {
        id: editExtraUserAccountComponent
        ServerExtraUserAccountEdit {
            id: srvExtraUserAccountEdit
        }
    }
}
