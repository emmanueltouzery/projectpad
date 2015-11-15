import QtQuick 2.0
import ".."
import "../utils.js" as Utils

ItemTile {
    property int modelId: model.id
    property variant model
    property variant server
    color: "dark gray"
    itemDesc: model.desc
    icon: "glyphicons-526-user-key"
    property variant global: undefined
    signal activated(variant tile)

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
            var options = [
                ["glyphicons-151-edit", function() { editExtraUserAccount(model)}],
                ["glyphicons-512-copy", function() {
                    appContext.copyItemEntity("ServerExtraUserEntityType",
                                                  model.id, true)
                }],
                ["glyphicons-193-circle-remove", function() {
                    appContext.confirmDelete(function() {
                        Utils.handleEitherVoid(getAppState().serverViewState
                                           .deleteServerExtraUserAccounts([model.id]))
                        refreshServerView()
                    })
                }]]
            if (model.authKeyFilename !== "...") {
                options.push(["glyphicons-45-keys", function() {
                    Utils.handleEither(
                        getAppState().serverViewState.saveAuthKey(model),
                        function(location) {successMessage("Saved file to " + location)});
                }])
            }
            selectMenu.options = options
            selectMenu.show(parent, global)
            activated(parent)
        }
    }
    Component {
        id: editExtraUserAccountComponent
        ServerExtraUserAccountEdit {
            id: srvExtraUserAccountEdit
        }
    }
}
