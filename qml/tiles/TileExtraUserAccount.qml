import QtQuick 2.0
import QtQuick.Window 2.2
import ".."
import "../utils.js" as Utils
import "../remote-control.js" as Remote

ItemTile {
    property int modelId: model.id
    property variant model
    property variant server
    color: "dark gray"
    itemDesc: model.desc
    icon: "glyphicons-526-user-key"
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
        return { type: "TileExtraUserAccount", id: modelId }
    }

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

    function showMenu(item) {
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
        var desktopSize = {width: Screen.desktopAvailableWidth,
                           height: Screen.desktopAvailableHeight}
        options = options.concat(Remote.tileRemoteControlOptions(
            server, desktopSize,
            function (w,h) { return getAppState().serverViewState.runExtraUserRdp(model, w, h) },
            function() { return getAppState().serverViewState.openExtraUserSshSession(model) }))
        selectMenu.options = options
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
        id: editExtraUserAccountComponent
        ServerExtraUserAccountEdit {
            id: srvExtraUserAccountEdit
        }
    }
}
