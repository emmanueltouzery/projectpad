import QtQuick 2.0
import QtQuick.Window 2.2
import ".."
import "../server-menu.js" as ServerMenu
import "../utils.js" as Utils

ItemTile {
    property int modelId: modelData.serverLink.id
    color: "light blue"
    itemDesc: modelData.serverLink.desc
    icon: "glyphicons-152-new-window"
    property variant global: undefined
    signal activated(variant tile)

    MouseArea {
        anchors.fill: parent
        onClicked: {
            var desktopSize = {width: Screen.desktopAvailableWidth,
                               height: Screen.desktopAvailableHeight}
            var customEdit = function() {
                popup.setContents(
                    "Edit link to server", serverLinkEditComponent,
                    function (serverLinkEdit) {
                        serverLinkEdit.activate(
                            pv.model.project, modelData.serverLink,
                            modelData.serverLink.environment)
                    },
                    function (serverLinkEdit) {
                        serverLinkEdit.onOk(pv.model.project)
                        refreshProjectView()
                    });
            }
            var customDelete = function() {
                appContext.confirmDelete(function() {
                    Utils.handleEitherVoid(getAppState().projectViewState.deleteServerLinks([modelData.serverLink.id]))
                    // force refresh
                    refreshProjectView()
                })
            }
            var overrides = {
                edit: customEdit,
                delete: customDelete
            };
            ServerMenu.showSelectMenu(
                pv.model.project, modelData.server, parent, desktopSize,
                function() { refreshProjectView() }, selectMenu, global, overrides)
            activated(parent)
        }
    }
    Component {
        id: serverEditComponent
        ServerEdit {
            id: serverEdit
        }
    }
}
