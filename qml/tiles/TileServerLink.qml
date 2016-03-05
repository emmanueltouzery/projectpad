import QtQuick 2.0
import QtQuick.Window 2.2
import ".."
import "../server-menu.js" as ServerMenu
import "../utils.js" as Utils

ItemTile {
    property int modelId: model.serverLink.id
    property variant model
    color: "#e0e8ef"
    itemDesc: model.serverLink.desc
    icon: ServerMenu.getServerIcon(model.server)
    property variant global: undefined
    signal activated(variant tile)
    property var serverLinkEditComponent
    property variant project

    Image {
        x: 5
        y: 145
        source: '../../glyphicons-free/glyphicons-152-new-window.png'
    }

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
                            project, model.serverLink,
                            model.serverLink.environment)
                    },
                    function (serverLinkEdit) {
                        serverLinkEdit.onOk(project)
                        refreshProjectView()
                    });
            }
            var customDelete = function() {
                appContext.confirmDelete(function() {
                    Utils.handleEitherVoid(getAppState().projectViewState.deleteServerLinks([model.serverLink.id]))
                    // force refresh
                    refreshProjectView()
                })
            }
            var overrides = {
                edit: customEdit,
                delete: customDelete
            };
            ServerMenu.showSelectMenu(
                project, model.server, parent, desktopSize,
                function() { refreshProjectView() }, selectMenu, global, overrides)
            activated(parent)
        }
    }

    // I must refer to the component through a string,
    // if I refer to it properly (commented code), then
    // I have a dependency loop in the search view.
    Component.onCompleted: {
        serverLinkEditComponent = Qt.createComponent("../ServerLinkEdit.qml")
    }
//    Component {
//        id: serverEditComponent
//        ServerEdit {
//            id: serverEdit
//        }
//    }
}
