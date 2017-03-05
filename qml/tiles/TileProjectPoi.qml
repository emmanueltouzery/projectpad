import QtQuick 2.0
import ".."
import "../poiactions.js" as PoiActions
import "../utils.js" as Utils

ItemTile {
    itemDesc: model.desc
    property variant model
    icon: PoiActions.actions[model.interestType].icon
    property int modelId: model.id
    color: "light gray"
    property variant global: undefined
    property variant project
    signal activated(variant tile)

    function editPoi(curPoi) {
        popup.setContents("Edit point of interest", poiEditComponent,
            function (poiEdit) {
                poiEdit.activate(project, curPoi)
            },
            function (poiEdit) {
                poiEdit.onOk()
                refreshProjectView()
            })
    }

    function tileId() {
        return { type: "TileProjectPoi", id: model.id }
    }

    onFocusChanged: {
        if (focus) {
            showMenu(this)
        } else if (typeof lineSelectMenu !== "undefined" && lineSelectMenu.visible) {
            // select menu & lineSelectMenu are exclusive (one or the other)
            selectMenu.visible = false
        }
    }

    function showMenu(item) {
        selectMenu.options = [[PoiActions.actions[model.interestType].icon, function() {
            var info = getAppState().projectViewState.runPoiAction(model)
            appContext.progressMessage("\nStarted program\n")
        }],
                              ["glyphicons-151-edit", function() {editPoi(model)}],
                              ["glyphicons-193-circle-remove", function() {
                                  appContext.confirmDelete(function() {
                                      Utils.handleEitherVoid(getAppState()
                                                             .projectViewState.deleteProjectPois([model.id]))
                                      // force refresh
                                      refreshProjectView()
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
        id: poiEditComponent
        PoiEdit {
            id: poiEdit
            isServerPoi: false
        }
    }
}
