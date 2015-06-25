import QtQuick 2.0
import "poiactions.js" as PoiActions
import "utils.js" as Utils

ItemTile {
    itemDesc: model.desc
    property variant model
    icon: PoiActions.actions[model.interestType].icon
    property int modelId: model.id
    color: "light gray"
    property variant global: undefined
    property variant project

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

    MouseArea {
        anchors.fill: parent
        onClicked: {
            selectMenu.options = [[PoiActions.actions[model.interestType].icon, function() {
                var info = getAppState().projectViewState.runPoiAction(model)
                appContext.progressMessage("\nStarted program\n")
                }],
                ["glyphicons-151-edit", function() {editPoi(model)}],
                ["glyphicons-193-circle-remove", function() {
                    appContext.confirmDelete(function() {
                        Utils.handleEither(getAppState()
                                           .projectViewState.deleteProjectPois([model.id]))
                        // force refresh
                        refreshProjectView()
                    })
                }]]
            selectMenu.show(parent, global)
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
