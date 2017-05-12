import QtQuick 2.0
import "../utils.js" as Utils

ItemTile {

    property int modelId: modelData.id
    color: "light blue"
    itemDesc: modelData.name
    signal activated(variant tile)
    icon: {
        if (modelData.hasCustomIcon) {
            return Utils.projectGetCustomIcon(modelData)
        } else {
            return "glyphicons-441-folder-closed"
        }
    }

    onFocusChanged: {
        if (focus) {
            selectMenu.options = getActions(modelData)
            selectMenu.show(this)
        }
    }

    function tileId() {
        return { type: "TileProject", id: modelId }
    }

    function getActions(projectModel) {
        var environments = [];
        if (projectModel.hasDev) {
            environments.push(["glyphicons-361-bug", function() {
                loadView("ProjectView.qml", {"project": projectModel, "environment": "EnvDevelopment"}, tileId(), null)
            }])
        }
        if (projectModel.hasUat) {
            environments.push(["glyphicons-534-lab", function() {
                loadView("ProjectView.qml", {"project": projectModel, "environment": "EnvUat"}, tileId(), null)
            }])
        }
        if (projectModel.hasStaging) {
            environments.push(["glyphicons-140-adjust-alt", function() {
                loadView("ProjectView.qml", {"project": projectModel, "environment": "EnvStage"}, tileId(), null)
            }])
        }
        if (projectModel.hasProd) {
            environments.push(["glyphicons-333-certificate", function() {
                loadView("ProjectView.qml", {"project": projectModel, "environment": "EnvProd"}, tileId(), null)
            }])
        }
        return environments.concat([
            ["glyphicons-151-edit", function() {
                popup.setContents("Edit project", projectEditComponent,
                    function (projectEdit) {
                        popup.implicitClose = false
                        projectEdit.activate(projectModel)
                    },
                    function (projectEdit) {
                        projectEdit.onOk()
                        loadView("ProjectList.qml", null, tileId(), null)
                    })
            }],
            ["glyphicons-193-circle-remove", function() {
                appContext.confirmDelete(function() {
                    Utils.handleEitherVoid(
                        getAppState().projectListState.deleteProjects([projectModel.id]))
                })
            }]
        ]);
    }

    MouseArea {
        anchors.fill: parent
        onClicked: {
            selectMenu.options = getActions(modelData)
            selectMenu.show(parent)
            activated(parent)
        }
    }
}
