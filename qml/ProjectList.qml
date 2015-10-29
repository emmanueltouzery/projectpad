import QtQuick 2.0
import QtQuick.Controls 1.2
import "utils.js" as Utils
import "core"
import "tiles"

ScrollView {
    id: projectList
    anchors.fill: parent
    property variant model /* model is ignored in this screen */
    property variant appContext: null

    property variant actions: [
        ["addprj", "glyphicons-146-folder-plus", "Add project"]]

    function getBreadCrumbs() {
        return {pathLinks: [], title: ''};
    }

    function actionTriggered(name) {
        switch (name) {
            case "addprj":
                popup.setContents("Add project", projectEditComponent,
                        function(projectEdit) {
                            popup.implicitClose = false
                            projectEdit.activate(Utils.deepCopy(projectEdit.basicModel))
                        },
                        function(projectEdit) {
                            projectEdit.onOk()
                        })
                break;
        }
    }

    signal loadView(string name, variant model)

    Flickable {
        id: projectsFlickable
        width: parent.width
        contentHeight: flow.implicitHeight
        pixelAligned: true
        Loader {
            id: plLoader
        }
        Flow {
            anchors.fill: parent
            anchors.topMargin: 7
            anchors.leftMargin: 4
            anchors.rightMargin: 4
            anchors.bottomMargin: 4
            spacing: 10
            id: flow

            Repeater {
                id: itemsrepeater
                model: getAppState().projectListState.projects

                TileProject {
                    onActivated: Utils.scrollInView(tile, projectList, projectsFlickable)
                }
            }
        }
        SelectMenu {
            id: selectMenu
            visible: false
            z: 3
        }
        Component {
            id: projectEditComponent
            ProjectEdit {
                id: projectEdit
                appContext: projectList.appContext
            }
        }
    }
}
