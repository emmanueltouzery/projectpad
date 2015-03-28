import QtQuick 2.0
import QtQuick.Controls 1.2
import "utils.js" as Utils

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
							projectEdit.activate({name: "Project name"})
						},
						function(projectEdit) {
							projectEdit.onOk()
						})
				break;
		}
	}

	signal loadView(string name, variant model)

	Flickable {
		width: parent.width
		contentHeight: flow.implicitHeight
		Loader {
			id: plLoader
		}
		Flow {
			anchors.fill: parent
			anchors.margins: 4
			spacing: 10
			id: flow

			Repeater {
				id: itemsrepeater
				model: projectListState.projects

				TileProject {
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
