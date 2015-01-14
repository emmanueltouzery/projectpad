import QtQuick 2.0
import QtQuick.Controls 1.2
import "selection.js" as Select
import "utils.js" as Utils

ScrollView {
	id: projectList
	anchors.fill: parent
	property variant model /* model is ignored in this screen */

	property variant actions: [
		["addprj", "glyphicons-146-folder-plus", "Add project"]]

	property bool editMode

	function getBreadCrumbs() {
		return {pathLinks: [], title: ''};
	}

	function actionTriggered(name) {
		switch (name) {
			case "addprj":
				popup.setContents("Add project", projectEditComponent,
						function(projectEdit) {
							projectEdit.activate({name: "Project name"})
						},
						function(projectEdit) {
							projectEdit.onOk()
						})
				break;
			case "edit":
				popup.setContents("Edit project", projectEditComponent,
						function (projectEdit) {
							var curProject = Utils.findById(projectListState.projects, Select.selectedItems["prj"][0])
							projectEdit.activate(curProject)
						},
						function (projectEdit) {
							projectEdit.onOk()
						})
				break;
			case "delete":
				projectListState.deleteProjects(Select.selectedItems["prj"])
				break;
		}
	}

	signal selectionChange(int selectionCount)
	signal loadView(string name, variant model)

	onSelectionChange: Select.updateSelectDisplay("prj", itemsrepeater)
	onEditModeChanged: Select.clearSelection(projectList.selectionChange)

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

				ItemTile {
					property int modelId: modelData.id
					property bool selected: false
					color: "light blue"
					border.width: selected ? 4 : 0
					border.color: "green"
					itemDesc: modelData.name
					icon: "glyphicons-441-folder-closed"

					MouseArea {
						anchors.fill: parent
						onClicked: {
							Select.handleClick(projectList.selectionChange, "prj", modelData.id, function() {
								loadView("ProjectView.qml", modelData)
							})
						}
					}
				}
			}
		}
		Component {
			id: projectEditComponent
			ProjectEdit {
				id: projectEdit
			}
		}
	}
}
