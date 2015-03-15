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

	function getActions(projectModel) {
		var environments = [];
		if (projectModel.hasDev === "True") {
			environments.push(["glyphicons-361-bug", function() {
				loadView("ProjectView.qml", {"project": projectModel, "environment": "EnvDevelopment"})
			}])
		}
		if (projectModel.hasUat === "True") {
			environments.push(["glyphicons-534-lab", function() {
				loadView("ProjectView.qml", {"project": projectModel, "environment": "EnvUat"})
			}])
		}
		if (projectModel.hasStaging === "True") {
			environments.push(["glyphicons-140-adjust-alt", function() {
				loadView("ProjectView.qml", {"project": projectModel, "environment": "EnvStage"})
			}])
		}
		if (projectModel.hasProd === "True") {
			environments.push(["glyphicons-333-certificate", function() {
				loadView("ProjectView.qml", {"project": projectModel, "environment": "EnvProd"})
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
						loadView("ProjectList.qml", null)
					})
			}],
			["glyphicons-193-circle-remove", function() {
				appContext.confirmDelete(function() {
					projectListState.deleteProjects([projectModel.id])
				})
			}]
		]);
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
							selectMenu.options = getActions(modelData)
							selectMenu.show(parent)
							//loadView("ProjectView.qml", modelData)
						}
					}
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
