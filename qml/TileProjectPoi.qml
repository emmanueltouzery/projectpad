import QtQuick 2.0
import "poiactions.js" as PoiActions

ItemTile {
	itemDesc: modelData.desc
	icon: PoiActions.actions[modelData.interestType].icon
	property int modelId: modelData.id
	property bool selected: false
	color: "light gray"
	border.width: selected ? 4 : 0
	border.color: "green"

	function editPoi(curPoi) {
		popup.setContents("Edit point of interest", poiEditComponent,
			function (poiEdit) {
				poiEdit.activate(curPoi)
			},
			function (poiEdit) {
				poiEdit.onOk()
				// force refresh
				poisrepeater.model = projectViewState.getPois(pv.model.project.id)
			})
	}

	MouseArea {
		anchors.fill: parent
		onClicked: {
			selectMenu.options = [[PoiActions.actions[modelData.interestType].icon, function() {
				var info = projectViewState.runPoiAction(modelData)
				appContext.progressMessage("\nStarted program\n")
				}],
				["glyphicons-151-edit", function() {editPoi(modelData)}],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						projectViewState.deleteProjectPois([modelData.id])
						// force refresh
						poisrepeater.model = projectViewState.getPois(pv.model.project.id)
					})
				}]]
			selectMenu.show(parent)
		}
	}
	Component {
		id: poiEditComponent
		PoiEdit {
			id: poiEdit
		}
	}
}
