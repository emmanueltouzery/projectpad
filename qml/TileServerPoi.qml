import QtQuick 2.0
import "poiactions.js" as PoiActions

ItemTile {
	property int modelId: modelData.id
	property bool selected: false
	color: "light gray"
	border.width: selected ? 4 : 0
	border.color: "green"
	itemDesc: modelData.desc
	icon: PoiActions.actions[modelData.interestType].icon

	MouseArea {
		anchors.fill: parent
		onClicked: {
			var options = [["glyphicons-151-edit", function() { editPoi(modelData)}],
				["glyphicons-512-copy", function() { appContext.copyItem(modelData.path, false) }],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						serverViewState.deleteServerPois([modelData.id])
						poisrepeater.model = serverViewState.getPois(pv.model.id)
					})
				}]]
			if (pv.model.accessType === "SrvAccessSsh"
				&& pv.model.serverIp.length > 0
				&& pv.model.username.length > 0
				&& pv.model.password.length > 0) {
				switch (modelData.interestType) {
				case "PoiCommandToRun":
					options.push(["glyphicons-138-cogwheels", function() {
						var info = serverViewState.executePoiAction(pv.model, modelData)
						appContext.progressMessage("\nStarted program\n")
					}])
					break
				case "PoiLogFile":
					options.push(["glyphicons-283-cardio", function() {
						serverViewState.executePoiAction(pv.model, modelData)
					}])
					options.push(["glyphicons-52-eye-open", function() {
						serverViewState.executePoiSecondaryAction(pv.model, modelData)
					}])
					break
				case "PoiConfigFile":
					options.push(["glyphicons-52-eye-open", function() {
						serverViewState.executePoiAction(pv.model, modelData)
					}])
					break
				}
			}
			selectMenu.options = options
			selectMenu.show(parent)
		}
	}
}
