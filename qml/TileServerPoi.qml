import QtQuick 2.0
import "poiactions.js" as PoiActions

ItemTile {
	property int modelId: modelData.id
	property bool selected: false
	color: "light gray"
	border.width: selected ? 4 : 0
	border.color: "green"
	itemDesc: modelData.desc
	property variant server
	icon: PoiActions.actions[modelData.interestType].icon

	MouseArea {
		anchors.fill: parent
		onClicked: {
			var options = [["glyphicons-151-edit", function() { editPoi(modelData)}],
				["glyphicons-512-copy", function() { appContext.copyItem(modelData.path, false) }],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						serverViewState.deleteServerPois([modelData.id])
						poisrepeater.model = serverViewState.getPois(server.id)
					})
				}]]
			if (server.accessType === "SrvAccessSsh"
				&& server.serverIp.length > 0
				&& server.username.length > 0
				&& server.password.length > 0) {
				switch (modelData.interestType) {
				case "PoiCommandToRun":
					options.push(["glyphicons-138-cogwheels", function() {
						var info = serverViewState.executePoiAction(server, modelData)
						appContext.progressMessage("\nStarted program\n")
					}])
					break
				case "PoiLogFile":
					options.push(["glyphicons-283-cardio", function() {
						serverViewState.executePoiAction(server, modelData)
					}])
					options.push(["glyphicons-52-eye-open", function() {
						serverViewState.executePoiSecondaryAction(server, modelData)
					}])
					break
				case "PoiConfigFile":
					options.push(["glyphicons-52-eye-open", function() {
						serverViewState.executePoiAction(server, modelData)
					}])
					break
				}
			}
			selectMenu.options = options
			selectMenu.show(parent)
		}
	}
}
