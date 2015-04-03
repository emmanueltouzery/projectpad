import QtQuick 2.0
import "poiactions.js" as PoiActions

ItemTile {
	property int modelId: model.id
	property variant model
	property bool selected: false
	color: "light gray"
	border.width: selected ? 4 : 0
	border.color: "green"
	itemDesc: model.desc
	property variant server
	icon: PoiActions.actions[model.interestType].icon
	property variant global: undefined

	MouseArea {
		anchors.fill: parent
		onClicked: {
			var options = [["glyphicons-151-edit", function() { editPoi(model)}],
				["glyphicons-512-copy", function() { appContext.copyItem(model.path, false) }],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						serverViewState.deleteServerPois([model.id])
						poisrepeater.model = serverViewState.getPois(server.id)
					})
				}]]
			if (server.accessType === "SrvAccessSsh"
				&& server.serverIp.length > 0
				&& server.username.length > 0
				&& server.password.length > 0) {
				switch (model.interestType) {
				case "PoiCommandToRun":
					options.push(["glyphicons-138-cogwheels", function() {
						var info = serverViewState.executePoiAction(server, model)
						appContext.progressMessage("\nStarted program\n")
					}])
					break
				case "PoiLogFile":
					options.push(["glyphicons-283-cardio", function() {
						serverViewState.executePoiAction(server, model)
					}])
					options.push(["glyphicons-52-eye-open", function() {
						serverViewState.executePoiSecondaryAction(server, model)
					}])
					break
				case "PoiConfigFile":
					options.push(["glyphicons-52-eye-open", function() {
						serverViewState.executePoiAction(server, model)
					}])
					break
				}
			}
			selectMenu.options = options
			selectMenu.show(parent, global)
		}
	}
}
