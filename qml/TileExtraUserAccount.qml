import QtQuick 2.0

ItemTile {
	property int modelId: modelData.id
	property bool selected: false
	color: "dark gray"
	border.width: selected ? 4 : 0
	border.color: "green"
	itemDesc: modelData.desc
	icon: "glyphicons-526-user-key"
	MouseArea {
		anchors.fill: parent
		onClicked: {
			var options = [["glyphicons-151-edit", function() { editExtraUserAccount(modelData)}],
				["glyphicons-512-copy", function() { appContext.copyItem(modelData.password, true) }],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						serverViewState.deleteServerExtraUserAccounts([modelData.id])
						useraccountsrepeater.model = serverViewState.getServerExtraUserAccounts(pv.model.id)
					})
				}]]
			if (modelData.authKeyFilename !== "...") {
				options.push(["glyphicons-45-keys", function() {
					saveAuthKeyDialog.userAcct = modelData
					saveAuthKeyDialog.visible = true
				}])
			}
			selectMenu.options = options
			selectMenu.show(parent)
		}
	}
}
