import QtQuick 2.0

ItemTile {
	property int modelId: modelData.id
	property bool selected: false
	color: "light slate gray"
	border.width: selected ? 4 : 0
	border.color: "green"
	itemDesc: modelData.desc
	icon: "glyphicons-372-global"

	function editSrvWww(curPoi) {
		popup.setContents("Edit website", editSrvWwwComponent,
				function (wwwEdit) {
					wwwEdit.activate(curPoi)
				},
				function (wwwEdit) {
					wwwEdit.onOk()
					// force refresh
					wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
				})
	}

	MouseArea {
		anchors.fill: parent
		onClicked: {
			selectMenu.options = [["glyphicons-151-edit", function() { editSrvWww(modelData)}],
				["glyphicons-372-global", function() { openAssociatedFile(modelData.url)}],
				["glyphicons-512-copy", function() { appContext.copyItem(modelData.password, true) }],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						serverViewState.deleteServerWebsites([modelData.id])
						wwwsrepeater.model = serverViewState.getServerWebsites(pv.model.id)
					})
				}]]
			selectMenu.show(parent)
		}
	}
	Component {
		id: editSrvWwwComponent
		ServerWebsiteEdit {
			id: wwwEdit
		}
	}
}
