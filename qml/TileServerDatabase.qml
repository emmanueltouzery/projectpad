import QtQuick 2.0

ItemTile {
	property int modelId: modelData.id
	property bool selected: false
	width: 180; height: 180
	color: "gray"
	border.width: selected ? 4 : 0
	border.color: "green"
	itemDesc: modelData.desc
	icon: "glyphicons-528-database"

	function editDb(curDb) {
		popup.setContents("Edit database", editDatabaseComponent,
				function (dbEdit) {
					dbEdit.activate(curDb)
				},
				function (dbEdit) {
					dbEdit.onOk()
					// force refresh
					dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
				})
	}

	MouseArea {
		anchors.fill: parent
		onClicked: {
			selectMenu.options = [["glyphicons-151-edit", function() { editDb(modelData)}],
				["glyphicons-512-copy", function() { appContext.copyItem(modelData.password, true) }],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						var msg = serverViewState.canDeleteServerDatabase(modelData)
						if (msg !== null) {
							appContext.errorMessage(msg)
							return
						}
						serverViewState.deleteServerDatabases([modelData.id])
						dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
					})
				}]]
			selectMenu.show(parent)
		}
	}
	Component {
		id: editDatabaseComponent
		ServerDatabaseEdit {
			id: dbEdit
		}
	}
}
