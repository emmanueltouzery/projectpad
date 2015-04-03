import QtQuick 2.0

ItemTile {
	id: tileServerDatabase
	property int modelId: model.id
	property bool selected: false
	property variant model
	width: 180; height: 180
	color: "gray"
	border.width: selected ? 4 : 0
	border.color: "green"
	itemDesc: model.desc
	icon: "glyphicons-528-database"
	property variant global: undefined

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
			selectMenu.options = [["glyphicons-151-edit", function() { editDb(model)}],
				["glyphicons-512-copy", function() { appContext.copyItem(model.password, true) }],
				["glyphicons-193-circle-remove", function() {
					appContext.confirmDelete(function() {
						var msg = serverViewState.canDeleteServerDatabase(model)
						if (msg !== null) {
							appContext.errorMessage(msg)
							return
						}
						serverViewState.deleteServerDatabases([model.id])
						dbsrepeater.model = serverViewState.getServerDatabases(pv.model.id)
					})
				}]]
			selectMenu.show(parent, tileServerDatabase.global)
		}
	}
	Component {
		id: editDatabaseComponent
		ServerDatabaseEdit {
			id: dbEdit
		}
	}
}
