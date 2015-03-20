import QtQuick 2.0
import QtQuick.Controls 1.3

Rectangle {
	color: "light grey"
	property int preferredHeight: 200

	function init() {
	}

	function next() {
		if (addServerPoi.checked) {
			return 1
		} else if (addServerWebsite.checked) {
			return 2
		} else if (addServerDatabase.checked) {
			return 3
		}
	}

	ExclusiveGroup { id: addGroup }

	Column {
		spacing: 10
		anchors.verticalCenter: parent.verticalCenter
		x: 10

		RadioButton {
			id: addServerPoi
			text: "Add point of interest"
			exclusiveGroup: addGroup
			checked: true
		}
		RadioButton {
			id: addServerWebsite
			text: "Add website"
			exclusiveGroup: addGroup
		}
		RadioButton {
			id: addServerDatabase
			text: "Add database"
			exclusiveGroup: addGroup
		}
	}
}
