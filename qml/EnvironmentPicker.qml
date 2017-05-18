import QtQuick 2.0
import QtQuick.Controls 1.2
import QtQuick.Layouts 1.1
import "core"

GridLayout {
    Layout.fillWidth: true
    Layout.columnSpan: 2
    columns: 2

    function setChecked(checkInfo) {
        envDevelopment.checked = checkInfo.dev
        envUat.checked = checkInfo.uat
        envStaging.checked = checkInfo.stg
        envProd.checked = checkInfo.prd
    }

    function getChecked() {
        return {
            dev: envDevelopment.checked,
            uat: envUat.checked,
            stg: envStaging.checked,
            prd: envProd.checked
        }
    }

    IconButton {
        id: envDevelopment
        iconX: 10
        iconTextPadding: 5
        Layout.fillWidth: true
        iconName: "glyphicons-361-bug"
        btnText: "Development"
        checkable: true
    }
    IconButton {
        id: envUat
        iconX: 10
        iconTextPadding: 5
        Layout.fillWidth: true
        iconName: "glyphicons-534-lab"
        btnText: "UAT"
        checkable: true
    }
    IconButton {
        id: envStaging
        iconX: 10
        iconTextPadding: 5
        Layout.fillWidth: true
        iconName: "glyphicons-140-adjust-alt"
        btnText: "Staging"
        checkable: true
    }
    IconButton {
        id: envProd
        iconX: 10
        iconTextPadding: 5
        Layout.fillWidth: true
        iconName: "glyphicons-333-certificate"
        btnText: "PROD"
        checkable: true
    }
}
