import QtQuick 2.0
import QtQuick.Controls 1.2

Rectangle {
    property int preferredHeight: -1
    property bool widthResize: true
    property string entityType
    property var extraFilter

    function focusSearch(txt) {
        searchText.forceActiveFocus()
        if (txt) {
            searchText.text = txt
        }
        searchText.selectAll()
    }

    function setSelectedItem(item) {
        dbPicker.setSelectedItem(item)
    }

    function getSelectedItem() {
        return dbPicker.getSelectedItem()
    }

    function search() {
        var basis = getAppState().search(entityType, searchText.text)
        var result = extraFilter
            ? basis.map(extraFilter).filter(function(item) {return item !== null})
            : basis
        return {
            matches: result,
            query: searchText.text
        }
    }

    TextField {
        id: searchText
        width: parent.width
        onTextChanged: {
            var item = dbPicker.getSelectedItem()
            dbPicker.model = search()
            if (item != null) {
                dbPicker.setSelectedItem(item.id)
            } else {
                dbPicker.setSelectedItem(null)
            }
        }
        Image {
            anchors { top: parent.top; right: parent.right; margins: 7 }
            source: '../glyphicons-free/glyphicons-28-search.png'
            fillMode: Image.PreserveAspectFit
            height: parent.height - 7 * 2
            width: parent.height - 7 * 2
        }
    }

    SearchView {
        id: dbPicker
        y: searchText.height
        width: parent.width
        height: parent.height - searchText.height
        selectorMode: true
        isPickingServers: entityType === "ServerEntityType"
        model: search()
    }
}
