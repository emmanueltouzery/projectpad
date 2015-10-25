import QtQuick 2.0

SearchView {
    id: dbPicker
    selectorMode: true
    model: search()

    property int preferredHeight: -1
    property bool widthResize: true

    function search() {
        return {
            matches: getAppState().search("DatabaseEntityType", ""),
            query: ""
        }
    }
}
