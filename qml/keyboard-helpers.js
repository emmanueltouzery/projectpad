function handleKey(event, flow, repeater) {
    if (repeater.count === 0) {
        return
    }
    var focusedItem = getFocusedItemInfo(repeater)
    if (!focusedItem) {
        repeater.itemAt(0).focus = true
        return
    }
    switch (event.key) {
    case Qt.Key_Left:
        focusPreviousItem(repeater, focusedItem)
        break;
    case Qt.Key_Right:
        focusNextItem(repeater, focusedItem)
        break;
    case Qt.Key_Down:
        focusItemDown(repeater, flow, focusedItem)
        break;
    case Qt.Key_Up:
        focusItemUp(repeater, flow, focusedItem)
        break;
    }
}

function getFocusedItemInfo(repeater) {
    for (var i=0;i<repeater.count; i++) {
        var curItem = repeater.itemAt(i)
        if (curItem.focus) {
            return {item: curItem, index: i}
        }
    }
    return null
}

function focusPreviousItem(repeater, focusedItem) {
    if (focusedItem.index > 0) {
        focusItem(repeater.itemAt(focusedItem.index-1))
    }
}

function focusNextItem(repeater, focusedItem) {
    if (focusedItem.index < repeater.count-1) {
        focusItem(repeater.itemAt(focusedItem.index+1))
    }
}

function itemGetRowColInfo(flow, item) {
    var itemsPerRow = Math.floor(flow.width / (item.item.width+flow.spacing))
    var myRow = Math.floor(item.index / itemsPerRow)
    var myIndexInRow = item.index % itemsPerRow
    return {row: myRow, col: myIndexInRow, itemsPerRow: itemsPerRow}
}

function focusItemUp(repeater, flow, focusedItem) {
    var rowCol = itemGetRowColInfo(flow, focusedItem)
    var itemOneRowAboveIndex = (rowCol.row-1)*rowCol.itemsPerRow+rowCol.col
    if (itemOneRowAboveIndex >= 0) {
        focusItem(repeater.itemAt(itemOneRowAboveIndex))
    }
}

function focusItemDown(repeater, flow, focusedItem) {
    var rowCol = itemGetRowColInfo(flow, focusedItem)
    var itemOneRowUnderIndex = (rowCol.row+1)*rowCol.itemsPerRow+rowCol.col
    if (itemOneRowUnderIndex < repeater.count) {
        focusItem(repeater.itemAt(itemOneRowUnderIndex))
    }
}

function focusItem(item) {
    item.activated(item)
    item.focus = true
}
