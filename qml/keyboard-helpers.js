function handleKey(event, flow) {
    var items = getAllItems(flow)
    if (items.length === 0) {
        return
    }
    var focusedItem = getFocusedItemInfo(items)
    if (!focusedItem) {
        items[0].focus = true
        return
    }
    switch (event.key) {
    case Qt.Key_Left:
        focusPreviousItem(items, focusedItem)
        break;
    case Qt.Key_Right:
        focusNextItem(items, focusedItem)
        break;
    case Qt.Key_Down:
        focusItemDown(items, flow, focusedItem)
        break;
    case Qt.Key_Up:
        focusItemUp(items, flow, focusedItem)
        break;
    }
}

function containerToItemList(container) {
    if (container.isTile) {
        return [container]
    }
    if (container.children) {
        return container.children
    }
    if (!container.count) {
        return [container]
    }
    var result = []
    for (var i=0;i<container.count;i++) {
        result.push(container.itemAt(i));
    }
    return result
}

function getAllItems(qmlItem) {
    if (qmlItem.isTile) {
        return [qmlItem]
    }
    var items = containerToItemList(qmlItem)
    var result = []
    for (var i=0;i<items.length;i++) {
        result = result.concat(getAllItems(items[i]));
    }
    return result
}

function getFocusedItemInfo(repeater) {
    for (var i=0;i<repeater.length; i++) {
        var curItem = repeater[i]
        if (curItem.focus) {
            return {item: curItem, index: i}
        }
    }
    return null
}

function focusPreviousItem(repeater, focusedItem) {
    if (focusedItem.index > 0) {
        focusItem(repeater[focusedItem.index-1])
    }
}

function focusNextItem(repeater, focusedItem) {
    if (focusedItem.index < repeater.length-1) {
        focusItem(repeater[focusedItem.index+1])
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
        focusItem(repeater[itemOneRowAboveIndex])
    }
}

function focusItemDown(repeater, flow, focusedItem) {
    var rowCol = itemGetRowColInfo(flow, focusedItem)
    var itemOneRowUnderIndex = (rowCol.row+1)*rowCol.itemsPerRow+rowCol.col
    if (itemOneRowUnderIndex < repeater.length) {
        focusItem(repeater[itemOneRowUnderIndex])
    }
}

function focusItem(item) {
    item.activated(item)
    item.focus = true
}
