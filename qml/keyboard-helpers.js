function handleKey(event, flow) {
    var items = getAllItems(flow)
    if (items.items.length === 0) {
        return
    }
    var focusedItem = getFocusedItemInfo(items.items)
    if (!focusedItem) {
        items.items[0].focus = true
        return
    }
    switch (event.key) {
    case Qt.Key_Left:
        focusPreviousItem(items.items, focusedItem)
        break;
    case Qt.Key_Right:
        focusNextItem(items.items, focusedItem)
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

/**
 * Get all tile items from this QML containers hierarchy,
 * grouped by Flow. Since the layout is reset for each flow,
 * eg:
 * I I I
 * I I      <-- empty item because next row is a new flow
 * I I I
 * we return the list of items and also the lengths of each flow.
 */
function getAllItems(qmlItem) {
    if (qmlItem.isTile) {
        return {items: [qmlItem], flowLengths: [1]}
    }
    var items = containerToItemList(qmlItem)
    var result = []
    var flowLengths = [0]
    for (var i=0;i<items.length;i++) {
        var curItem = items[i]
        var isFlow = curItem.toString().indexOf("QQuickFlow") >= 0
        var children = getAllItems(curItem)
        result = result.concat(children.items);
        if (isFlow && flowLengths[flowLengths.length-1] > 0) {
            flowLengths.push(children.items.length)
        } else {
            flowLengths[flowLengths.length-1] += children.items.length
        }
    }
    return {items: result, flowLengths: flowLengths}
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


function itemGetRowColInfoMultipleFlows(flow, item, flowLengths) {
    var remainingIdx = item.index
    var curRow = 0
    for (var i=0;i<flowLengths.length;i++) {
        if (remainingIdx > flowLengths[i]-1) {
            // the item is actually in a later flow
            var r = itemGetRowColInfo(flow, {item:item.item, index:flowLengths[i]-1})
            if (flowLengths[i] > 0) {
                curRow += r.row+1
            }
            // item.item.itemDesc = r.row + ", " + r.col
            remainingIdx -= flowLengths[i]
        } else {
            // the item is in this flow
            var r = itemGetRowColInfo(flow, {item:item.item, index:remainingIdx})
            // item.item.itemDesc = curRow+r.row + ", " + r.col
            return {row: curRow+r.row, col: r.col, itemsPerRow: r.itemsPerRow}
        }
    }
}

function focusItemUp(items, flow, focusedItem) {
    var rowCol = itemGetRowColInfoMultipleFlows(flow, focusedItem, items.flowLengths)
    // calculating the previous item index based on row, column, itemsPerRow and
    // some multiplications is too naive because rows may not be complete in
    // case several flows are in play, like so:
    // I I I
    // I I      <-- empty item because next row is a new flow
    // I I I
    var curItemIndex = focusedItem.index
    for (; curItemIndex>=0; curItemIndex--) {
        var curItem = items.items[curItemIndex]
        var curRowCol = itemGetRowColInfoMultipleFlows(
            flow, {item:curItem, index:curItemIndex}, items.flowLengths)
        if (curRowCol.row < rowCol.row && curRowCol.col <= rowCol.col) {
            break
        }
    }
    // need the Math.max() because in case we don't find anything,
    // the for loop will call curItemIndex-- one extra time when we finish
    focusItem(items.items[Math.max(curItemIndex, 0)])
}

function focusItemDown(items, flow, focusedItem) {
    var rowCol = itemGetRowColInfoMultipleFlows(flow, focusedItem, items.flowLengths)
    // calculating the next item index based on row, column, itemsPerRow and
    // some multiplications is too naive because rows may not be complete in
    // case several flows are in play, like so:
    // I I I
    // I I      <-- empty item because next row is a new flow
    // I I I
    var curItemIndex = focusedItem.index
    for (; curItemIndex<items.items.length; curItemIndex++) {
        var curItem = items.items[curItemIndex]
        var curRowCol = itemGetRowColInfoMultipleFlows(
            flow, {item:curItem, index:curItemIndex}, items.flowLengths)
        if (curRowCol.row > rowCol.row && curRowCol.col >= rowCol.col) {
            break
        }
    }
    // need the Math.min() because in case we don't find anything,
    // the for loop will call curItemIndex++ one extra time when we finish
    focusItem(items.items[Math.min(curItemIndex, items.items.length-1)])
}

function focusItem(item) {
    item.activated(item)
    item.focus = true
}
