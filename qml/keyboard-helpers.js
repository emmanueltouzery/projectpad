var debugGrid = false

function handleKey(event, flow, selectMenu) {
    if (event.modifiers === Qt.AltModifier && event.key === Qt.Key_Left) {
        toolbar.backAction()
        return
    }
    if (event.modifiers === Qt.AltModifier && event.key === Qt.Key_Right) {
        toolbar.forwardAction()
        return
    }

    var items = getAllItems(flow)
    if (items.length === 0) {
        return
    }
    var focusedItem = getFocusedItemInfo(items)
    if (!focusedItem) {
        items[0].focus = true
        return
    }
    if (event.modifiers === Qt.ControlModifier && event.key === Qt.Key_U) {
        // move to the parent (server if we're under a server, project if we're a server)
        // i need a proper maybe/option type!!!
        var projectId =
            (focusedItem.item.project ? focusedItem.item.project.id : null) ||
            (focusedItem.item.projectId ? focusedItem.item.projectId() : null)
        if (focusedItem.item.server) {
            // the parent case happens from the search view
            var serverId = focusedItem.item.server.parent ?
                focusedItem.item.server.parent.id : focusedItem.item.server.id
            var server = getAppState().projectListState.getServerById(serverId)
            var project = Utils.findById(
                getAppState().projectListState.projects, server.projectId)
            loadView("ProjectView.qml",
                     {project: project, environment: server.environment},
                     focusedItem.item.tileId(), {type: "TileServer", id: serverId})
        } else if (projectId) {
            var project = Utils.findById(
                getAppState().projectListState.projects, projectId)
            loadView("ProjectList.qml",
                     {project: project, environment: focusedItem.item.environment},
                     focusedItem.item.tileId(), {type: "TileProject", id: projectId})
        }
    }
    switch (event.key) {
    case Qt.Key_Left:
        focusPreviousItem(items, flow, focusedItem)
        break
    case Qt.Key_Right:
        focusNextItem(items, flow, focusedItem)
        break
    case Qt.Key_Down:
        focusItemDown(items, flow, focusedItem)
        break
    case Qt.Key_Up:
        focusItemUp(items, flow, focusedItem)
        break
    }

    var options = focusedItem.item.isTile ? selectMenu.options : lineSelectMenu.options
    var shortcuts = options
        .map(function (opt) { return opt[0]; })
        .map(function (icon) {
            var v = iconShortcuts[icon]
            if (!v) {
                console.warn("No shortcut for icon " + icon)
            }
            return v
        });
    var shortcutIndex = shortcuts.indexOf(event.text)
    if (shortcutIndex >= 0) {
        options[shortcutIndex][1]()
    }
}

// shortcut key alphabetically ordered for a quickcheck
// that we don't duplicate them
var iconShortcuts = {
    "glyphicons-58-history": "b", // not documented, not sure there's much point?
    "glyphicons-512-copy": "c",
    "glyphicons-182-download-alt": "d",
    "glyphicons-151-edit": "e",
    "glyphicons-283-cardio": "f",
    "glyphicons-138-cogwheels": "g",
    "glyphicons-45-keys": "k",
    "glyphicons-145-folder-open": "o",
    "glyphicons-333-certificate": "p",
    "glyphicons-489-multiple-displays": "r",
    "glyphicons-140-adjust-alt": "s",
    "glyphicons-361-bug": "t",
    "glyphicons-534-lab": "u",
    "glyphicons-52-eye-open": "v",
    "glyphicons-372-global": "w",
    "glyphicons-193-circle-remove": "x"
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
 * we return the list of items, separated by nulls which signal the
 * reset of the layout to the next row.
 */
function getAllItems(qmlItem) {
    if (qmlItem.isTile) {
        return [qmlItem]
    }
    var items = containerToItemList(qmlItem)
    var result = []
    for (var i=0;i<items.length;i++) {
        var curItem = items[i]
        if (curItem.isTile) {
            result.push(curItem)
        } else if (curItem.isServerHeader) {
            result.push(curItem)
            result.push(null) // force a reset of the row after this, as it takes the whole row
        } else {
            var isFlow = curItem.isFlow || curItem.toString().indexOf("QQuickFlow") >= 0
            if (isFlow) {
                if (result.length > 0 && result[result.length-1] !== null) {
                    result.push(null)
                }
            }
            var children = getAllItems(curItem)
            result = result.concat(children);
        }
    }
    return result
}

function getFocusedItemInfo(repeater) {
    for (var i=0;i<repeater.length; i++) {
        var curItem = repeater[i]
        if (curItem && curItem.focus) {
            return {item: curItem, index: i}
        }
    }
    return null
}

function focusPreviousItem(items, flow, focusedItem) {
    focusItemToLeft(items, flow, focusedItem, function (curRowCol, rowCol) {
        return (curRowCol.row === rowCol.row && curRowCol.col < rowCol.col) ||
            (curRowCol.row < rowCol.row)
    })
}

function focusNextItem(items, flow, focusedItem) {
    focusItemToRight(items, flow, focusedItem, function (curRowCol, rowCol) {
        return (curRowCol.row === rowCol.row && curRowCol.col > rowCol.col)  ||
            (curRowCol.row > rowCol.row)
    })
}

function getTileWidth(flow, items) {
    var tileItems = items.filter(function (it) { return it && it.isTile })
    return tileItems.length > 0 ? tileItems[0].width : flow.width-flow.spacing
}

function itemGetRowColInfoMultipleFlows(flow, item, items) {
    // find a tile item for the width
    var itemsPerRow = Math.floor(flow.width / (getTileWidth(flow, items)+flow.spacing))
    var remainingIdx = item.index
    var curRow = 0
    var curCol = 0
    for (var i=0;i<items.length;i++) {
        var curItem = items[i]
        if (curItem === null) {
            ++curRow
            curCol = 0
        } else {
            if (debugGrid && curItem.itemDesc) {
                curItem.itemDesc = curRow + ", " + curCol
            }
            if (i === item.index) {
                return {row: curRow, col: curCol, itemsPerRow: itemsPerRow}
            }
            ++curCol
            if (curCol >= itemsPerRow) {
                ++curRow
                curCol = 0
            }
        }
    }
    console.error("itemGetRowColInfoMultipleFlows: didn't find the item!")
}

function focusItemUp(items, flow, focusedItem) {
    focusItemToLeft(items, flow, focusedItem, function (curRowCol, rowCol) {
        return (curRowCol.row < rowCol.row && curRowCol.col <= rowCol.col)
    })
}

function focusItemToLeft(items, flow, focusedItem, cellAcceptCheck) {
    var rowCol = itemGetRowColInfoMultipleFlows(flow, focusedItem, items)
    // calculating the previous item index based on row, column, itemsPerRow and
    // some multiplications is too naive because rows may not be complete in
    // case several flows are in play, like so:
    // I I I
    // I I      <-- empty item because next row is a new flow
    // I I I
    var curItemIndex = focusedItem.index
    for (; curItemIndex>=0; curItemIndex--) {
        var curItem = items[curItemIndex]
        if (curItem === null) {
            continue
        }
        var curRowCol = itemGetRowColInfoMultipleFlows(
            flow, {item:curItem, index:curItemIndex}, items)
        if (cellAcceptCheck(curRowCol, rowCol)) {
            break
        }
    }
    // need the Math.max() because in case we don't find anything,
    // the for loop will call curItemIndex-- one extra time when we finish
    focusItem(items[Math.max(curItemIndex, 0)])
}

function focusItemDown(items, flow, focusedItem) {
    focusItemToRight(items, flow, focusedItem, function (curRowCol, rowCol) {
        return (curRowCol.row > rowCol.row && curRowCol.col >= rowCol.col)
    })
}

function focusItemToRight(items, flow, focusedItem, cellAcceptCheck) {
    var rowCol = itemGetRowColInfoMultipleFlows(flow, focusedItem, items)
    // calculating the next item index based on row, column, itemsPerRow and
    // some multiplications is too naive because rows may not be complete in
    // case several flows are in play, like so:
    // I I I
    // I I      <-- empty item because next row is a new flow
    // I I I
    var curItemIndex = focusedItem.index
    for (; curItemIndex<items.length; curItemIndex++) {
        var curItem = items[curItemIndex]
        if (curItem === null) {
            continue
        }
        var curRowCol = itemGetRowColInfoMultipleFlows(
            flow, {item:curItem, index:curItemIndex}, items)
        if (cellAcceptCheck(curRowCol, rowCol)) {
            break
        }
    }
    // need the Math.min() because in case we don't find anything,
    // the for loop will call curItemIndex++ one extra time when we finish
    var lastIndex = items.length-1
    while (lastIndex >= 0 && items[lastIndex] === null) {
        --lastIndex
    }
    focusItem(items[Math.min(curItemIndex, lastIndex)])
}

function focusItem(item) {
    item.activated(item)
    item.focus = true
}
