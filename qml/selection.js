var selectedItems = []

function addToSelection(item) {
	selectedItems.push(item)
}

function removeFromSelection(item) {
	selectedItems.splice(selectedItems.indexOf(item), 1)
}

function clearSelection(signal) {
	selectedItems.splice(0, selectedItems.length)
	signal(selectedItems)
}

function isSelected(item) {
	return selectedItems.indexOf(item) >= 0
}

function handleClick(selectChange, item, f) {
	if (editMode) {
		if (isSelected(item)) {
			removeFromSelection(item)
		} else {
			addToSelection(item)
		}
		selectChange(selectedItems)
	} else {
		f()
	}
}

function updateSelectDisplay(repeater) {
	for (var i=0;i<repeater.count;i++) {
		var item = repeater.itemAt(i)
		item.selected = isSelected(item.modelId)
	}
}
