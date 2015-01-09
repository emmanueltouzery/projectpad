var selectedItems = {}

function addToSelection(category, item) {
	if (selectedItems[category] === undefined) {
		selectedItems[category] = []
	}
	selectedItems[category].push(item)
}

function removeFromSelection(category, item) {
	selectedItems[category].splice(selectedItems[category].indexOf(item), 1)
}

function getSelectionCount() {
	var count = 0
	for (var property in selectedItems) {
		count += selectedItems[property].length
	}
	return count
}

function clearSelection(signal) {
	for (var property in selectedItems) {
		selectedItems[property].splice(0, selectedItems[property].length)
	}
	signal(getSelectionCount())
}

function isSelected(category, item) {
	if (selectedItems[category] === undefined) {
		return false
	}
	return selectedItems[category].indexOf(item) >= 0
}

function handleClick(selectChange, category, item, f) {
	if (editMode) {
		if (isSelected(category, item)) {
			removeFromSelection(category, item)
		} else {
			addToSelection(category, item)
		}
		selectChange(getSelectionCount())
	} else {
		f()
	}
}

function updateSelectDisplay(category, repeater) {
	for (var i=0;i<repeater.count;i++) {
		var item = repeater.itemAt(i)
		item.selected = isSelected(category, item.modelId)
	}
}

function getSelectedItem(categoriesToConsider) {
	for (var i=0;i<categoriesToConsider.length;i++) {
		var cat = categoriesToConsider[i]
		if (selectedItems[cat].length > 0) {
			return [cat, selectedItems[cat][0]]
		}
	}
	return null
}
