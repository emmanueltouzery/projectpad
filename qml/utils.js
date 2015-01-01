function findById(list, id) {
	for (var i=0;i<list.length;i++) {
		if (list[i].id === id) {
			return list[i];
		}
	}
	return null
}

function listModelGetValueIndex(listModel, value) {
	for (var i=0;i<listModel.count;i++) {
		if (listModel.get(i).value === value) {
			return i;
		}
	}
	return 0
}

function filter(list, predicate) {
	var result = []
	for (var i=0;i<list.length;i++) {
		if (predicate(list[i])) {
			result.push(list[i])
		}
	}
	return result
}

function map(list, transform) {
	var result = []
	for (var i=0;i<list.length;i++) {
		result.push(transform(list[i]))
	}
	return result
}
