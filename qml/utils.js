function handleEither(eitherR, okCb) {
    var value = eitherR[1]
    if (eitherR[0] === "error") {
        errorMessage(value)
    } else if (okCb !== undefined) {
        okCb(value)
    }
}

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
    return -1
}
