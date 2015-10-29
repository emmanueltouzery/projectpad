function projectGetCustomIcon(project) {
    return getAppState().projectListState.projectIconsFolder +
        "/" + project.id + ".png"
}

function runIfSshHostTrusted(server, f) {
    handleEither(
        getAppState().serverViewState.isHostTrusted(server),
        function(isTrusted) {
            if (!isTrusted) {
                var title = "Server '" + server.serverIp + "' is not trusted!"
                var msg = "Add the key automatically to the trust store?"
                appContext.confirmDanger(title, msg, "Add", function() {
                    handleEitherVoid(getAppState().serverViewState.addInHostTrustStore(server), function() {
                        f();
                    })
                })
            } else {
                f()
            }
        })
}

function handleEitherVoid(eitherR, okCb) {
    if (!eitherR.success) {
        errorMessage(eitherR.errorMsg)
    } else if (okCb !== undefined) {
        okCb()
    }
}

function handleEither(eitherR, okCb) {
    if (!eitherR.success) {
        errorMessage(eitherR.errorMsg)
    } else if (okCb !== undefined) {
        okCb(eitherR.value)
    }
}

function fromEither(defaultValue, eitherR) {
    if (!eitherR.success) {
        return defaultValue
    }
    return eitherR.value
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

// in normal JS I would serialize to JSON & back,
// pretty sure that wouldn't work in QML
// http://stackoverflow.com/a/11390499/516188
function deepCopy(p) {
    var c = {};
    for (var i in p) {
        if (p[i] !== null && typeof p[i] === 'object') {
            c[i] = (p[i].constructor === Array) ? [] : {};
            deepCopy(p[i], c[i]);
        } else {
            c[i] = p[i];
        }
    }
    return c;
}

function scrollInView(tile, scrollView, flickable) {
    var tileTop = tile.mapToItem(scrollView, 0, 0).y + flickable.contentY
    if (flickable.contentY > tileTop) {
        flickable.contentY = tileTop
    }
    var positionSoItsAtBottom = tileTop + tile.height - scrollView.height
    if (flickable.contentY < positionSoItsAtBottom) {
        flickable.contentY = positionSoItsAtBottom
    }
}
