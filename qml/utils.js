function runIfSshHostTrusted(server, f) {
    if (!getAppState().serverViewState.isHostTrusted(server.serverIp)) {
        var title = "Server '" + server.serverIp + "' is not trusted!"
        var msg = "Add the key automatically to the trust store?"
        appContext.confirmDanger(title, msg, "Add", function() {
            getAppState().serverViewState.addInHostTrustStore(server.serverIp)
            f();
        })
    } else {
        f()
    }
}

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
