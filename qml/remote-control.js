.import "utils.js" as Utils

function tileRemoteControlOptions(server, desktopSize, rdpCallback, sshCallback) {
    var options = []
    if (server.accessType === "SrvAccessRdp" &&
            server.username.length > 0) {
        options.push(["glyphicons-489-multiple-displays", function() {
            var desktopWidth = desktopSize.width
            if (desktopSize.width / desktopSize.height > 3) {
                // I can assume a double monitor setup: divide the
                // width by two to compensate.
                desktopWidth = desktopWidth / 2
            }
            Utils.handleEitherVoid(
                rdpCallback(
                    Math.round(desktopWidth * 0.75),
                    Math.round(desktopSize.height * 0.75)))
        }])
    }
    if ((server.accessType === "SrvAccessSsh" || server.accessType === "SrvAccessSshTunnel") &&
            server.username.length > 0) {
        options.push(["glyphicons-489-multiple-displays", function() {
            Utils.runIfSshHostTrusted(server, function () {
                Utils.handleEitherVoid(sshCallback())
            })
        }])
    }
    return options
}
