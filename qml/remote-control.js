.import "utils.js" as Utils

function tileRemoteControlOptions(server, desktopSize, rdpCallback, sshCallback) {
    var options = []
    if (server.accessType === "SrvAccessRdp" &&
            server.username.length > 0) {
        options.push(["glyphicons-489-multiple-displays", function() {
            // it's a bit messy to compute ideal sizes for the RDP window,
            // because when people have multiple monitors, Qt gives us the
            // combined resolution.
            // I used to have heuristics like:
            // var desktopWidth = desktopSize.width
            // if (desktopSize.width / desktopSize.height > 3) {
            //     // I can assume a double monitor setup: divide the
            //     // width by two to compensate.
            //     desktopWidth = desktopWidth / 2
            // }
            // But now I just assume that the vertical resolution is correct,
            // that people are not stacking monitors vertically, and then
            // hardcode a 16:9 aspect ratio
            Utils.handleEitherVoid(
                rdpCallback(
                    Math.round(desktopSize.height*16/9 * 0.75),
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
