import QtQuick 2.0

Canvas {
	id: canvas
	width: 180
	height: 180

	property variant options;
	property int radius : 70
	property int centerX : 90
	property int centerY : 90

	onPaint: {
		if (options === undefined) {
			return
		}
		var colors = ["dark gray", "gray", "slate gray"]
		var ctx = canvas.getContext("2d")
		var portionsRange = Math.PI*2/options.length
		for (var i=0;i<options.length;i++) {
			ctx.beginPath()
			ctx.fillStyle = colors[i%colors.length]
			ctx.arc(centerX, centerY, radius, Math.PI*2-portionsRange*i, Math.PI*2-portionsRange*(i+1), true)
			ctx.lineTo(centerX, centerY)
			ctx.fill();

			ctx.fillStyle = "white"
			ctx.textAlign = "center"
			ctx.font = "20px sans-serif"
			var angle = portionsRange*i + portionsRange/2
			ctx.fillText(options[i][0],
				centerX + Math.cos(angle)*radius/2,
				centerY - Math.sin(angle)*radius/2)
		}
	}

	MouseArea {
		anchors.fill: parent
		onClicked: {
			parent.visible = false
			// are we in the circle?
			var distance = Math.sqrt(Math.pow((mouse.y-centerY), 2)
					+ Math.pow(mouse.x-centerX, 2))
			if (distance > radius) {
				// out of the circle
				return
			}
			var angle0 = Math.atan2(mouse.y - centerY, mouse.x - centerX)
			var angle
			if (angle0 < 0) {
				angle = -angle0
			} else {
				angle = 2*Math.PI-angle0
			}
			var selectedOption = options[Math.floor(options.length*angle/(2*Math.PI))]
			selectedOption[1]()
		}
	}
}
