import QtQuick 2.0

import "utils.js" as Utils

Canvas {
	id: canvas
	width: 180
	height: 180

	property variant options;
	property int radius : 70
	property int innerRadius: 70/2.5
	property int centerX : 90
	property int centerY : 90

	onOptionsChanged: {
		// never unloading images. there's a bounded number anyway,
		// the number of icons. memory use shouldn't be a problem.
		var imgs = getAllImages()
		for (var i=0;i<imgs.length;i++) {
			if (!isImageLoaded(imgs[i])) {
				loadImage(imgs[i])
			}
		}
	}

	function getAllImages() {
		return Utils.map(options, function(opt) {
			return getImagePath(opt[0])
		})
	}

	function getImagePath(iconName) {
		return '../glyphicons-free/' + iconName + '.png'
	}

	function allImagesLoaded() {
		return Utils.all(getAllImages(), function(imgPath) {
			return isImageLoaded(imgPath)
		})
	}

	function show(parnt, global) {
	if (global !== undefined) {
		var globalCoords = parnt.mapToItem(global, 0, 0)
		selectMenu.x = globalCoords.x
		selectMenu.y = globalCoords.y
	} else {
		selectMenu.y = parnt.y
		selectMenu.x = parnt.x
        }
		selectMenu.visible = true
		selectMenu.requestPaint()
	}

	onImageLoaded: {
		if (allImagesLoaded()) {
			requestPaint()
		}
	}

	onPaint: {
		if (options === undefined || !allImagesLoaded()) {
			return
		}
		var colors = ["dark gray", "gray", "slate gray"]
		var ctx = canvas.getContext("2d")
		ctx.clearRect ( 0 , 0 , canvas.width, canvas.height );
		var portionsRange = Math.PI*2/options.length
		for (var i=0;i<options.length;i++) {
			ctx.beginPath()
			ctx.fillStyle = colors[i%colors.length]
			ctx.arc(centerX, centerY, radius, Math.PI*2-portionsRange*i, Math.PI*2-portionsRange*(i+1), true)
			var endAngle = portionsRange+(i+1)
			ctx.arc(centerX, centerY, innerRadius, Math.PI*2-portionsRange*(i+1), Math.PI*2-portionsRange*i, false)
			ctx.fill();

			ctx.fillStyle = "white"
			ctx.textAlign = "center"
			ctx.textBaseline = "middle"
			ctx.font = "16px sans-serif"
			var angle = portionsRange*i + portionsRange/2

			var imageData = ctx.createImageData(getImagePath(options[i][0]))
			ctx.drawImage(imageData,
				centerX + Math.cos(angle)*radius/1.5 - imageData.width/2,
				centerY - Math.sin(angle)*radius/1.5 - imageData.height/2)
		}
	}

	MouseArea {
		anchors.fill: parent
		onClicked: {
			parent.visible = false
			// are we in the circle?
			var distance = Math.sqrt(Math.pow((mouse.y-centerY), 2)
					+ Math.pow(mouse.x-centerX, 2))
			if (distance > radius || distance < innerRadius) {
				// out of the clickable area
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
