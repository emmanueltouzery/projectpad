import QtQuick 2.0

Canvas {
	id: canvas
	width: 150
	height: 10+menuItems.length*cellHeight

	property variant menuItems: []
	property variant fontSpec: "14px sans-serif"
	property int cellHeight: 30
	property int arrowStartOffsetFromEnd: 25
	property int arrowEndOffsetFromEnd: 5
	property int topOffset: 10

	onPaint: {
		var ctx = canvas.getContext("2d")
		ctx.fillStyle = "light gray"
		ctx.clearRect (0 , 0 , canvas.width, canvas.height);
		ctx.beginPath()
		ctx.moveTo(0, topOffset)
		ctx.lineTo(canvas.width - arrowStartOffsetFromEnd, topOffset)
		ctx.lineTo(canvas.width - arrowStartOffsetFromEnd
			+ (arrowStartOffsetFromEnd-arrowEndOffsetFromEnd)/2, 0)
		ctx.lineTo(canvas.width - arrowEndOffsetFromEnd, topOffset)
		ctx.lineTo(canvas.width, topOffset)
		ctx.lineTo(canvas.width, canvas.height)
		ctx.lineTo(0, canvas.height)
		ctx.lineTo(0, topOffset)
		ctx.fill()
		ctx.stroke()

		ctx.fillStyle = "black"
		ctx.font = fontSpec
		ctx.textBaseline = "middle"
		for (var i=0;i<menuItems.length;i++) {
			ctx.fillText(menuItems[i][0], 5, 10+cellHeight*i+cellHeight/2)
		}
	}

	MouseArea {
		anchors.fill: parent
		onClicked: {
			if (mouse.y < topOffset) {
				return
			}
			parent.visible = false
			var index = Math.floor((mouse.y-topOffset) / cellHeight)
			menuItems[index][1]()
		}
	}
}
