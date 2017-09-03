package tinyrenderer

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom

object TinyRenderer extends js.JSApp {
  def main(): Unit = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    canvas.width = 1000;
    canvas.height = 1000;
    val ctx = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
    ctx.strokeStyle = "red"
    ctx.lineWidth = 3
    ctx.beginPath()
    ctx.moveTo(canvas.width/3, 0)
    ctx.lineTo(canvas.width/3, canvas.width/3)
    ctx.moveTo(canvas.width*2/3, 0)
    ctx.lineTo(canvas.width*2/3, canvas.width/3)
    ctx.moveTo(canvas.width, canvas.width/2)
    ctx.arc(canvas.width/2, canvas.width/2, canvas.width/2, 0, 3.14)

    ctx.stroke()
    dom.document.getElementById("main").appendChild(canvas)
  }
}
