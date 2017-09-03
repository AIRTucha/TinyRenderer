package tinyrenderer

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global
import scala.util.{Failure, Success}
import fr.hmil.roshttp.response.SimpleHttpResponse

object TinyRenderer extends js.JSApp {
  def main(): Unit = {
    HttpRequest(s"${dom.window.location.href}/obj/floor.obj").send().onComplete({
        case res:Success[SimpleHttpResponse] => println("ok")
        case e: Failure[SimpleHttpResponse] => println("Huston, we got a problem!")
      })
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
