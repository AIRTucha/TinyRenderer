package tinyrenderer

import scala.concurrent.{ Future, Promise }
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8ClampedArray
import monix.execution.Scheduler.Implicits.global
import js.annotation.JSExport
import org.scalajs.dom
import Obj._
import Commone.{ Vec3, Vec2, Color, Vertex, Vert, rotationY }
import Texture._
import scala.util.{Failure, Success}
import fr.hmil.roshttp.response.SimpleHttpResponse
import scala.util.{Try, Failure, Success}

object App extends js.JSApp {
  def main(): Unit = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    val enginge = new Engine(canvas)
    dom.document.getElementById("main").appendChild(canvas)
    Obj("obj/african_head/african_head.obj") map {
      obj => obj.render(enginge)
    }
    // val tex = Texture("obj/african_head/african_head_diffuse.jpg")
    // tex onSuccess {
    //   case r => println(r.get(0.1, 0.1))
    // }

  }
}
