package tinyrenderer

import scala.concurrent.{Future, Promise}
import scala.scalajs.js
import scala.scalajs.js.typedarray.Uint8ClampedArray
import monix.execution.Scheduler.Implicits.global
import js.annotation.JSExport
import org.scalajs.dom
import Obj._
import Commone.{ Vec3, Vec2, Color, Indeces, Vertex, rotationY }
import Texture._
import scala.util.{ Try, Failure, Success }
import fr.hmil.roshttp.response.SimpleHttpResponse

object App extends js.JSApp {
  def main(): Unit = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    val enginge = new Engine(canvas)
    dom.document.getElementById("main").appendChild(canvas)
    Obj("obj/african_head/african_head.obj") map { obj =>
      val scene = enginge.Scene(Vec3(-1, 1, -1), Vec3(1, -1, 1))
      // obj draw scene
      scene.clear
      enginge render scene
      obj drawDebugingTriangle scene
      enginge render scene
    }
  }
}
