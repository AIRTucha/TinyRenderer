package tinyrenderer

import scala.scalajs.js
import js.annotation.JSExport
import org.scalajs.dom
import Commone.{Vec3, Color}

object App extends js.JSApp {
  def main(): Unit = {

    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    val enginge = new Engine(canvas)
    val scene = enginge.createScene
    val color = Color(129.asInstanceOf[Short], 255.toShort, 255.toShort)
    // for( i <- 1 to 100)
    //   scene.dot(Vec3(i, 100), Color(129.asInstanceOf[Short], 255.toShort, 255.toShort))
    scene.line( Vec3(0, 100), Vec3(400, 400), color )
    enginge draw scene

    dom.document.getElementById("main").appendChild(canvas)
  }
}
