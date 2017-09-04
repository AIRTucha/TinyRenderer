package tinyrenderer

import scala.scalajs.js
import monix.execution.Scheduler.Implicits.global
import js.annotation.JSExport
import org.scalajs.dom
import Commone.{Vec3, Color, Obj, Face}
import scala.util.{Failure, Success}
import fr.hmil.roshttp.response.SimpleHttpResponse
import scala.util.{Try, Failure, Success}

object App extends js.JSApp {
  def main(): Unit = {

    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    val enginge = new Engine(canvas)
    val scene = enginge.createScene
    val color = Color(129.asInstanceOf[Short], 255.toShort, 255.toShort)
    // for( i <- 1 to 100)
    //   scene.dot(Vec3(i, 100), Color(129.asInstanceOf[Short], 255.toShort, 255.toShort))
    scene.clear
    scene.line( Vec3(0, 100), Vec3(400, 400), color )
    enginge draw scene
    Parser.get("obj/african_head/african_head.obj").onComplete({
        case res:Success[SimpleHttpResponse] => {
          val values = res.get.body.split("\n").map( str => str.split(" ") )
          Obj(
            values.withFilter(_(0) == "v") map {
              value =>
                Vec3(
                  value(1).toDouble, 
                  value(2).toDouble, 
                  value(3).toDouble
                ) 
            },
            values.withFilter(_(0) == "vn") map {
              value =>
                Vec3(
                  value(2).toDouble, 
                  value(3).toDouble, 
                  value(4).toDouble
                ) 
            },
            values.withFilter(_(0) == "f")
              .map {
                value => (
                  value(1).split("/") match { case Array( fst, snd, trd ) => Face(fst.toInt, snd.toInt, trd.toInt) },
                  value(1).split("/") match { case Array( fst, snd, trd ) => Face(fst.toInt, snd.toInt, trd.toInt) },
                  value(1).split("/") match { case Array( fst, snd, trd ) => Face(fst.toInt, snd.toInt, trd.toInt) }
                )
              }
          )
        }
        case e: Failure[SimpleHttpResponse] => println("Huston, we got a problem!")
      })
    dom.document.getElementById("main").appendChild(canvas)
  }
}
