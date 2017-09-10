package tinyrenderer

import scala.scalajs.js
import monix.execution.Scheduler.Implicits.global
import js.annotation.JSExport
import org.scalajs.dom
import Commone.{Vec3, Color, Obj, Vertex}
import scala.util.{Failure, Success}
import fr.hmil.roshttp.response.SimpleHttpResponse
import scala.util.{Try, Failure, Success}

object App extends js.JSApp {
  def main(): Unit = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    val enginge = new Engine(canvas)
    val scene = enginge.createScene(Vec3(-1, 1, -1), Vec3(1, -1, 1))
    val color = Color(129.asInstanceOf[Short], 255.toShort, 255.toShort)
    scene.clear
    // scene.triangle(
    //     Vec3(0.9, 0.79),
    //     Vec3(0.5, -0.7),
    //     Vec3(-0.7, 0.8),
    //     color
    // ) 
    // println("ok") 
    // scene.dot(500, 500, color)
    enginge draw scene 

    Parser.get("obj/african_head/african_head.obj").onComplete({
        case res:Success[SimpleHttpResponse] => {
          val values = res.get.body.split("\n").map( str => str.split(" ") )
          val obj = Obj(
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
                  value(1).split("/") match { case Array( fst, snd, trd ) => Vertex(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1) },
                  value(2).split("/") match { case Array( fst, snd, trd ) => Vertex(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1) },
                  value(3).split("/") match { case Array( fst, snd, trd ) => Vertex(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1) }
                )
              }
          )
          println(obj.faces.length)
          for ( ( fst, snd, trd ) <- obj.faces ) {
            scene.triangle(
              obj.vertices(fst.vertex),
              obj.vertices(snd.vertex),
              obj.vertices(trd.vertex),
              color
            )
            // scene.line( obj.vertices(fst.vertex), obj.vertices(snd.vertex), color )
            // scene.line( obj.vertices(snd.vertex), obj.vertices(trd.vertex), color )
            // scene.line( obj.vertices(trd.vertex), obj.vertices(fst.vertex), color )
          }
          // scene.line(obj.vertices(obj.faces(0)._1.vertex), obj.vertices(obj.faces(0)._2.vertex), color )
          enginge draw scene
        }
        case e: Failure[SimpleHttpResponse] => println("Huston, we got a problem!")
      }) 
    dom.document.getElementById("main").appendChild(canvas)
  }
}
