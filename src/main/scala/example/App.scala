package tinyrenderer

import scala.scalajs.js
import monix.execution.Scheduler.Implicits.global
import js.annotation.JSExport
import org.scalajs.dom
import Commone.{Vec3, Color, Obj, Vertex, Vert, rotationY}
import scala.util.{Failure, Success}
import fr.hmil.roshttp.response.SimpleHttpResponse
import scala.util.{Try, Failure, Success}

object App extends js.JSApp {
  def main(): Unit = {
    val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
    val enginge = new Engine(canvas)
    val scene = enginge.createScene(Vec3(-1, 1, -1), Vec3(1, -1, 1))
    val color = Color(255.toShort, 255.toShort, 255.toShort)
    scene.clear
    // scene.triangle(
    //     Vec3(0.9, 0.8),
    //     Vec3(0.95, -0.7),
    //     Vec3(0.9, -0.6),
    //     color
    // )
    // println("ok") 
    // scene.dot(500, 500, color)
    enginge draw scene 
    // "obj/african_head/african_head.obj"
    Parser.get("obj/diablo3_pose/diablo3_pose.obj").onComplete({
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
            // scene.triangle(
            //   Vert(
            //     Vec3(0, 0, 0),
            //     Vec3(0.9, 0.9, 0.9)
            //   ),
            //    Vert(
            //     Vec3(0.9, -0.9, 1),
            //     Vec3(0.9, 0.9, 0.9)
            //   ),
            //    Vert(
            //     Vec3(0.9, 0.9, 0.5),
            //     Vec3(0.9, 0.9, 0.9) 
            //   ),
            //   color
            // ) 
          for ( ( fst, snd, trd ) <- obj.faces ) { 
            scene.triangle(
              Vert(
                obj.vertices(fst.vertex),
                obj.normals(fst.normal)
              ),
               Vert(
                obj.vertices(snd.vertex),
                obj.normals(snd.normal)
              ),
               Vert(
                obj.vertices(trd.vertex),
                obj.normals(trd.normal)
              ),
              color
            )
          }
          // val angle = 0
          // for ( ( fst, snd, trd ) <- obj.faces ) { 
          //   scene.triangle(
          //     Vert(
          //       rotationY( obj.vertices(fst.vertex), angle ),
          //       rotationY( obj.normals(fst.normal), angle )
          //     ),
          //      Vert(
          //       rotationY( obj.vertices(snd.vertex), angle ),
          //       rotationY( obj.normals(snd.normal), angle )
          //     ),
          //      Vert(
          //       rotationY( obj.vertices(trd.vertex), angle ),
          //       rotationY( obj.normals(trd.normal), angle )
          //     ),
          //     color
          //   )
          // }
          enginge draw scene 
        }
        case e: Failure[SimpleHttpResponse] => println("Huston, we got a problem!")
      }) 
    dom.document.getElementById("main").appendChild(canvas)
  }
}
