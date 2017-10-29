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
    // Obj(
    //   "obj/diablo3_pose/diablo3_pose.obj",
    //   "obj/diablo3_pose/diablo3_pose_diffuse.jpg",
    //   "obj/diablo3_pose/diablo3_pose_nm.jpg",
    //   "obj/diablo3_pose/diablo3_pose_spec.jpg"
    // ) map { obj =>
    //   val scene = enginge.Scene(Vec3(-1, 1, -1), Vec3(1, -1, 1))
    //   obj draw scene
    //   enginge render scene
    // }
    println( Matrix4x4(
      (2, 1, 2, 1),
      (1, 1, 1, 1),
      (1, 2, 3, 3),
      (1, 2, 3, 4)
    ).invert )
    Obj(
      "obj/african_head/african_head.obj",  
      "obj/african_head/african_head_diffuse.jpg",
      "obj/african_head/african_head_nm.jpg",
      "obj/african_head/african_head_spec.jpg"
    ) map { obj =>
      val scene1 = enginge.Scene(Vec3(-1, 1, -1), Vec3(1, -1, 1))
      val scene2 = enginge.Scene(Vec3(-1, 1, -1), Vec3(1, -1, 1))
      ObjPipeline.draw(obj, scene1)
      enginge render scene1
      ObjTexPipeline.draw(obj, scene2)
      enginge render scene2
        // obj.forEachPolygon( a )
    }
  }
}
