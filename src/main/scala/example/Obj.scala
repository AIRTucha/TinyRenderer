package tinyrenderer

import scala.concurrent.{ Future, Promise }
import Commone.{ Vec3, Vec2, Color, Vertex, Vert, rotationY }
import fr.hmil.roshttp.response.SimpleHttpResponse
import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom

class Obj(
    val vertices: Array[Vec3], 
    val normals: Array[Vec3], 
    val textures: Array[Vec2], 
    val faces: Array[( Vertex, Vertex, Vertex )] 
) {
    def render(enginge: Engine) = {
        val scene = enginge.createScene(Vec3(-1, 1, -1), Vec3(1, -1, 1))
        val color = Color(255.toShort, 255.toShort, 255.toShort)
        enginge draw scene 
        for ( ( fst, snd, trd ) <- faces ) { 
            scene.triangle(
              Vert(
                vertices(fst.vertex),
                normals(fst.normal),
                textures(fst.texture)
              ),
              Vert(
                vertices(snd.vertex),
                normals(snd.normal),
                textures(snd.texture)
              ),
              Vert(
                vertices(trd.vertex),
                normals(trd.normal),
                textures(trd.texture)
              ),
              color
            )
          }
          enginge draw scene 
    }
} 
object Obj {
    def apply(url: String): Future[Obj] = {
        get("obj/african_head/african_head.obj") map { res: SimpleHttpResponse => 
          val values = res.body.split("\n").map( str => str.split(" ") )
          new Obj(
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
            values.withFilter(_(0) == "vt") map {
              value =>
                Vec2(
                  value(2).toDouble, 
                  value(3).toDouble
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
        }
    }
    def get( url: String ) = HttpRequest( s"${dom.window.location.href}/${url}" ).send   
}