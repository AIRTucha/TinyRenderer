package tinyrenderer

import scala.concurrent.{Future, Promise}
import Commone.{Vec3, Vec2, Color, Indeces, Vertex, rotationY}
import fr.hmil.roshttp.response.SimpleHttpResponse
import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom
import scala.math.{floor, abs, pow, max, min}
import Commone.{
  Vec2,
  Vec3,
  Color,
  interpolate,
  dotProduct,
  Vertex,
  normalize,
  crossProduct
}
class Obj(
    val vertices: Array[Vec3],
    val normals: Array[Vec3],
    val textures: Array[Vec2],
    val faces: Array[(Indeces, Indeces, Indeces)],
    val deffuse: Texture,
    val normalsTex: Texture,
    val specular: Texture
) {
  def forEachPolygon( action: ( Vertex, Vertex, Vertex) => Unit ) = {
    val vert = Vertex(Vec3(1, 1,1), Vec3(1,1,1), Vec2(1,1))
    for ( (fst, snd, trd) <- faces ) {
      action(
        Vertex(
          vertices( fst.vertex ),
          normals( fst.normal ),
          textures( fst.texture )
        ),
        Vertex(
          vertices( snd.vertex ),
          normals( snd.normal ),
          textures( snd.texture )
        ),
        Vertex(
          vertices( trd.vertex ),
          normals( trd.normal ),
          textures( trd.texture )
        )
      )
    }
}
}

// class ObjDNSMaps extends 
object Obj {
  def apply[T >: Obj](modelUrl: String, deffuseUrl: String, normalsUrl: String, specularUrl: String): Future[T] = {
    for {
      obj <- get(modelUrl) map { _.body.split("\n").map( _.split(" ") ) } 
      deffuse <- Texture(deffuseUrl)
      normals <- Texture(normalsUrl)
      specular <- Texture(specularUrl)
    } yield new Obj(
      parseV(obj),
      parseVN(obj),
      parseVT(obj),
      parseF(obj),
      deffuse,
      normals,
      specular
    )
  }
  def get(url: String) = HttpRequest(s"${dom.window.location.href}/${url}").send
  def parseV(data: Array[Array[String]]) = data
    .withFilter(_(0) == "v")
    .map{ value =>
      Vec3(
        value(1).toDouble,
        value(2).toDouble,
        value(3).toDouble
      )
    }
  def parseVN(data: Array[Array[String]]) = data
    .withFilter(_(0) == "vn")
    .map{ value =>
      Vec3(
        value(2).toDouble,
        value(3).toDouble,
        value(4).toDouble
      )
    }
  def parseVT(data: Array[Array[String]]) = data
    .withFilter(_(0) == "vt")
    .map{ value =>
      Vec2(
        value(2).toDouble,
        value(3).toDouble
      )
    }
  def parseF(data: Array[Array[String]]) = data
    .withFilter(_(0) == "f")
    .map { 
      value => (
        value(1).split("/") match {
          case Array(fst, snd, trd) =>
            Indeces(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1)
        },
        value(2).split("/") match {
          case Array(fst, snd, trd) =>
            Indeces(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1)
        },
        value(3).split("/") match {
          case Array(fst, snd, trd) =>
            Indeces(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1)
        }
      )
    }
}
