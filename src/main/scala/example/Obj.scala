package tinyrenderer

import scala.concurrent.{Future, Promise}
import Commone.{Vec3, Vec2, Color, Indeces, Vertex, rotationY}
import fr.hmil.roshttp.response.SimpleHttpResponse
import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global
import org.scalajs.dom
import scala.math.{floor, abs, pow}
import Commone.{
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
    val deffuse: Texture
) {
  def draw(scene: Scene) = {
    for ( (fst, snd, trd) <- faces ) {
      triangle(
        Vertex(
          scene scale vertices( fst.vertex ),
          normals( fst.normal ),
          textures( fst.texture )
        ),
        Vertex(
          scene scale vertices( snd.vertex ),
          normals( snd.normal ),
          textures( snd.texture )
        ),
        Vertex(
          scene scale vertices( trd.vertex ),
          normals( trd.normal ),
          textures( trd.texture )
        ),
        scene
      )
    }
  }
  def drawDebugingTriangle(scene: Scene) = {
    triangle(
      Vertex(
        scene scale Vec3(-1, -1, 1),
        Vec3(0, 1, 1),
        Vec2(0, 0)
      ),
      Vertex(
        scene scale Vec3(1, -0.999, 0.5),
        Vec3(1, 1, 1),
        Vec2(1, 0)
      ),
      Vertex(
        scene scale Vec3(1, 1, 0),
        Vec3(0, 0, 1),
        Vec2(0, 1)
      ),
      scene
    )
  }
  def triangle(vec1: Vertex, vec2: Vertex, vec3: Vertex, scene: Scene) =
    if (
      dotProduct(Vec3(0, 0, 1), vec1.normal) > 0 ||
      dotProduct(Vec3(0, 0, 1), vec2.normal) > 0 ||
      dotProduct(Vec3(0, 0, 1), vec3.normal) > 0
    ) {
      var vert1 = vec1
      var vert2 = vec2
      var vert3 = vec3
      if (vert1.vertex.y > vert2.vertex.y) {
        val buff = vert1
        vert1 = vert2
        vert2 = buff
      }
      if (vert2.vertex.y > vert3.vertex.y) {
        val buff = vert2
        vert2 = vert3
        vert3 = buff
      }
      if (vert1.vertex.y > vert2.vertex.y) {
        val buff = vert1
        vert1 = vert2
        vert2 = buff
      }

      val d1 =
        if (vert2.vertex.y - vert1.vertex.y > 0)
          (vert2.vertex.x - vert1.vertex.x) / (vert2.vertex.y - vert1.vertex.y)
        else 0
      val d2 =
        if (vert3.vertex.y - vert1.vertex.y > 0)
          (vert3.vertex.x - vert1.vertex.x) / (vert3.vertex.y - vert1.vertex.y)
        else 0

      if (d1 > d2) {
        for (y <- vert1.vertex.y.asInstanceOf[Int] to vert2.vertex.y.asInstanceOf[Int])
          line(y, vert1, vert3, vert1, vert2, scene)
        for (y <- vert2.vertex.y.asInstanceOf[Int] until vert3.vertex.y.asInstanceOf[Int])
          line(y, vert3, vert1, vert3, vert2, scene)
      } else {
        for (y <- vert1.vertex.y.asInstanceOf[Int] to vert2.vertex.y.asInstanceOf[Int])
          line(y, vert1, vert2, vert1, vert3, scene)
        for (y <- vert2.vertex.y.asInstanceOf[Int] until vert3.vertex.y.asInstanceOf[Int])
          line(y, vert3, vert2, vert3, vert1, scene)
      }
    }
  def line(
    y: Int,
    vec1: Vertex,
    vec2: Vertex,
    vec3: Vertex,
    vec4: Vertex,
    scene: Scene
  ) {
    val gradientY12 = if (vec1.vertex.y != vec2.vertex.y)
      (y.asInstanceOf[Double] - vec1.vertex.y) / (vec2.vertex.y - vec1.vertex.y)
    else 1
    val gradientY34 = if (vec3.vertex.y != vec4.vertex.y)
      (y.asInstanceOf[Double] - vec3.vertex.y) / (vec4.vertex.y - vec3.vertex.y)
    else 1

    val startX       = interpolate(vec1.vertex.x, vec2.vertex.x, gradientY12).asInstanceOf[Int]
    val endX         = interpolate(vec3.vertex.x, vec4.vertex.x, gradientY34).asInstanceOf[Int]
    val startZ       = interpolate(vec1.vertex.z, vec2.vertex.z, gradientY12)
    val endZ         = interpolate(vec3.vertex.z, vec4.vertex.z, gradientY34)
    val startNormalX = interpolate(vec1.normal.x, vec2.normal.x, gradientY12)
    val endNormalX   = interpolate(vec3.normal.x, vec4.normal.x, gradientY34)
    val startNormalY = interpolate(vec1.normal.y, vec2.normal.y, gradientY12)
    val endNormalY   = interpolate(vec3.normal.y, vec4.normal.y, gradientY34)
    val startNormalZ = interpolate(vec1.normal.z, vec2.normal.z, gradientY12)
    val endNormalZ   = interpolate(vec3.normal.z, vec4.normal.z, gradientY34)
    val startXTex    = interpolate(vec1.texture.x, vec2.texture.x, gradientY12)
    val endXTex      = interpolate(vec3.texture.x, vec4.texture.x, gradientY34)
    val startYTex    = interpolate(vec1.texture.y, vec2.texture.y, gradientY12)
    val endYTex      = interpolate(vec3.texture.y, vec4.texture.y, gradientY34)

    for (x <- startX until endX) {
      val gradientX: Double = (x.asInstanceOf[Double] - startX) / (endX - startX)
      val normal = Vec3(
        interpolate(startNormalX, endNormalX, gradientX),
        interpolate(startNormalY, endNormalY, gradientX),
        interpolate(startNormalZ, endNormalZ, gradientX)
      )
      val z = interpolate(startZ, endZ, gradientX)
      val intensity = dotProduct(Vec3(0, 0.5, 0.7), normal)
      val xTex = interpolate(startXTex, endXTex, gradientX)
      val yTex = interpolate(startYTex, endYTex, gradientX)
    
      val color = deffuse.get(xTex, yTex)
      scene.dot(
        x,
        y,
        z,
        color.r * intensity,
        color.g * intensity,
        color.b * intensity,
        color.a * intensity
      )
    }
  }
}
object Obj {
  def apply(url: String): Future[Obj] = {
    for {
      obj <- get("obj/african_head/african_head.obj") map { _.body.split("\n").map( _.split(" ") ) } 
      deffuse <- Texture("obj/african_head/african_head_diffuse.jpg")
    } yield new Obj(
      parseV(obj),
      parseVN(obj),
      parseVT(obj),
      parseF(obj),
      deffuse
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
