package tinyrenderer

import scala.concurrent.{Future, Promise}
import Commone.{Vec3, Vec2, Color, Vertex, Vert, rotationY}
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
  Vert,
  normalize,
  crossProduct
}

class Obj(
    val vertices: Array[Vec3],
    val normals: Array[Vec3],
    val textures: Array[Vec2],
    val faces: Array[(Vertex, Vertex, Vertex)]
) {
  def draw(scene: Scene) = {
    val color = Color(255.toShort, 255.toShort, 255.toShort)
    for ((fst, snd, trd) <- faces) {
      triangle(
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
        color,
        scene
      )
    }
  }
  def triangle(vec1: Vert, vec2: Vert, vec3: Vert, color: Color, scene: Scene) =
    if (
      dotProduct(Vec3(0, 0, 1), vec1.normal) > 0 ||
      dotProduct(Vec3(0, 0, 1), vec2.normal) > 0 ||
      dotProduct(Vec3(0, 0, 1), vec3.normal) > 0
    ) {
      var vert1 = Vert(
        Vec3(
          (scene.width * (vec1.vertex.x - scene.low.x) / (scene.high.x - scene.low.x)),
          (scene.height * (vec1.vertex.y - scene.low.y) / (scene.high.y - scene.low.y)),
          vec1.vertex.z
        ),
        vec1.normal,
        vec1.texture
      )
      var vert2 = Vert(
        Vec3(
          (scene.width * (vec2.vertex.x - scene.low.x) / (scene.high.x - scene.low.x)),
          (scene.height * (vec2.vertex.y - scene.low.y) / (scene.high.y - scene.low.y)),
          vec2.vertex.z
        ),
        vec2.normal,
        vec2.texture
      )
      var vert3 = Vert(
        Vec3(
          (scene.width * (vec3.vertex.x - scene.low.x) / (scene.high.x - scene.low.x)),
          (scene.height * (vec3.vertex.y - scene.low.y) / (scene.high.y - scene.low.y)),
          vec3.vertex.z
        ),
        vec3.normal,
        vec3.texture
      )
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
          line(y, vert1, vert3, vert1, vert2, color, scene)
        for (y <- vert2.vertex.y.asInstanceOf[Int] until vert3.vertex.y.asInstanceOf[Int])
          line(y, vert3, vert1, vert3, vert2, color, scene)
      } else {
        for (y <- vert1.vertex.y.asInstanceOf[Int] to vert2.vertex.y.asInstanceOf[Int])
          line(y, vert1, vert2, vert1, vert3, color, scene)
        for (y <- vert2.vertex.y.asInstanceOf[Int] until vert3.vertex.y.asInstanceOf[Int])
          line(y, vert3, vert2, vert3, vert1, color, scene)
      }
    }
  def line(
    y: Int,
    vec1: Vert,
    vec2: Vert,
    vec3: Vert,
    vec4: Vert,
    color: Color,
    scene: Scene
  ) {
    val gradientY1 = if (vec1.vertex.y != vec2.vertex.y)
      (y.asInstanceOf[Double] - vec1.vertex.y) / (vec2.vertex.y - vec1.vertex.y)
    else 1
    val gradientY2 = if (vec3.vertex.y != vec4.vertex.y)
      (y.asInstanceOf[Double] - vec3.vertex.y) / (vec4.vertex.y - vec3.vertex.y)
    else 1

    val startX =
      interpolate(vec1.vertex.x, vec2.vertex.x, gradientY1).asInstanceOf[Int]
    val endX =
      interpolate(vec3.vertex.x, vec4.vertex.x, gradientY2).asInstanceOf[Int]

    val startNormalX = interpolate(vec1.normal.x, vec2.normal.x, gradientY1)
    val endNormalX = interpolate(vec3.normal.x, vec4.normal.x, gradientY2)
    val startNormalY = interpolate(vec1.normal.y, vec2.normal.y, gradientY1)
    val endNormalY = interpolate(vec3.normal.y, vec4.normal.y, gradientY2)
    val startNormalZ = interpolate(vec1.normal.z, vec2.normal.z, gradientY1)
    val endNormalZ = interpolate(vec3.normal.z, vec4.normal.z, gradientY2)

    val startZ = interpolate(vec1.vertex.z, vec2.vertex.z, gradientY1)
    val endZ = interpolate(vec3.vertex.z, vec4.vertex.z, gradientY2)

    for (x <- startX until endX) {
      val gradientX: Double = (x
        .asInstanceOf[Double] - startX) / (endX - startX)
      val normal = Vec3(
        interpolate(startNormalX, endNormalX, gradientX),
        interpolate(startNormalY, endNormalY, gradientX),
        interpolate(startNormalZ, endNormalZ, gradientX)
      )
      val z = interpolate(startZ, endZ, gradientX)
      val intensity = dotProduct(Vec3(0, 0.5, 0.7), normal)
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
    get("obj/african_head/african_head.obj") map { res: SimpleHttpResponse =>
      val values = res.body.split("\n").map(str => str.split(" "))
      new Obj(
        values.withFilter(_(0) == "v") map { value =>
          Vec3(
            value(1).toDouble,
            value(2).toDouble,
            value(3).toDouble
          )
        },
        values.withFilter(_(0) == "vn") map { value =>
          Vec3(
            value(2).toDouble,
            value(3).toDouble,
            value(4).toDouble
          )
        },
        values.withFilter(_(0) == "vt") map { value =>
          Vec2(
            value(2).toDouble,
            value(3).toDouble
          )
        },
        values
          .withFilter(_(0) == "f")
          .map { 
            value => (
              value(1).split("/") match {
                case Array(fst, snd, trd) =>
                  Vertex(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1)
              },
              value(2).split("/") match {
                case Array(fst, snd, trd) =>
                  Vertex(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1)
              },
              value(3).split("/") match {
                case Array(fst, snd, trd) =>
                  Vertex(fst.toInt - 1, snd.toInt - 1, trd.toInt - 1)
              }
            )
          }
      )
    }
  }
  def get(url: String) = HttpRequest(s"${dom.window.location.href}/${url}").send
}
