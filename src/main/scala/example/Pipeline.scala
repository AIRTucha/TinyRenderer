package tinyrenderer
import scala.math.{floor, abs, pow, max, min}
import annotation.tailrec
import scala.{ Option, Some, None }
import Commone.{
  Vec2,
  Vec3,
  Color,
  Indeces,
  rotationY,
  interpolate,
  dotProduct,
  Vertex,
  Position,
  normalize,
  crossProduct
}
trait Pipeline[T <: Position, M <: Model[T]] {
  def vertexShader(vert: T, scene: Scene): Option[T]
  def pixelShader(x: Double, y: Double, obj: Obj ): ( Double, Double, Double, Double )
  def draw(obj: M, scene: Scene) = obj forEachPolygon triangle(scene, obj)
  def line(
    y: Int,
    vec1: T,
    vec2: T,
    vec3: T,
    vec4: T,
    scene: Scene,
    obj: M
  ): Unit
  def triangle(scene: Scene, obj: M)(vec1: T, vec2: T, vec3: T) = 
    for {
      vertex1 <- vertexShader(vec1, scene)
      vertex2 <- vertexShader(vec2, scene)
      vertex3 <- vertexShader(vec3, scene)
    } {
      rasterize(scene, obj, vertex1, vertex2, vertex3)
  }
  @tailrec
  private def rasterize(scene: Scene, obj: M, vert1: T, vert2: T, vert3: T): Unit = {
    if (vert1.vertex.y > vert2.vertex.y) 
      rasterize(scene, obj, vert2, vert1, vert3)
    else if(vert2.vertex.y > vert3.vertex.y)
      rasterize(scene, obj, vert1, vert3, vert2)
    else {
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
          line(y, vert1, vert3, vert1, vert2, scene, obj)
        for (y <- vert2.vertex.y.asInstanceOf[Int] until vert3.vertex.y.asInstanceOf[Int])
          line(y, vert3, vert1, vert3, vert2, scene, obj)
      } else {
        for (y <- vert1.vertex.y.asInstanceOf[Int] to vert2.vertex.y.asInstanceOf[Int])
          line(y, vert1, vert2, vert1, vert3, scene, obj)
        for (y <- vert2.vertex.y.asInstanceOf[Int] until vert3.vertex.y.asInstanceOf[Int])
          line(y, vert3, vert2, vert3, vert1, scene, obj)
      }
    }
  }
}
trait VertexPipeline extends Pipeline[Vertex, Obj] {
  def line(
    y: Int,
    vec1: Vertex,
    vec2: Vertex,
    vec3: Vertex,
    vec4: Vertex,
    scene: Scene,
    obj: Obj
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
    val startXTex    = interpolate(vec1.texture.x, vec2.texture.x, gradientY12)
    val endXTex      = interpolate(vec3.texture.x, vec4.texture.x, gradientY34)
    val startYTex    = interpolate(vec1.texture.y, vec2.texture.y, gradientY12)
    val endYTex      = interpolate(vec3.texture.y, vec4.texture.y, gradientY34)

    for (x <- startX until endX) {
      val gradientX: Double = (x.asInstanceOf[Double] - startX) / (endX - startX)

      val z = interpolate(startZ, endZ, gradientX)
      val xTex = interpolate(startXTex, endXTex, gradientX)
      val yTex = interpolate(startYTex, endYTex, gradientX)
      pixelShader( xTex, yTex, obj ) match { 
        case ( r, g, b, a ) => scene.dot( x, y, z, r, g, b, a )
      }
    }
  }
}
object ObjPipeline extends VertexPipeline {
  def vertexShader(vert: Vertex, scene: Scene) = 
      vert match { 
            case Vertex( vertex: Vec3, normal: Vec3, texture: Vec2 ) => 
              if( dotProduct(Vec3(0, 0, 1), normal) > 0 ) 
                Some ( 
                  Vertex(
                    scene scale vertex,
                    normal,
                    texture
                  )
                )
              else None
        }
  def pixelShader(x: Double, y: Double, obj: Obj ) = {
    val light = Vec3(0.65, 0.65, -0.15)
    val normal = normalize(obj.normalsTex.getVec3(x, y))
    val specularPow = obj.specular.getColor(x, y)
    val rPlusL = crossProduct( normal, crossProduct( normal, Vec3(-light.x*2, -light.y*2, light.z*2) ))
    val r = normalize(Vec3(rPlusL.x - light.x, rPlusL.y - light.y, rPlusL.z - light.z))
    val color = obj.deffuse.getColor( x, y)
    val spec = dotProduct(r, Vec3(0, 0, 1))
    val deffuseIntensity = dotProduct( light, normal )
    val g = pow(spec, specularPow.r)
    (
      color.r * min(deffuseIntensity + 0.3*abs(pow(spec, specularPow.r)), 1),
      color.g * min(deffuseIntensity + 0.3*abs(pow(spec, specularPow.g)), 1),
      color.b * min(deffuseIntensity + 0.3*abs(pow(spec, specularPow.b)), 1),
      color.a
    )
  } 
}

  