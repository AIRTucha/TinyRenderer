package tinyrenderer

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.ImageData
import scala.math.{floor, abs, pow}
import Commone.{Vec3, Color, interpolate, dotProduct, Vert, normalize, crossProduct}
import scala.collection.mutable.ListBuffer

class Engine( val canvas: Canvas ) {
  canvas.width = 1000;
  canvas.height = 1000;
  val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  def draw( scene: Scene ) = ctx.putImageData(scene.img, 0, 0)
  def createScene( low: Vec3, high: Vec3) = Scene(canvas.width, canvas.height, low, high, ctx.getImageData(0,0, canvas.width, canvas.height))
}
case class Scene( width: Int, height: Int, val low: Vec3, val high: Vec3, img: ImageData ) {
  val zBuffer: Array[Array[Double]] = Array.fill(height, width)(-2)
  val lights: ListBuffer[Vec3] = ListBuffer()
  val dataAmount = width * height * 4
  def clear = {
    var i = 0
    while(i < dataAmount) {
      img.data( i )     = 0
      img.data( i + 1 ) = 0
      img.data( i + 2 ) = 0
      img.data( i + 3 ) = 255
      i += 4
    }
  }
  def dot( vec: Vec3, color: Color ) = {
    val redIndex: Int = ( vec.y.asInstanceOf[Int] * width + vec.x.asInstanceOf[Int] ) * 4
    img.data( redIndex )     = color.r
    img.data( redIndex + 1 ) = color.g
    img.data( redIndex + 2 ) = color.b
    img.data( redIndex + 3 ) = color.a
  }
  def dot( x: Double, y: Double, color: Color ) = {
    val redIndex: Int = ( width * y + x).asInstanceOf[Int] * 4
    img.data( redIndex )     = color.r
    img.data( redIndex + 1 ) = color.g
    img.data( redIndex + 2 ) = color.b
    img.data( redIndex + 3 ) = color.a
  }
  def dot( x: Int, y: Int, z: Double, r: Double, g: Double, b: Double, a: Double ) = 
    if( zBuffer(x)(y) < z ) {
      val e = z * 255
      val redIndex: Int = ( width * y + x ) * 4 
      img.data( redIndex )     = e.asInstanceOf[Short]
      img.data( redIndex + 1 ) = e.asInstanceOf[Short]
      img.data( redIndex + 2 ) = e.asInstanceOf[Short]
      img.data( redIndex + 3 ) = 255//a.asInstanceOf[Short]
      zBuffer(x)(y) = z
    }
  
  def line( vec1: Vec3, vec2: Vec3, color: Color ) = {
    var x1 = (width * ( vec1.x - low.x ) / ( high.x - low.x )).asInstanceOf[Int]
    var y1 = (height * ( vec1.y - low.y ) / ( high.y - low.y )).asInstanceOf[Int] 
    val x2 = (width * ( vec2.x - low.x ) / ( high.x - low.x )).asInstanceOf[Int]
    val y2 = (height * ( vec2.y - low.y ) / ( high.y - low.y )).asInstanceOf[Int]
    val dx = abs( x2 - x1 )
		val dy = abs( y2 - y1 ) 
		val sx = if( x1 < x2 ) 1 else -1
		val sy = if( y1 < y2 ) 1 else -1
		var e = dx - dy;
    do {
      dot( x1, y1, color )
      if ( !( x1 == x2 && y1 == y2 ) ) {
        val e2 = 2 * e
        if( e2 > -dy ) {
          e -= dy
          x1 += sx
        }
        if( e2 < dx ) { 
          e += dx
          y1 += sy
        }
      }
    } while ( !( x1 == x2 && y1 == y1 ) ) 
  }
  def triangle(vec1: Vert, vec2: Vert, vec3: Vert, color: Color) = 
    // if(
    //   dotProduct( Vec3( 0, 0, 1), vec1.normal ) > 0 &&
    //   dotProduct( Vec3( 0, 0, 1), vec2.normal ) > 0 &&
    //   dotProduct( Vec3( 0, 0, 1), vec3.normal ) > 0
    // )
     {
      var vert1 = Vert(
        Vec3(
          (width * ( vec1.vertex.x - low.x ) / ( high.x - low.x )),
          (height * ( vec1.vertex.y - low.y ) / ( high.y - low.y )),
          vec1.vertex.z
        ),
        vec1.normal
      )
      var vert2 = Vert(
        Vec3(
          (width * ( vec2.vertex.x - low.x ) / ( high.x - low.x )),
          (height * ( vec2.vertex.y - low.y ) / ( high.y - low.y )),
          vec2.vertex.z
        ),
        vec2.normal
      )
      var vert3 = Vert(
        Vec3(
          (width * ( vec3.vertex.x - low.x ) / ( high.x - low.x )),
          (height * ( vec3.vertex.y - low.y ) / ( high.y - low.y )),
          vec3.vertex.z
        ),
        vec3.normal
      )
      if ( vert1.vertex.y > vert2.vertex.y ) {
        val buff = vert1
        vert1 = vert2
        vert2 = buff
      } 
      if ( vert2.vertex.y > vert3.vertex.y ) {
        val buff = vert2
        vert2 = vert3
        vert3 = buff
      } 
      if ( vert1.vertex.y > vert2.vertex.y ) {
        val buff = vert1
        vert1 = vert2
        vert2 = buff
      } 

      val d1 = if ( vert2.vertex.y - vert1.vertex.y > 0 ) ( vert2.vertex.x - vert1.vertex.x ) / ( vert2.vertex.y - vert1.vertex.y ) else 0 
      val d2 = if ( vert3.vertex.y - vert1.vertex.y > 0 ) ( vert3.vertex.x - vert1.vertex.x ) / ( vert3.vertex.y - vert1.vertex.y ) else 0

      if( d1 > d2) {
        for( y <- vert1.vertex.y.asInstanceOf[Int] to vert2.vertex.y.asInstanceOf[Int] )
            scanLine( y, vert1, vert3, vert1, vert2, color )
        for( y <- vert2.vertex.y.asInstanceOf[Int] until vert3.vertex.y.asInstanceOf[Int] )
            scanLine( y, vert3, vert1, vert3, vert2, color )
      } else {
        for( y <- vert1.vertex.y.asInstanceOf[Int] to vert2.vertex.y.asInstanceOf[Int] ) 
          scanLine( y, vert1, vert2, vert1, vert3, color )
        for( y <- vert2.vertex.y.asInstanceOf[Int] until vert3.vertex.y.asInstanceOf[Int] )
          scanLine( y, vert3, vert2, vert3, vert1, color )  
      }
    }
 	def scanLine( y: Int, vec1: Vert, vec2: Vert, vec3: Vert, vec4: Vert, color: Color ) {
			val gradientY1 = if( vec1.vertex.y != vec2.vertex.y ) ( y - vec1.vertex.y ) / ( vec2.vertex.y - vec1.vertex.y ) else 1
			val gradientY2 = if( vec3.vertex.y != vec4.vertex.y ) ( y - vec3.vertex.y ) / ( vec4.vertex.y - vec3.vertex.y ) else 1
 
			val startX =  interpolate ( vec1.vertex.x, vec2.vertex.x, gradientY1 ).asInstanceOf[Int]
			val endX =  interpolate ( vec3.vertex.x, vec4.vertex.x, gradientY2 ).asInstanceOf[Int]

      val startNormalX = interpolate ( vec1.normal.x, vec2.normal.x, gradientY1 )
      val endNormalX = interpolate ( vec3.normal.x, vec4.normal.x, gradientY2 )
      val startNormalY = interpolate ( vec1.normal.y, vec2.normal.y, gradientY1 )
      val endNormalY = interpolate ( vec3.normal.y, vec4.normal.y, gradientY2 )
      val startNormalZ = interpolate ( vec1.normal.z, vec2.normal.z, gradientY1 )
      val endNormalZ = interpolate ( vec3.normal.z, vec4.normal.z, gradientY2 )
      
      val startZ = interpolate ( vec1.vertex.z, vec2.vertex.z, gradientY1 ) 
      val endZ =  interpolate ( vec3.vertex.z, vec4.vertex.z, gradientY2 ) 
      
			for( x <- startX until endX ) {
				val gradientX = ( x - startX ) / ( endX - startX )
        val normal = Vec3 (
          interpolate( startNormalX, endNormalX, gradientX ), 
          interpolate( startNormalY, endNormalY, gradientX ), 
          interpolate( startNormalZ, endNormalZ, gradientX )
        ) 
        val z = interpolate( startZ, endZ, gradientX )
        val intensity = dotProduct( Vec3( 0, 0.5, 0.7), normal)
				dot( x, y, z, color.r * intensity, color.g * intensity, color.b * intensity, color.a * intensity)
			}
		}
}