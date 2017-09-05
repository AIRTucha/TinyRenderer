package tinyrenderer

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.ImageData
import scala.math.{floor, abs}
import Commone.{Vec3, Color}

class Engine( val canvas: Canvas ) {
  canvas.width = 1000;
  canvas.height = 1000;
  val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  val data = ctx.getImageData(0,0, 1000, 1000)
  def draw( scene: Scene ) = ctx.putImageData(scene.img, 0, 0)
  def createScene( low: Vec3, high: Vec3) = Scene(canvas.width, canvas.height, low, high, ctx.getImageData(0,0, canvas.width, canvas.height))
}
case class Scene( width: Int, height: Int, val low: Vec3, val high: Vec3, img: ImageData ) {
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
}
