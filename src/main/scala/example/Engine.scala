package tinyrenderer

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.ImageData
import scala.math.{floor, abs}
import Commone.{Vec3, Color}

class Engine(val canvas: Canvas) {
  canvas.width = 1000;
  canvas.height = 1000;
  val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  val data = ctx.getImageData(0,0, 1000, 1000)
  def draw( scene: Scene ) = ctx.putImageData(scene.img, 0, 0)
  def createScene = Scene(canvas.width, canvas.height, ctx.getImageData(0,0, 1000, 1000))
}
case class Scene( width: Int, height: Int, img: ImageData ) {
  val dataAmount = width * height * 4
  def clear = {
    var i = 0
    while(i < dataAmount) {
      img.data(i) = 0
      i += 1
    }
  }
  def dot(vec: Vec3, color: Color) = {
    val redIndex: Int = ( vec.y.asInstanceOf[Int] * width + vec.x.asInstanceOf[Int] ) * 4
    img.data( redIndex )     = color.r
    img.data( redIndex + 1 ) = color.g
    img.data( redIndex + 2 ) = color.b
    img.data( redIndex + 3 ) = color.a
  }
  def dot(x: Double, y: Double, color: Color) = {
    val redIndex: Int = ( y.asInstanceOf[Int] * width + x.asInstanceOf[Int] ) * 4
    img.data( redIndex )     = color.r
    img.data( redIndex + 1 ) = color.g
    img.data( redIndex + 2 ) = color.b
    img.data( redIndex + 3 ) = color.a
  }
  def line(vec1: Vec3, vec2: Vec3, color: Color) = {
    var x = vec1.x
    var y = vec1.y
    val dx = abs( vec2.x - vec1.x )
		val dy = abs( vec2.y - vec1.y ) 
		val sx = if(vec1.x < vec2.x) 1 else -1
		val sy = if(vec1.y < vec2.y) 1 else -1
		var e = dx - dy;
    do {
      dot( x, y, color )
        val e2 = 2 * e
        if( e2 > -dy ) {
          e -= dy
          x += sx
        }
        if( e2 < dx ) { 
          e += dx
          y += sy
        }
    } while ( !( x == vec2.x && y == vec2.y ) ) 
  }	
}
