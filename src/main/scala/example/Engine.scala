package tinyrenderer

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.ImageData
import scala.math.{floor, abs, pow}
import Commone.{Vec3, Color, interpolate}

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
  def dot( x: Int, y: Int, color: Color ) = {
    val redIndex: Int = ( width * y + x ) * 4
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
  def triangle(vec1: Vec3, vec2: Vec3, vec3: Vec3, color: Color) = {
    var vert1 = Vec3(
      (width * ( vec1.x - low.x ) / ( high.x - low.x )),
      (height * ( vec1.y - low.y ) / ( high.y - low.y ))
    )
    var vert2 = Vec3(
      (width * ( vec2.x - low.x ) / ( high.x - low.x )),
      (height * ( vec2.y - low.y ) / ( high.y - low.y ))
    )
    var vert3 = Vec3(
      (width * ( vec3.x - low.x ) / ( high.x - low.x )),
      (height * ( vec3.y - low.y ) / ( high.y - low.y ))
    )

    if ( vert1.y > vert2.y ) {
      val buff = vert1
      vert1 = vert2
      vert2 = buff
    } 
    if ( vert2.y > vert3.y ) {
      val buff = vert2
      vert2 = vert3
      vert3 = buff
    }
    if ( vert1.y > vert2.y ) {
      val buff = vert1
      vert1 = vert2
      vert2 = buff
    }  

    println(vert1)
    println(vert2)
    println(vert3)
     
    val (k12, slope12) = lfCoffs( vert1, vert2 ) 
    val (k13, slope13) = lfCoffs( vert1, vert3 )
    val (k23, slope23) = lfCoffs( vert2, vert3 )

		if ( vert2.x > ( ( vert2.y + k13 ) / slope13 ) ) {
      for( y <- vert1.y .asInstanceOf[Int] to vert2.y.asInstanceOf[Int] )
        lineByLF( y, slope13, slope12, k13, k12, color ) 
      for( y <- vert2.y.asInstanceOf[Int] until vert3.y.asInstanceOf[Int] )
        lineByLF( y, slope13, slope23, k13, k23, color ) 
    } else {
      for( y <- vert1.y.asInstanceOf[Int] to vert2.y.asInstanceOf[Int] ) 
        lineByLF( y, slope12, slope13, k12, k13, color )
      for( y <- vert2.y.asInstanceOf[Int] until vert3.y.asInstanceOf[Int] )
        lineByLF( y, slope23, slope13, k23, k13, color ) 
    }
	}
  def lfCoffs( vec1: Vec3, vec2: Vec3 ): (Double, Double) =
    if( vec1.x == vec2.x ) {
        ( 500, 1 )
    } else if( vec1.y == vec2.y ) {
        ( vec1.x, 1 )
    }
    else {
      val slope = ( vec1.y - vec2.y ) / ( vec1.x - vec2.x )
      (slope * vec1.x - vec1.y, slope)
    }
  def lineByLF( y: Int, slope1: Double, slope2: Double, k1: Double, k2: Double, color: Color ) {
    println(k1 + " " + slope1 + " " + k2 + " " + slope2)
    println(( ( y + k1 ) / slope1 ).asInstanceOf[Int] until ( ( y + k2 ) / slope2 ).asInstanceOf[Int])
    for {
      x <- ( ( y + k1 ) / slope1 ).asInstanceOf[Int] until ( ( y + k2 ) / slope2 ).asInstanceOf[Int]
    } dot(x, y, color)
  }
}

