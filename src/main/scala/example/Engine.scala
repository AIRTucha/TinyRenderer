package tinyrenderer

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.ImageData
import scala.math.floor
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
  def line(vec1: Vec3, vec2: Vec3, color: Color) = {
    
  }
}
