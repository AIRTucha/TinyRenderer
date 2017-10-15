package tinyrenderer

import org.scalajs.dom.CanvasRenderingContext2D
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.ImageData
import Commone.{ Vec3, Color }
import scala.collection.mutable.ListBuffer

class Engine(val canvas: Canvas) {
  canvas.width = 2500;
  canvas.height = 2500;
  private val ctx = canvas.getContext("2d").asInstanceOf[CanvasRenderingContext2D]
  def render(scene: Scene) = ctx.putImageData(scene.getImg, 0, 0)
  def Scene(low: Vec3, high: Vec3) = new Scene(
    canvas.width,
    canvas.height,
    low,
    high,
    ctx.getImageData(0, 0, canvas.width, canvas.height)
  )
}
class Scene(
  val width: Int,
  val height: Int,
  val low: Vec3,
  val high: Vec3,
  private val img: ImageData
) {
  val zBuffer: Array[Array[Double]] = Array.fill(height, width)(-2)
  val lights: ListBuffer[Vec3] = ListBuffer()
  val dataAmount = width * height * 4
  def clear = {
    var i = 0
    while (i < dataAmount) {
      img.data(i) = 0
      img.data(i + 1) = 0
      img.data(i + 2) = 0
      img.data(i + 3) = 255
      i += 4
    }
  }
  def dot(
    x: Int,
    y: Int,
    z: Double,
    r: Double,
    g: Double,
    b: Double,
    a: Double
  ) = if (zBuffer(x)(y) < z) {
      val redIndex: Int = (width * y + x) * 4
      img.data(redIndex) = r.asInstanceOf[Short]
      img.data(redIndex + 1) = g.asInstanceOf[Short]
      img.data(redIndex + 2) = b.asInstanceOf[Short]
      img.data(redIndex + 3) = 255 //a.asInstanceOf[Short]
      zBuffer(x)(y) = z
    }
  def getImg = img
  def scale( vec: Vec3 ) = Vec3(
    ( width * ( vec.x - low.x ) / ( high.x - low.x ) ),
    ( height * ( vec.y - low.y ) / ( high.y - low.y ) ),
    vec.z
  )
}
