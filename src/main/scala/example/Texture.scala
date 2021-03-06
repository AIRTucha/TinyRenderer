package tinyrenderer

import scala.concurrent.{Future, Promise}
import monix.execution.Scheduler.Implicits.global
import scala.scalajs.js.typedarray.Uint8ClampedArray
import Commone.{ Color, Vec3 }
import org.scalajs.dom
//"obj/african_head/african_head_diffuse.jpg"
class Texture(
    private val data: Uint8ClampedArray,
    val width: Int,
    val height: Int
) {
  def getColor(x: Double, y: Double) = {
    val redIndex: Int = (
      width * ( height * ( 1 - y ) ).asInstanceOf[Int] + 
      ( width * x ).asInstanceOf[Int]
    ) * 4
    Color(
      data(redIndex).asInstanceOf[Short],
      data(redIndex + 1).asInstanceOf[Short],
      data(redIndex + 2).asInstanceOf[Short],
      data(redIndex + 3).asInstanceOf[Short]
    )
  }
  def getVec3(x: Double, y: Double) = {
    val redIndex: Int = (
      width * ( height * ( 1 - y ) ).asInstanceOf[Int] + 
      ( width * x ).asInstanceOf[Int]
    ) * 4
    Vec3(
      data(redIndex).asInstanceOf[Short],
      data(redIndex + 1).asInstanceOf[Short],
      data(redIndex + 2).asInstanceOf[Short]
    )
  }
  //TODO: implement interpolation
  def getSmooth(x: Double, y: Double) = {
    val redIndex: Int = ((width * y).asInstanceOf[Int] + x
      .asInstanceOf[Int]) * 4
    Color(
      data(redIndex).asInstanceOf[Short],
      data(redIndex + 1).asInstanceOf[Short],
      data(redIndex + 2).asInstanceOf[Short],
      data(redIndex + 3).asInstanceOf[Short]
    )
  }
}
object Texture {
  val img = dom.document.createElement("img").asInstanceOf[dom.html.Image]
  def apply(url: String): Future[Texture] = {
    val promise = Promise[Texture]()
    img.src = url
    img.onload = (e: dom.Event) => {
      val canvas =
        dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
      canvas.width = img.width
      canvas.height = img.height
      val ctx = canvas
        .getContext("2d")
      ctx.drawImage(img, 0, 0, img.width, img.height)
      promise success new Texture(
        ctx
          .getImageData(0, 0, img.width, img.height)
          .data
          .asInstanceOf[Uint8ClampedArray],
        img.width,
        img.height
      )
    }
    promise.future
  }
}
