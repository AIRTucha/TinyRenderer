package tinyrenderer

import scala.concurrent.{ Future, Promise }
import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.typedarray.Uint8ClampedArray
import Commone.Color
import org.scalajs.dom

class Texture(val data: Uint8ClampedArray, val width: Int, val height: Int) {
    def get(x: Double, y: Double) = {
        val redIndex: Int = ( (width * y).asInstanceOf[Int] + x.asInstanceOf[Int] ) * 4 
        Color (
            data(redIndex).asInstanceOf[Short],
            data(redIndex + 1).asInstanceOf[Short],
            data(redIndex + 2).asInstanceOf[Short],
            data(redIndex + 3).asInstanceOf[Short]
        )
    }
    def getSmooth(x: Double, y: Double) = {
        val redIndex: Int = ( (width * y).asInstanceOf[Int] + x.asInstanceOf[Int] ) * 4 
        Color (
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
            val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
            canvas.width = img.width
            canvas.height = img.height
            val ctx = canvas
                .getContext("2d")
                ctx.drawImage(img, 0, 0, img.width, img.height)
            promise success new Texture( 
                ctx.getImageData( 0, 0, img.width, img.height).data.asInstanceOf[Uint8ClampedArray],
                img.width,
                img.height
            )
        }
        promise.future
    }
} 