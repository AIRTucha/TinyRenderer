package tinyrenderer

import fr.hmil.roshttp.HttpRequest
import monix.execution.Scheduler.Implicits.global
import scala.util.{Failure, Success}
import fr.hmil.roshttp.response.SimpleHttpResponse
import org.scalajs.dom

object Parser {
  def get( url: String ) = HttpRequest( s"${dom.window.location.href}/${url}" ).send   
}

// .onComplete({
//         case res:Success[SimpleHttpResponse] => println("ok")
//         case e: Failure[SimpleHttpResponse] => println("Huston, we got a problem!")
//       })