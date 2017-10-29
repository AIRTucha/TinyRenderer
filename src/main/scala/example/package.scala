package tinyrenderer

import scala.scalajs.js.typedarray.Uint8ClampedArray
import org.scalajs.dom
import scala.math.{ cos, sin, min, max, pow, sqrt }
import scala.inline

package object Commone {
  def square(x: Int): Int = x * x
  case class Color(r: Int, g: Int, b: Int, a: Int = 255 )
  case class Vec3(x: Double, y: Double, z: Double = 0)
  case class Vec2(x: Double, y: Double)
  case class Indeces(vertex: Int, texture: Int, normal: Int)
  trait Position {
    val vertex: Vec3
  }
  case class Vertex( vertex: Vec3, normal: Vec3, texture: Vec2 ) extends Position
  @inline
  def interpolate( minV: Double, maxV: Double, gradient: Double) = {
			minV + ( maxV - minV ) * clamp( gradient )
	}
  @inline
  def clamp( value: Double ): Double = {
		 max( 0, min( value, 1 ) ) 
	}
  @inline
  def dotProduct(vec1: Vec3, vec2: Vec3) = {
    vec1.x * vec2.x + vec1.y * vec2.y + vec1.z * vec2.z;
  }
  @inline
  def normalize(vec: Vec3) = {
      val lenght = sqrt( pow( vec.x, 2 ) + pow( vec.y, 2 ) + pow( vec.z, 2 ) )
			Vec3( vec.x/lenght, vec.y/lenght, vec.z/lenght )
  }
  @inline
  def rotationY(v: Vec3, angle: Double): Vec3 = {
			Vec3( 
        -cos( angle ) * v.x - sin( angle ) * v.z ,
        v.y,
        v.z * cos( angle ) + v.x * sin( angle )
      )
		}
  @inline
  def crossProduct(vec1: Vec3, vec2: Vec3, vec3: Vec3, vec4: Vec3): Vec3 = {
      val vec12 = Vec3( vec1.x - vec2.x, vec1.y - vec2.y, vec1.z - vec2.z )
      val vec34 = Vec3( vec3.x - vec4.x, vec3.y - vec4.y, vec3.z - vec4.z )
			Vec3(
        vec12.y * vec34.z - vec34.y * vec12.z,
        -( vec12.x * vec34.z - vec34.x * vec12.z ),
        vec12.x * vec34.y - vec12.y * vec34.x
      )
		}
  @inline
  def crossProduct(vec1: Vec3, vec2: Vec3): Vec3 = {
			Vec3(
        vec1.y * vec2.z - vec2.y * vec1.z,
        -( vec1.x * vec2.z - vec2.x * vec1.z ),
        vec1.x * vec2.y - vec1.y * vec2.x
      )
		}
}