package tinyrenderer

import scala.math.{ min, max }
import scala.inline

package object Commone {
  def square(x: Int): Int = x*x
  case class Color(r: Short, g: Short, b: Short, a: Short = 255.toShort )
  case class Vec3(x: Double, y: Double, z: Double = 0)
  case class Vertex(vertex: Int, normal: Int, texture: Int)
  case class Obj(vertices: Array[Vec3], normals: Array[Vec3], faces: Array[( Vertex, Vertex, Vertex )] )
  @inline
  def interpolate( minV: Double, maxV: Double, gradient: Double) = {
			minV + ( maxV - minV ) * clamp( gradient )
	}
  @inline
  def clamp( value: Double ): Double = {
		 max( 0, min( value, 1 ) ) 
	}
}