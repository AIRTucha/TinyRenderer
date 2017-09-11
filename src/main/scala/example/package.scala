package tinyrenderer

import scala.math.{ cos, sin, min, max, pow, sqrt }
import scala.inline

package object Commone {
  def square(x: Int): Int = x * x
  case class Color(r: Short, g: Short, b: Short, a: Short = 255.toShort )
  case class Vec3(x: Double, y: Double, z: Double = 0)
  case class Vertex(vertex: Int, texture: Int, normal: Int)
  case class Obj(vertices: Array[Vec3], normals: Array[Vec3], faces: Array[( Vertex, Vertex, Vertex )] )
  case class Vert( vertex: Vec3, normal: Vec3 )
  @inline
  def interpolate( minV: Double, maxV: Double, gradient: Double) = {
			minV + ( maxV - minV ) * clamp( gradient )
	}
  @inline
  def clamp( value: Double ): Double = {
		 max( 0, min( value, 1 ) ) 
	}
  def dotProduct(vec1: Vec3, vec2: Vec3) = {
    vec1.x * vec2.x + vec1.y * vec2.y + vec1.z * vec2.z;
  }
  def normalize(vec: Vec3) = {
      val lenght = sqrt( pow( vec.x, 2 ) + pow( vec.y, 2 ) + pow( vec.z, 2 ) )
			Vec3( vec.x/lenght, vec.y/lenght, vec.z/lenght )
  }
  def rotationY(v: Vec3, angle: Double): Vec3= {
			Vec3( 
        -cos( angle ) * v.x - sin( angle ) * v.z ,
        v.y,
        v.z * cos( angle ) + v.x * sin( angle )
      )
		}
}