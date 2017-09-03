package tinyrenderer

package object Commone {
  def square(x: Int): Int = x*x
  case class Color(r: Short, g: Short, b: Short, a: Short = 255.toShort )
  case class Vec3(x: Double, y: Double, z: Double = 0d)
}