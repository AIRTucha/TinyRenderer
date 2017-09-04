package tinyrenderer

package object Commone {
  def square(x: Int): Int = x*x
  case class Color(r: Short, g: Short, b: Short, a: Short = 255.toShort )
  case class Vec3(x: Double, y: Double, z: Double = 0d)
  case class Face(vertex: Int, normal: Int, texture: Int)
  case class Obj(vertices: Array[Vec3], normals: Array[Vec3], fases: Array[( Face, Face, Face )] )
}