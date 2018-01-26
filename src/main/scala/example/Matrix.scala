package tinyrenderer
import scala.math.{ sin, cos }
import Commone.{
    Row,
    Vec3
}
case class Matrix4x4(value: (Row, Row, Row, Row)){
    def *( other: Matrix4x4 ) = {
        Matrix4x4(
            (
                value._1._1 * other.value._1._1 + value._1._2 * other.value._2._1 + value._1._3 * other.value._3._1 + value._1._4 * other.value._4._1,
                value._1._1 * other.value._1._2 + value._1._2 * other.value._2._2 + value._1._3 * other.value._3._2 + value._1._4 * other.value._4._2,
                value._1._1 * other.value._1._3 + value._1._2 * other.value._2._3 + value._1._3 * other.value._3._3 + value._1._4 * other.value._4._3,
                value._1._1 * other.value._1._4 + value._1._2 * other.value._2._4 + value._1._3 * other.value._3._4 + value._1._4 * other.value._4._4
            ),
            (
                value._2._1 * other.value._1._1 + value._2._2 * other.value._2._1 + value._2._3 * other.value._3._1 + value._2._4 * other.value._4._1,
                value._2._1 * other.value._1._2 + value._2._2 * other.value._2._2 + value._2._3 * other.value._3._2 + value._2._4 * other.value._4._2,
                value._2._1 * other.value._1._3 + value._2._2 * other.value._2._3 + value._2._3 * other.value._3._3 + value._2._4 * other.value._4._3,
                value._2._1 * other.value._1._4 + value._2._2 * other.value._2._4 + value._2._3 * other.value._3._4 + value._2._4 * other.value._4._4
            ),
            (
                value._3._1 * other.value._1._1 + value._3._2 * other.value._2._1 + value._3._3 * other.value._3._1 + value._3._4 * other.value._4._1,
                value._3._1 * other.value._1._2 + value._3._2 * other.value._2._2 + value._3._3 * other.value._3._2 + value._3._4 * other.value._4._2,
                value._3._1 * other.value._1._3 + value._3._2 * other.value._2._3 + value._3._3 * other.value._3._3 + value._3._4 * other.value._4._3,
                value._3._1 * other.value._1._4 + value._3._2 * other.value._2._4 + value._3._3 * other.value._3._4 + value._3._4 * other.value._4._4
            ),
            (
                value._4._1 * other.value._1._1 + value._4._2 * other.value._2._1 + value._4._3 * other.value._3._1 + value._4._4 * other.value._4._1,
                value._4._1 * other.value._1._2 + value._4._2 * other.value._2._2 + value._4._3 * other.value._3._2 + value._4._4 * other.value._4._2,
                value._4._1 * other.value._1._3 + value._4._2 * other.value._2._3 + value._4._3 * other.value._3._3 + value._4._4 * other.value._4._3,
                value._4._1 * other.value._1._4 + value._4._2 * other.value._2._4 + value._4._3 * other.value._3._4 + value._4._4 * other.value._4._4
            )
        )
    }
    def * ( vec: Vec3 ) = {
        Vec3(
            vec.x * value._1._1 + vec.y * value._1._2 + vec.z * value._1._3 + value._1._4,
            vec.x * value._2._1 + vec.y * value._2._2 + vec.z * value._2._3 + value._2._4,
            vec.x * value._3._1 + vec.y * value._3._2 + vec.z * value._3._3 + value._3._4
        )
    }
    def * (f: Double) = Matrix4x4(
        (value._1._1 * f, value._1._2 * f, value._1._3 * f, value._1._4 * f),
        (value._2._1 * f, value._2._2 * f, value._2._3 * f, value._2._4 * f),
        (value._3._1 * f, value._3._2 * f, value._3._3 * f, value._3._4 * f),
        (value._4._1 * f, value._4._2 * f, value._4._3 * f, value._4._4 * f)
    )
    def transpose = {
        Matrix4x4(
            (value._1._1, value._2._1, value._3._1, value._4._1),
            (value._1._2, value._2._2, value._3._2, value._4._2),
            (value._1._3, value._2._3, value._3._3, value._4._3),
            (value._1._4, value._2._4, value._3._4, value._4._4)
        )
    }
  def invert = {
        val _11 = 
            value._2._2 * value._3._3 * value._4._4 - 
            value._2._2 * value._3._4 * value._4._3 - 
            value._3._2 * value._2._3 * value._4._4 +
            value._3._2 * value._2._4 * value._4._3 + 
            value._4._2 * value._2._3 * value._3._4 - 
            value._4._2 * value._2._4 * value._3._3
        val _21 = 
            -value._2._1 * value._3._3 * value._4._4 + 
            value._2._1 * value._3._4 * value._4._3 + 
            value._3._1 * value._2._3 * value._4._4 -
            value._3._1 * value._2._4 * value._4._3 - 
            value._4._1 * value._2._3 * value._3._4 + 
            value._4._1 * value._2._4 * value._3._3
        val _31 = 
            value._2._1 * value._3._2 * value._4._4 - 
            value._2._1 * value._3._4 * value._4._2 - 
            value._3._1 * value._2._2 * value._4._4 + 
            value._3._1 * value._2._4 * value._4._2 + 
            value._4._1 * value._2._2 * value._3._4 -
            value._4._1 * value._2._4 * value._3._2
        val _41 = 
            -value._2._1 * value._3._2 * value._4._3 + 
            value._2._1 * value._3._3 * value._4._2 + 
            value._3._1 * value._2._2 * value._4._3 -
            value._3._1 * value._2._3 * value._4._2 - 
            value._4._1 * value._2._2 * value._3._3 + 
            value._4._1 * value._2._3 * value._3._2
        val _12 = 
            -value._1._2 * value._3._3 * value._4._4 + 
            value._1._2 * value._3._4 * value._4._3 + 
            value._3._2 * value._1._3 * value. _4._4 -
            value._3._2 * value._1._4 * value._4._3 - 
            value._4._2 * value._1._3 * value._3._4 + 
            value._4._2 * value._1._4 * value._3._3
        val _22 = 
            value._1._1 * value._3._3 * value._4._4 - 
            value._1._1 * value._3._4 * value._4._3 -
            value._3._1 * value._1._3 * value._4._4 +
            value._3._1 * value._1._4 * value._4._3 + 
            value._4._1 * value._1._3 * value._3._4 - 
            value._4._1 * value._1._4 * value._3._3
        val _32 = 
            -value._1._1 * value._3._2 * value._4._4 + 
            value._1._1 * value._3._4 * value._4._2 + 
            value._3._1 * value._1._2 * value._4._4 -
            value._3._1 * value._1._4 * value._4._2 - 
            value._4._1 * value._1._2 * value._3._4 +
            value._4._1 * value._1._4 * value._3._2
        val _42 = 
            value._1._1 * value._3._2 * value._4._3 - 
            value._1._1 * value._3._3 * value._4._2 - 
            value._3._1 * value._1._2 * value._4._3 +
            value._3._1 * value._1._3 * value._4._2 + 
            value._4._1 * value._1._2 * value._3._3 - 
            value._4._1 * value._1._3 * value._3._2
        val _13 =
            value._1._2 * value._2._3 * value._4._4 - 
            value._1._2 * value._2._4 * value._4._3 - 
            value._2._2 * value._1._3 * value._4._4 +
            value._2._2 * value._1._4 * value._4._3 + 
            value._4._2 * value._1._3 * value._2._4 - 
            value._4._2 * value._1._4 * value._2._3
        val _23 = 
            -value._1._1 * value._2._3 * value._4._4 + 
            value._1._1 * value._2._4 * value._4._3 + 
            value._2._1 * value._1._3 * value._4._4 -
            value._2._1 * value._1._4 * value._4._3 - 
            value._4._1 * value._1._3 * value._2._4 + 
            value._4._1 * value._1._4 * value._2._3
        val _33 = 
            value._1._1 * value._2._2 * value._4._4 - 
            value._1._1 * value._2._4 * value._4._2 -
            value._2._1 * value._1._2 * value._4._4 +
            value._2._1 * value._1._4 * value._4._2 + 
            value._4._1 * value._1._2 * value._2._4 - 
            value._4._1 * value._1._4 * value._2._2
        val _43 = 
            -value._1._1 * value._2._2 * value._4._3 + 
            value._1._1 * value._2._3 * value._4._2 + 
            value._2._1 * value._1._2 * value._4._3 - 
            value._2._1 * value._1._3 * value._4._2 - 
            value._4._1 * value._1._2 * value._2._3 + 
            value._4._1 * value._1._3 * value._2._2
        val _14 = 
            -value._1._2 * value._2._3 * value._3._4 + 
            value._1._2 * value._2._4 * value._3._3 + 
            value._2._2 * value._1._3 * value._3._4 -
            value._2._2 * value._1._4 * value._3._3 - 
            value._3._2 * value._1._3 * value._2._4 + 
            value._3._2 * value._1._4 * value._2._3
        val _24 = 
            value._1._1 * value._2._3 * value._3._4 - 
            value._1._1 * value._2._4 * value._3._3 - 
            value._2._1 * value._1._3 * value._3._4 +
            value._2._1 * value._1._4 * value._3._3 + 
            value._3._1 * value._1._3 * value._2._4 - 
            value._3._1 * value._1._4 * value._2._3
        val _34 = 
            -value._1._1 * value._2._2 * value._3._4 + 
            value._1._1 * value._2._4 * value._3._2 + 
            value._2._1 * value._1._2 * value._3._4 -
            value._2._1 * value._1._4 * value._3._2 - 
            value._3._1 * value._1._2 * value._2._4 + 
            value._3._1 * value._1._4 * value._2._2
        val _44 = 
            value._1._1 * value._2._2 * value._3._3 - 
            value._1._1 * value._2._3 * value._3._2 - 
            value._2._1 * value._1._2 * value._3._3 +
            value._2._1 * value._1._3 * value._3._2 + 
            value._3._1 * value._1._2 * value._2._3 - 
            value._3._1 * value._1._3 * value._2._2

        val det = value._1._1 * _11 + value._1._2 * _21 + value._1._3 * _31 + value._1._4 * _41

        if (det == 0) 
            this
        else 
            Matrix4x4(
                (_11, _12, _13, _14),
                (_21, _22, _23, _24),
                (_31, _32, _33, _34),
                (_41, _42, _43, _44)
            ) * (1 / det)
    }
    def rotate(x: Double, y: Double, z: Double) = {
        this * 
        Matrix4x4(
            (
                (   1,     0 ,      0 ,   0),
                (   0, cos(x), -sin(x),   0),
                (   0, sin(x),  cos(x),   0),
                (   0,     0 ,      0 ,   1)
            )
        ) * 
        Matrix4x4(
            (
                (   cos(x) ,     0,  sin(x),   0),
                (        0 ,     1,      0 ,   0),
                (  -sin(x) ,     0,  cos(x),   0),
                (        0 ,     0,      0 ,   1)
            )
        ) *
        Matrix4x4(
            (
                (   cos(x) , -sin(x),      0,   0),
                (   sin(x) ,  cos(x),      0,   0),
                (        0 ,      0 ,      1,   0),
                (        0 ,      0 ,      0,   1)
            )
        )
    }
    override def toString = {
        s"""Matrix4x4(
            ${value._1}, 
            ${value._2},
            ${value._3},
            ${value._4} 
        )"""
    }
}
