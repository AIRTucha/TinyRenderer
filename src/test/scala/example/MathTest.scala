package tinyrenderer

import utest._

object MathTest extends TestSuite {

  import math._

  def tests = TestSuite {
    'square {
      assert(square(0) == 0)
      assert(square(4) == 16)
      assert(square(-5) == 25)
    }
  }
}
