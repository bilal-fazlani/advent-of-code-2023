package day1

import day1.MySuite.{suite, test}
import zio.*
import zio.test.*
import zio.test.Assertion.*

object MySuite extends ZIOSpecDefault {
  val spec = suite("MySuite")(
    test("example test that succeeds"){
      val obtained = 42
      val expected = 42
      assertTrue(obtained == expected)
    } @@ TestAspect.timeout(10.seconds) 
  ) @@ TestAspect.sequential
}
