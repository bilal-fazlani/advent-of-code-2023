package day1

import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

object Day1Test extends ZIOSpecDefault {
  val spec = suite("Day1Test")(
    test("part 1: sum of digits") {
      val input = ZStream
        .fromFileName("day-1-trebuchet/src/test/input.txt")
        .via(ZPipeline.utfDecode)
        .via(ZPipeline.splitLines)

      val output = Part1.calculate(input)

      output.map(obtained => assertTrue(obtained == 142))
    } @@ TestAspect.timeout(10.seconds)
  )
}
