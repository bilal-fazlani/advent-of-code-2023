package day1.part2

import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

object Part2Test extends ZIOSpecDefault {
  val spec = suite("Day 1 Part 2 Test")(
    test("sum of digits") {
      val input = ZStream
        .fromFileName("src/test/scala/day1/part2/input.txt")
        .via(ZPipeline.utfDecode)
        .via(ZPipeline.splitLines)

      val output = Part2.calculate(input)

      output.map(obtained => assertTrue(obtained == 142))
    } @@ TestAspect.timeout(10.seconds)
  )
}
