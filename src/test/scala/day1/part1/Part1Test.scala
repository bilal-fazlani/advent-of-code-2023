package day1.part1

import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

object Part1Test extends ZIOSpecDefault {
  val spec = suite("Day 1 Part 1 Test")(
    test("sum of digits") {
      val input = ZStream
        .fromFileName("src/test/scala/day1/part1/input.txt")
        .via(ZPipeline.utfDecode)
        .via(ZPipeline.splitLines)

      val output = Part1.calculate(input)

      output.map(obtained => assertTrue(obtained == 142))
    }
  )
}
