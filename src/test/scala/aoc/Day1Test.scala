package aoc.day1

import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

object Day1Tests extends ZIOSpecDefault {
  val spec = suite("Day 1 Tests")(
    test("part 1 : sum of digits") {
      Part1.execute.map(obtained => assertTrue(obtained == 142))
    },
    test("part 2 : sum of digits") {
      Part2.execute.map(obtained => assertTrue(obtained == 281))
    }
  )
}
