package aoc.day1

import zio.test.*
import zio.test.Assertion.*

object Day1Tests extends ZIOSpecDefault {
  val spec = suite("Day 1 Tests")(
    test("part 1 : sum of digits") {
      assertTrue(Part1.execute == 142)
    },
    test("part 2 : sum of digits") {
      assertTrue(Part2.execute == 281)
    }
  )
}
