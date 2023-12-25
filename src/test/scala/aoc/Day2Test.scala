package aoc.day2

import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

object Day2Tests extends ZIOSpecDefault {
  val spec = suite("Day 2 Tests")(
    test("cubes game") {
      Day2.execute.map(value => assertTrue(value == 0))
    }
  )
}
