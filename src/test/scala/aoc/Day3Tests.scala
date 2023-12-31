package aoc.day3

import zio.test.*
import zio.test.Assertion.*

object Day3Tests extends ZIOSpecDefault {
  val spec = suite("Day 3 Parsing Tests")(
    test("parse digit cell") {
      assertTrue(Cell.parse('1') == Cell.Digit('1'))
    },
    test("parse empty cell") {
      assertTrue(Cell.parse('.') == Cell.Empty)
    },
    test("parse gear symbol") {
      assertTrue(Cell.parse('*') == Cell.StarSymbol)
    },
    test("parse special char cell") {
      assertTrue(Cell.parse('$') == Cell.SpecialChar)
    },
    test("read line") {
      assertTrue(
        Part1.readLine("1..*a") == List(
          Cell.Digit('1'),
          Cell.Empty,
          Cell.Empty,
          Cell.StarSymbol,
          Cell.SpecialChar
        )
      )
    }
  ) + suite("Day 3 Tests")(
    test("sum of part numbers") {
      assertTrue(Part1.execute == 4361)
    },
    test("sum of gear ratios") {
      assertTrue(Part2.execute == 467835)
    }
  )
}
