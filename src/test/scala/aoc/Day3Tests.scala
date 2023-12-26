package aoc.day3

import zio.*
import zio.test.*
import zio.stream.*
import zio.test.Assertion.*

object Day3Tests extends ZIOSpecDefault {
  val spec = suite("Day 3 Parsing Tests")(
    test("parse digit cell") {
      assertTrue(Cell.parse('1') == Cell.Digit('1'))
    },
    test("parse empty cell") {
      assertTrue(Cell.parse('.') == Cell.Empty)
    },
    test("parse special char cell") {
      assertTrue(Cell.parse('*') == Cell.SpecialChar)
    },
    test("read line") {
      assertTrue(
        Part1.readLine("1..*a") == Chunk(
          Cell.Digit('1'),
          Cell.Empty,
          Cell.Empty,
          Cell.SpecialChar,
          Cell.SpecialChar
        )
      )
    }
  ) + suite("Day 3 Tests")(
    test("sum of part numbers"){
      Part1.execute.map(value => assertTrue(value == 4361))
    }
  )
}