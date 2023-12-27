package aoc.day4

import zio.test.*
import zio.test.Assertion.*

object Day4Tests extends ZIOSpecDefault {
  import Syntax.*
  val spec = suite("Day 4 Parsing Tests")(
    test("parse card id") {
      assertTrue(cardId.parseString("Card 1: ") == Right(1))
    },
    test("parse number set") {
      assertTrue(numberSet.parseString("41 48 83 86 17") == Right(Set(41, 48, 83, 86, 17)))
    },
    test("parse card") {
      assertTrue(
        card.parseString("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1") ==
          Right(Card(3, Set(1, 21, 53, 59, 44), Set(69, 82, 63, 72, 16, 21, 14, 1)))
      )
    }
  ) + suite("Day 4 Tests")(
    test("part 1 : calculate points for 0 matches") {
      val set1 = Set(1)
      val set2 = Set(2)
      val output = Part1.calculatePoints(Card(1, set1, set2))
      assertTrue(output == 0)
    },
    test("part 1 : calculate points for 1 match") {
      val set1 = Set(1)
      val set2 = Set(1)
      val output = Part1.calculatePoints(Card(1, set1, set2))
      assertTrue(output == 1)
    },
    test("part 1 : calculate points for 2 matches") {
      val set1 = Set(1, 2)
      val set2 = Set(1, 2)
      val output = Part1.calculatePoints(Card(1, set1, set2))
      assertTrue(output == 2)
    },
    test("part 1 : calculate points for 3 matches") {
      val set1 = Set(1, 2, 3)
      val set2 = Set(1, 2, 3)
      val output = Part1.calculatePoints(Card(1, set1, set2))
      assertTrue(output == 4)
    },
    test("part 1 : calculate points for 4 matches") {
      val set1 = Set(1, 2, 3, 4)
      val set2 = Set(1, 2, 3, 4)
      val output = Part1.calculatePoints(Card(1, set1, set2))
      assertTrue(output == 8)
    },
    test("part 1 : winning number points") {
      assertTrue(Part1.execute == 13)
    },
    test("part 2 : pile of scratch cards") {
      assertTrue(Part2.execute == 30)
    }
  )
}
