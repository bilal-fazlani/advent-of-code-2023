package aoc.day4

import zio.*
import zio.test.*
import zio.test.Assertion.*

object Day4Tests extends ZIOSpecDefault {
  import Syntax.*
  val spec = suite("Day 4 Parsing Tests")(
    test("parse card id") {
      assertTrue(cardId.parseString("Card 1: ") == Right(1))
    },
    test("parse number set") {
      assertTrue(numberSet.parseString("41 48 83 86 17") == Right(Chunk(41, 48, 83, 86, 17)))
    },
    test("parse card") {
      assertTrue(
        card.parseString("Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1") ==
          Right(Card(3, Chunk(1, 21, 53, 59, 44), Chunk(69, 82, 63, 72, 16, 21, 14, 1)))
      )
    }
  ) + suite("Day 4 Tests")(
    test("part 1 : winning number points") {
      assertCompletes
    }
  )
}
