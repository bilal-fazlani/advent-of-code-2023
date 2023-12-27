package aoc.day2

import zio.test.*
import zio.test.Assertion.*

object Day2Tests extends ZIOSpecDefault {
  val spec = suite("Day 2 Syntax Tests")(
    test("parse color") {
      import GameSyntax.*
      val input = "red"
      val expected = Color.Red
      val actual = color.parseString(input)
      assertTrue(actual == Right(expected))
    },
    test("parse number") {
      import GameSyntax.*
      val input = "34"
      val expected = 34
      val actual = number.parseString(input)
      assertTrue(actual == Right(expected))
    },
    test("parse cube count") {
      import GameSyntax.*
      val input = "34 red"
      val expected = CubeCount(34, Color.Red)
      val actual = cubeCount.parseString(input)
      assertTrue(actual == Right(expected))
    },
    test("parse reveal") {
      import GameSyntax.*
      val input = "34 red, 12 blue"
      val expected =
        Reveal(Set(CubeCount(34, Color.Red), CubeCount(12, Color.Blue)))
      val actual = reveal.parseString(input)
      assertTrue(actual == Right(expected))
    },
    test("parse game") {
      import GameSyntax.*
      val input = "Game 12: 34 red, 12 blue; 12 green, 34 red; 2 red"
      val expected = Game(
        12,
        Seq(
          Reveal(Set(CubeCount(34, Color.Red), CubeCount(12, Color.Blue))),
          Reveal(Set(CubeCount(12, Color.Green), CubeCount(34, Color.Red))),
          Reveal(Set(CubeCount(2, Color.Red)))
        )
      )
      val actual = game.parseString(input)
      assertTrue(actual == Right(expected))
    }
  ) + suite("Day 2 Tests")(
    test("part 1 : possible games") {
      assertTrue(Part1.execute == 8)
    },
    test("part 2 : lowest number of cubes") {
      assertTrue(Part2.execute == 2286)
    }
  )
}
