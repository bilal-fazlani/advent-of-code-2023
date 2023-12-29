package aoc.day5

import zio.test.*
import zio.test.Assertion.*
import MapElement.*
import scala.collection.MapView.MapValues
import aoc.day5.MapLine.MapValue
import aoc.readSync
import aoc.day

object Day5Tests extends ZIOSpecDefault {
  import Syntax.*
  val spec = suite("Day 5 Parsing Tests")(
    test("parse seeds") {
      assertTrue(Seeds.parse("seeds: 1 03 78 99") == Seeds(List(1, 3, 78, 99)))
    },
    test("parse map title") {
      assertTrue(MapLine.parse("seed-to-soil map:") == MapLine.MapTitle(MapElement.Seed, MapElement.Soil))
    },
    test("parse map value") {
      assertTrue(MapLine.parse("01 2 783") == MapLine.MapValue(1, 2, 783))
    }
  ) + suite("Day 5 Mapping Tests")(
    test("map construction") {
      val mapping = Mapping(
        Seed,
        Soil,
        Seq(
          MapValue(50, 98, 2),
          MapValue(52, 50, 48)
        )
      )

      assertTrue(mapping.get(79) == 81)
    },
    test("seed to soil mapping") {
      val lines = readSync(day(5))
      assertTrue(Mapping.resolve(Seed, Soil, 79)(Mapping.constructAll(lines.toList)) == 81)
    },
    test("part 1: min location") {
      assertTrue(Part1.execute == 35)
    }
    // test("part 2: min location") {
    //   Part2.execute.map(value => assertTrue(value == 46))
    // }
  )
}
