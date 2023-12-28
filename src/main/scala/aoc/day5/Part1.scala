package aoc
package day5

import MapLine.MapTitle
import MapElement.*
import aoc.day5.Syntax.mapping

object Part1 extends Challenge(day(5)):
  def execute: Long =
    val initialSeeds = Seeds.parse(input.head)
    val mappings = Mapping.constructAll(input.tail)
    val locations = initialSeeds.values.map(seed => Mapping.resolve(Seed, Location, seed)(mappings))
    locations.min
