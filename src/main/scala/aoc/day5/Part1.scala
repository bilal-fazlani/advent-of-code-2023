package aoc
package day5

import MapLine.MapTitle
import MapElement.*
import aoc.day5.Syntax.mapping

object Part1 extends Challenge(day(5)):
  def execute: Long =
    val initialSeeds = Seeds.parse(input.head)
    val locations = initialSeeds.values.map(seed => Mapping.resolve(Seed, Location, seed)(mappings))
    locations.min

  def mappings = {
    val lines = input.tail.map(MapLine.parse)
    val state = lines
      .foldRight[State](State.empty) {
        case (title: MapLine.MapTitle, State(mappings, wip)) => // TITLE LINE
          // move in progress to done
          val newMapping = Mapping(title.from, title.to, wip)
          State(mappings.prepended(newMapping), Seq.empty)
        case (value: MapLine.MapValue, state @ State(mappings, wip)) => // VALUE LINE
          // add value to in progress
          state.copy(wip = wip :+ value)
      }
    state.mappings
  }

  case class State(mappings: Seq[Mapping], wip: Seq[MapLine.MapValue])
  object State:
    val empty = State(Seq.empty, Seq.empty)
