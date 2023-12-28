package aoc
package day5

import MapLine.MapTitle
import MapElement.*

object Part1 extends Challenge(day(5)):
  def execute: Int =
    val initialSeeds = Seeds.parse(input.head)
    val locations = initialSeeds.values.map(seed => Mapping.resolve(Seed, Location, seed)(mappings))
    locations.min

  def mappings = {
    val lines = input.tail.map(MapLine.parse)
    val state = lines
      .foldLeft[State](State.empty) {
        case (state @ State(mappings, Some(prevTitle), values), newTitle: MapLine.MapTitle) => // TITLE LINE WHEN THERE WAS A TITLE SET
          // move in progress to done
          val newMapping = Mapping(prevTitle.from, prevTitle.to, values)
          State(mappings :+ newMapping, Some(newTitle), Seq.empty)
        case (state @ State(mappings, None, _), newTitle: MapLine.MapTitle) => // TITLE LINE WHEN THERE WAS NO TITLE SET
          // set current in progress
          State(mappings, Some(newTitle), Seq.empty)
        case (state @ State(mappings, _, values), value: MapLine.MapValue) => //VALUE LINE
          // add value to in progress
          state.copy(wip = values :+ value)
      }
    val newMapping = Mapping(state.currentMap.get.from, state.currentMap.get.to, state.wip)
    state.mappings :+ newMapping
  }

  case class State(mappings: Seq[Mapping], currentMap: Option[MapTitle], wip: Seq[MapLine.MapValue])
  object State:
    val empty = State(Seq.empty, None, Seq.empty)
