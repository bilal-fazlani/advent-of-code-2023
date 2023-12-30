package aoc
package day5

import MapElement.*
import MapLine.MapValue

case class Mapping(from: MapElement, to: MapElement, initial: Seq[MapValue]):

  def get(source: Long): Long =
    val mapping = initial.find(x => source.between(x.sourceStart, x.sourceStart + x.length - 1))
    var default = false
    val result = mapping
      .map { m =>
        source - m.sourceStart + m.destinationStart
      }
      .getOrElse {
        default = true
        source
      }
    // println(s"mapping $from($source) -> $to($result)${if default then " (default)" else ""}")
    result

  override def toString(): String = s"$from -> $to"

object Mapping:
  def resolve(from: MapElement, to: MapElement, value: Long)(mappings: Seq[Mapping]): Long =
    val chain = Seq(Seed, Soil, Fertilizer, Water, Light, Temperature, Humidity, Location)
    val start = chain.indexOf(from)
    val end = chain.indexOf(to) - 1

    mappings.zipWithIndex
      .collect { case (e, i) if i >= start && i <= end => e }
      .foldLeft(Option.empty[Long]) {
        case (None, mapping)        => Some(mapping.get(value))
        case (Some(input), mapping) => Some(mapping.get(input))
      }
      .getOrElse(throw Exception(s"could not find path from $from to $to for value $value"))

  def constructAll(input: List[String]) = {
    val lines = input.map(MapLine.parse)
    val state = lines
      .foldRight[State](State.empty) {
        case (title: MapLine.MapTitle, State(complete, wip)) => // TITLE LINE
          // move in progress to done
          val newMapping = Mapping(title.from, title.to, wip)
          State(complete.prepended(newMapping), Seq.empty)
        case (value: MapLine.MapValue, state @ State(mappings, wip)) => // VALUE LINE
          // add value to in progress
          state.copy(wip = wip prepended value)
      }
    state.complete
  }

  private case class State(complete: Seq[Mapping], wip: Seq[MapLine.MapValue])
  private object State:
    val empty = State(Seq.empty, Seq.empty)
