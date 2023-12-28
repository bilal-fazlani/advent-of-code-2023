package aoc
package day5

import MapElement.*
import MapLine.MapValue

case class Mapping(from: MapElement, to: MapElement, initial: Seq[MapValue]):
  private def map = initial
    .map { range =>
      val sourceRange = range.sourceStart to (range.sourceStart + range.length)
      val destinationRange = range.destinationStart to (range.destinationStart + range.length)
      sourceRange.zip(destinationRange).toMap
    }
    .reduce(_ ++ _)

  def get(source: Long): Long = map.get(source).getOrElse(source)

  override def toString(): String = s"$from -> $to"

object Mapping:
  def resolve(from: MapElement, to: MapElement, value: Long)(mappings: Seq[Mapping]): Long =
    val chain = Seq(Seed, Soil, Fertilizer, Water, Light, Temperature, Humidity, Location)
    val start = chain.indexOf(from)
    val end = chain.indexOf(to) - 1

    val dd = mappings.zipWithIndex.collect { case (e, i) if i >= start && i <= end => e }.toList

    dd.foldLeft(Option.empty[Long]) {
      case (None, mapping)        => Some(mapping.get(value))
      case (Some(input), mapping) => Some(mapping.get(input))
    }.getOrElse(throw Exception(s"could not find path from $from to $to for value $value"))
