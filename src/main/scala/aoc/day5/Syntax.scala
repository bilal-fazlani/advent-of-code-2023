package aoc
package day5

import zio.parser.*
import zio.parser.Parser.*

enum MapElement:
  case Seed, Soil, Fertilizer, Water, Light, Temperature, Humidity, Location
object MapElement:
  private val mapping = MapElement.values.map(elem => (elem.toString.toLowerCase, elem)).toMap
  def parse(str: String): MapElement = mapping(str)

object Seeds:
  def parse(str: String) = Syntax.seeds.parseString(str).orDie(_.toString)

sealed trait MapLine
object MapLine:
  case class MapTitle(from: MapElement, to: MapElement) extends MapLine
  case class MapValue(destinationStart: Long, sourceStart: Long, length: Long) extends MapLine

  def parse(str: String): MapLine = (Syntax.mapTitle | Syntax.mapping).parseString(str).orDie(_.toString)

object Syntax:
  val number = digit.repeat.map(_.mkString.toLong)

  val seeds = string("seeds: ", ()) ~ number.repeatWithSep(whitespace.unit).map(_.toList)
  val mapTitle = {
    val elements = MapElement.values.map(_.toString.toLowerCase)
    val mapElement = elements.map(str => string(str, MapElement.parse(str))).reduce(_ | _)
    (mapElement ~ string("-to-", ()) ~ mapElement ~ whitespace.unit ~ string("map:", ()))
      .map(MapLine.MapTitle.apply)
  }
  val mapping = (number ~ whitespace.unit ~ number ~ whitespace.unit ~ number).map(MapLine.MapValue.apply)
