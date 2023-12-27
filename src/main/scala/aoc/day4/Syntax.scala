package aoc
package day4

import zio.parser.*
import zio.parser.Parser.*
import zio.Chunk

case class Card(
    id: Int,
    winningNumbers: Chunk[Int],
    ownNumbers: Chunk[Int]
)

object Syntax:
  val number = digit.repeat.map(_.mkString.toInt)
  val pipe = char('|')
  val colon = char(':')
  val cardId = string("Card ", ()) ~ whitespace.unit ~ number ~ colon ~ whitespace.unit
  val numberSet = number.repeatWithSep(whitespace.optional.unit).optimized
  val card = (cardId ~ numberSet ~ whitespace.unit ~ pipe ~ whitespace.unit ~ numberSet)
    .map(Card.apply)
