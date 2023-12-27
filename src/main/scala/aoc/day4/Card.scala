package aoc
package day4

import zio.parser.*
import zio.parser.Parser.*
import zio.Chunk

case class Card(
    id: Int,
    winningNumbers: Set[Int],
    ownNumbers: Set[Int]
):
  def matchCount = winningNumbers.intersect(ownNumbers).size

  override def toString(): String =
    s"Card $id: \n" +
      s"  ${winningNumbers.mkString(", ")}\n" +
      s"  ${ownNumbers.mkString(", ")}"

object Card:
  def parse(input: String): Card =
    Syntax.card.parseString(input).orDie(_.toString)      

object Syntax:
  val number = digit.repeat.map(_.mkString.toInt)
  val pipe = char('|')
  val colon = char(':')
  val cardId = string("Card ", ()) ~ whitespace.unit ~ number ~ colon ~ whitespace.unit
  val numberSet = number.repeatWithSep(whitespace.optional.unit).map(_.toSet)
  val card = (cardId ~ numberSet ~ whitespace.unit ~ pipe ~ whitespace.unit ~ numberSet)
    .map(Card.apply)
