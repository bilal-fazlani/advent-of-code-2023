package aoc
package day3

import zio.parser.*
import zio.parser.Parser.*
import zio.Chunk

enum Cell:
  case Empty
  case SpecialChar
  case Digit(int: Char)

  def isSpecial = this match
    case SpecialChar => true
    case _           => false

  def digit: Option[Char] = this match
    case Digit(int) => Some(int)
    case _          => None

object Cell:
  def parse(value: Char): Cell =
    lazy val dot = char('.').map(_ => Cell.Empty)
    lazy val digitSyntax = digit.map(Cell.Digit(_))

    val cellSyntax = dot | digitSyntax
    cellSyntax.parseChars(Chunk(value)).getOrElse(Cell.SpecialChar)
