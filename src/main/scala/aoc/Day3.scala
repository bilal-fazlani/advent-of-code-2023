package aoc
package day3

import zio.*
import zio.stream.*

object Day3 extends Challenge[Int](day(3)):

  def execute: Task[Int] =
    for {
      allLines <- file.runCollect
      metrix = allLines.map(readLine)
      linesLength = metrix.length
      linesWidth = metrix.head.length
    } yield 3

  def readLine(line: String) = Chunk.from(line.map(Cell.parse))

enum Cell:
  case Empty
  case SpecialChar
  case Digit(int: Char)

object Cell:  
  def parse(value: Char): Cell =
    import zio.parser.*
    import zio.parser.Parser.*

    lazy val dot = char('.').map(_ => Cell.Empty)
    lazy val digitSyntax = digit.map(n => Cell.Digit(n))

    val cellSyntax = dot | digitSyntax
    cellSyntax.parseChars(Chunk(value)).getOrElse(Cell.SpecialChar)
