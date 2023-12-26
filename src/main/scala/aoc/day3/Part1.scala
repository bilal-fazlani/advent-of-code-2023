package aoc
package day3

import zio.*

case class DigitScan(int: Char, isAdjacentToSymbol: Boolean)

object Part1 extends Challenge[Int](day(3)):

  def execute =
    for {
      allLines <- file.runCollect
      metrix = allLines.map(readLine)
      length = metrix.length
      width = metrix.head.length
      scannedDigits =
        metrix.zipWithIndex
          .map(x => (x._1.zipWithIndex, x._2))
          .map(x =>
            x._1.map { y =>
              scanDigit(metrix, x._2, y._2, width, length)
            }
          )
      numbersScanned = findNumbers(scannedDigits)
      sum = numbersScanned.collect { case Number(value, true) =>
        value
      }.sum
    } yield sum

  def readLine(line: String) = Chunk.from(line.map(Cell.parse))

  def scanDigit(
      metrix: Chunk[Chunk[Cell]],
      x: Int,
      y: Int,
      width: Int,
      height: Int
  ): Option[DigitScan] =
    val cell = metrix(x)(y)
    cell.digit.map { int =>
      val topLeft: Option[Cell] = if x <= 0 || y <= 0 then None else Some(metrix(x - 1)(y - 1))
      val topCenter: Option[Cell] = if y <= 0 then None else Some(metrix(x)(y - 1))
      val topRight: Option[Cell] = if y <= 0 || x >= (width - 1) then None else Some(metrix(x + 1)(y - 1))

      val left: Option[Cell] = if x <= 0 then None else Some(metrix(x - 1)(y))
      val right: Option[Cell] = if x >= (width - 1) then None else Some(metrix(x + 1)(y))

      val bottomLeft: Option[Cell] = if x <= 0 || y >= (height - 1) then None else Some(metrix(x - 1)(y + 1))
      val bottomCenter: Option[Cell] = if y >= (height - 1) then None else Some(metrix(x)(y + 1))
      val bottomRight: Option[Cell] = if x >= (width - 1) || y >= (height - 1) then None else Some(metrix(x + 1)(y + 1))

      val isAdjacent = Seq(topLeft, topCenter, topRight, left, right, bottomLeft, bottomCenter, bottomRight).flatten
        .exists(_.isSpecial)
      if isAdjacent then DigitScan(int, true)
      else DigitScan(int, false)
    }

  case class Number(value: Int, isPartNumber: Boolean)

  enum State:
    case Stopped(numbers: Chunk[Number])
    case Working(numbers: Chunk[Number], pending: Chunk[DigitScan])

  object State:
    val empty = State.Stopped(Chunk.empty)

  def findNumbers(scan: Chunk[Chunk[Option[DigitScan]]]): Chunk[Number] =
    var currentGroup: Chunk[DigitScan] = Chunk.empty
    var currentPart: Boolean = false

    var done: Chunk[Number] = Chunk.empty

    def isInProgress = currentGroup.nonEmpty

    scan.foreach(row =>
      row.foreach {
        case Some(digit) =>
          currentGroup = currentGroup.appended(digit)
          if digit.isAdjacentToSymbol then currentPart = true
        case None =>
          // if some number is in progress
          if currentGroup.nonEmpty then
            // move it to done
            done = done.appended(Number(currentGroup.map(_.int).mkString.toInt, currentPart))
            // reset current number
            currentGroup = Chunk.empty

          // always part to false when encountering a wall
          currentPart = false
      }
    )
    done
