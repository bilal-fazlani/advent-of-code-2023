package aoc
package day3

object Part1 extends ChallengeSync(day(3)):
  case class DigitScan(int: Char, isAdjacentToSymbol: Boolean)

  def execute: Int =
    val metrix = input.map(readLine)
    val length = metrix.length
    val width = metrix.head.length
    val scannedDigits =
      metrix.zipWithIndex
        .map(x => (x._1.zipWithIndex, x._2))
        .map(x =>
          x._1.map { y =>
            scanDigit(metrix, x._2, y._2, width, length)
          }
        )
    val numbersScanned = findNumbers(scannedDigits)
    val sum = numbersScanned.collect { case Number(value, true) =>
      value
    }.sum
    sum

  def readLine(line: String) = line.map(Cell.parse).toList

  private def neighbours(x: Int, y: Int, metrix: List[List[Cell]]) =
    val product =
      (for {
        xs <- (x - 1) to (x + 1)
        ys <- (y - 1) to (y + 1)
        if !(xs == x && ys == y)
      } yield (xs, ys))
        .filter((x1, y1) => x1 >= 0 && y1 >= 0 && x1 < metrix.head.length && y1 < metrix.length)
        .map((x, y) => metrix(x)(y))
        .toList
    product

  def scanDigit(
      metrix: List[List[Cell]],
      x: Int,
      y: Int,
      width: Int,
      height: Int
  ): Option[DigitScan] =
    val cell = metrix(x)(y)
    cell.digit.map { int =>
      val ns = neighbours(x, y, metrix)
      val isAdjacent = ns.exists(x => x.isSpecial || x.isStar)
      if isAdjacent then DigitScan(int, true)
      else DigitScan(int, false)
    }

  case class Number(value: Int, isPartNumber: Boolean)

  def findNumbers(scan: List[List[Option[DigitScan]]]): List[Number] =
    var currentGroup: List[DigitScan] = List.empty
    var currentPart: Boolean = false

    var done: List[Number] = List.empty

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
            currentGroup = List.empty

          // always part to false when encountering a wall
          currentPart = false
      }
    )
    done
