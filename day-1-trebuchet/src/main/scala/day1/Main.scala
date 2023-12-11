package day1

import zio.*

object Main extends ZIOAppDefault:
  val msg = "I was compiled by Scala 3. :)"

  def run = 
    Console.printLine("Hello world!") *> Console.printLine(msg)
    