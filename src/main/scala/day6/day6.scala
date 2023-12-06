package day6

import scala.io.Source
import scala.util.chaining.*

def countSolutions(raceTime: Long, recordDistance: Long): Long =
  val a = -1
  val b = raceTime
  val c = -recordDistance
  val delta = b * b - 4 * a * c
  val x1 = (-b + Math.sqrt(delta)) / (2 * a)
  val x2 = (-b - Math.sqrt(delta)) / (2 * a)
  x2.ceil.toLong - 1 - x1.floor.toLong


def part1(lines: List[String]): Long =
  val time = lines.head.substring(5).trim.split("\\s+").map(_.toLong).toVector
  val distance = lines.tail.head.substring(9).trim.split("\\s+").map(_.toLong).toVector

  val input = time.zip(distance)

  input.map(countSolutions.tupled).product

def part2(lines: List[String]): Long =
  val time = lines.head.substring(5).trim.split("\\s+").mkString.toLong
  val distance = lines.tail.head.substring(9).trim.split("\\s+").mkString.toLong
  countSolutions(time, distance)


@main
def main =
  val source = Source.fromResource("day6.txt")

  val lines = source.getLines().toList

  part2(lines).tap(println)

  source.close()
