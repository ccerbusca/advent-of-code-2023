package advent
package day5

import scala.collection.immutable.NumericRange
import scala.io.Source
import scala.util.chaining.*

case class SeedMapRange(
    source: NumericRange[Long],
    dest: NumericRange[Long],
)

case class SeedMap private (
    ranges: Vector[SeedMapRange]
):

    def apply(e: Long): Long =
        ranges
            .find(_.source.contains(e))
            .map: x =>
                x.dest.start + e - x.source.start
            .getOrElse(e)

end SeedMap

object SeedMap:

    def fromLines(lines: Vector[Vector[Long]]): SeedMap =
        lines.foldLeft(SeedMap(Vector.empty)): (acc, e) =>
            val dest = e(0) until (e(0) + e(2))
            val src  = e(1) until (e(1) + e(2))
            acc.copy(
                ranges = acc.ranges.appended(SeedMapRange(src, dest))
            )

end SeedMap

def lowestLocation(input: Vector[String]) =
    val groups = input.foldLeft(Vector(Vector.empty[String])): (acc, e) =>
        if e.nonEmpty then acc.updated(acc.length - 1, acc.last.appended(e))
        else acc.appended(Vector.empty)

    val seeds = groups.head.head.substring(6).trim.split("\\s+").map(_.toLong).toVector

    val maps = groups.tail.map(_.tail.map(_.split("\\s+").map(_.toLong).toVector)).map(SeedMap.fromLines)

    seeds
        .map: s =>
            maps.foldLeft(s): (acc, e) =>
                e(acc)
        .min

end lowestLocation

@main
def main =
    val source = Source.fromResource("day5.txt")

    val input = source.getLines().toVector

    lowestLocation(input).tap(println)

    source.close()

end main
