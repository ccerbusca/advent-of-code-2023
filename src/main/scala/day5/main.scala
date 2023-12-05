package advent
package day5

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.chaining.*

case class Range private (
    start: Long,
    end: Long,
    size: Long,
):

    def intersects(n2: Range): Boolean =
        (start < n2.end && end > n2.start) || (start > n2.end && end < n2.start)

    def isEmpty: Boolean =
        start == end

end Range

object Range:
    lazy val Empty: Range = Range(0, 0, 0)

    def apply(
        start: Long,
        end: Long,
    ): Range =
        if start < end then Range(start, end, end - start)
        else Range.Empty

end Range

case class SeedMapRange(
    source: Range,
    dest: Range,
)

case class SeedMap private (
    ranges: Vector[SeedMapRange]
):

    def intersect(range: Range): (Range, Range, Range) =
        ranges
            .find(_.source.intersects(range))
            .map: r =>
                val start              = r.source.start.max(range.start)
                val end                = r.source.end.min(range.end)
                val leftOuter          = Range(range.start, start)
                val rightOuter         = Range(end, range.end)
                val mappedIntersection = Range(
                    start + (r.dest.start - r.source.start),
                    end + (r.dest.start - r.source.start),
                )
                (leftOuter, mappedIntersection, rightOuter)
            .getOrElse:
                (Range.Empty, range, Range.Empty)

end SeedMap

object SeedMap:

    def fromLines(lines: Vector[Vector[Long]]): SeedMap =
        lines.foldLeft(SeedMap(Vector.empty)): (acc, e) =>
            val dest = Range(e(0), e(0) + e(2))
            val src  = Range(e(1), e(1) + e(2))
            acc.copy(
                ranges = acc.ranges.appended(SeedMapRange(src, dest))
            )

end SeedMap

def lowestLocation(input: Vector[String]) =
    val groups = input.foldLeft(Vector(Vector.empty[String])): (acc, e) =>
        if e.nonEmpty then acc.updated(acc.length - 1, acc.last.appended(e))
        else acc.appended(Vector.empty)

    val seeds: Vector[Range] = groups
        .head.head.substring(6).trim.split("\\s+").map(_.toLong).toVector.grouped(2)
        .map(pair => Range(pair(0), pair(0) + pair(1)))
        .toVector

    val maps = groups.tail.map(_.tail.map(_.split("\\s+").map(_.toLong).toVector)).map(SeedMap.fromLines).tap(println)

    @tailrec
    def loop(
        toProcess: Queue[Range],
        group: SeedMap,
        res: Vector[Range],
    ): Vector[Range] =
        toProcess.dequeueOption match
        case Some((range, q)) =>
            val (left, intersection, right) = group.intersect(range)

            loop(q.enqueueAll(List(left, right).filter(!_.isEmpty)), group, res.appended(intersection))
        case None             =>
            res
        end match
    end loop

    maps
        .foldLeft(seeds): (acc, seedMap) =>
            loop(Queue.from(acc), seedMap, Vector.empty)
        .minBy(_.start)

end lowestLocation

@main
def main =
    val source = Source.fromResource("day5.txt")

    val input = source.getLines().toVector

    lowestLocation(input).start.tap(println)

    source.close()

end main
