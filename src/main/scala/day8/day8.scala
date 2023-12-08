package day8

import scala.io.Source
import scala.util.chaining.*
import fastparse.*
import SingleLineWhitespace.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.concurrent.{ Await, Future }

def nodeName[$: P] = P(CharIn("1-9A-Z").rep(exactly = 3).!)

def parseLine[$: P] = P(nodeName ~ "=" ~ "(" ~ nodeName ~ "," ~ nodeName ~ ")")

def part1(moves: Queue[Char], graph: Map[String, (String, String)], from: String, to: String) =
    @tailrec
    def loop(count: Int, now: String, currentMoves: Queue[Char]): Int =
        if now == to then count
        else
            val (left, right) = graph(now)
            val (move, rest)  = currentMoves.dequeue
            val next          = move match
            case 'L' => left
            case 'R' => right
            if rest.isEmpty then loop(count + 1, next, moves)
            else loop(count + 1, next, rest)

    loop(0, from, moves)
end part1

def greatestCommonDivisor(a: Long, b: Long): Long =
    if b == 0 then a
    else greatestCommonDivisor(b, a % b)

def leastCommonMultiple(a: Long, b: Long): Long =
    if a > b then a / greatestCommonDivisor(a, b) * b
    else b / greatestCommonDivisor(a, b) * a

def part2(moves: Queue[Char], graph: Map[String, (String, String)], start: Char, end: Char) =
    @tailrec
    def loop(count: Long, now: String, currentMoves: Queue[Char]): Long =
        if now.last == end then count
        else
            val (left, right) = graph(now)
            val (move, rest)  = currentMoves.dequeue
            val next          = move match
            case 'L' => left
            case 'R' => right
            loop(count + 1, next, if rest.nonEmpty then rest else moves)

    import scala.concurrent.ExecutionContext.Implicits.global

    val futures = graph
        .keys.filter(_.last == start)
        .map { startNode =>
            Future(loop(0L, startNode, moves))
        }
    val result  = Future.sequence(futures)

    val res = Await.result(result, scala.concurrent.duration.Duration.Inf)

    res.reduce(leastCommonMultiple)

end part2

@main
def main =
    val source = Source.fromResource("day8.txt")

    val lines = source
        .getLines()
        .to(Vector)

    val moves = lines.head.trim.to(Queue)
    val graph = lines
        .tail.tail.foldLeft(Map.empty[String, (String, String)]): (acc, e) =>
            val Parsed.Success((node, left, right), _) = parse(e, parseLine(_))
            acc.updated(node, (left, right))

    part2(moves, graph, start = 'A', end = 'Z').tap(println)

    source.close()

end main
