package day9

import scala.annotation.tailrec
import scala.util.chaining.*

def predict(line: Vector[Int]): Vector[Int] =
    line.drop(1).zip(line).map(_ - _)

def extrapolate(predictions: Vector[Vector[Int]]) =
    predictions.map(_.last).sum

def extrapolateFromLine(line: Vector[Int]) =

    @tailrec
    def loop(line: Vector[Int], acc: Vector[Vector[Int]]): Vector[Vector[Int]] =
        if line.forall(_ == 0) then acc.appended(line)
        else
            loop(
                predict(line),
                acc.appended(line),
            )

    val predictions = loop(line, Vector.empty)
    extrapolate(predictions)

end extrapolateFromLine

@main
def main =
    val source = io.Source.fromResource("day9.txt")

    // part 1
//    source
//        .getLines()
//        .to(LazyList)
//        .map: line =>
//            line.split("\\s+").map(_.toInt).toVector
//        .map(extrapolateFromLine)
//        .sum
//        .tap(println)

    // part2
    source
        .getLines()
        .to(LazyList)
        .map: line =>
            line.split("\\s+").map(_.toInt).toVector
        .map(_.reverse)
        .map(extrapolateFromLine)
        .sum
        .tap(println)
    source.close()

end main
