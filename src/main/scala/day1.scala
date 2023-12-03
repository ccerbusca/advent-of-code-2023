package advent

import scala.io.Source
import scala.util.chaining.*

val digits = Map(
  "one"   -> "1",
  "two"   -> "2",
  "three" -> "3",
  "four"  -> "4",
  "five"  -> "5",
  "six"   -> "6",
  "seven" -> "7",
  "eight" -> "8",
  "nine"  -> "9",
)

def firstDigit(s: String) =
    (digits.values ++ digits.keys)
        .map: d =>
            s.indexOf(d) -> digits.getOrElse(d, d)
        .filter(_._1 != -1)
        .minBy(_._1)
        ._2
end firstDigit

def lastDigit(s: String) =
    (digits.values ++ digits.keys)
        .map: d =>
            s.lastIndexOf(d) -> digits.getOrElse(d, d)
        .filter(_._1 != -1)
        .maxBy(_._1)
        ._2
end lastDigit

def calibrationValue(s: String) =
    print(s)
    print(" ")
    val first = firstDigit(s).tap(print)
    val last  = lastDigit(s).tap(println)
    s"$first$last".toInt
end calibrationValue

@main
def main(): Unit =
    val source = Source.fromResource("day1.txt")

    LazyList
        .from(source.getLines())
        .map(calibrationValue)
        .sum
        .tap(println)

    source.close()

end main
