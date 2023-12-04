package advent
package day3

import scala.io.Source
import scala.util.chaining.*
import scala.collection.mutable

def isSymbol(c: Char) =
    !c.isDigit && c != '.'

val coords = Vector(-1, 0, 1)

def getNumber(vv: Vector[Vector[Char]], i: Int, j: Int, found: mutable.Map[(Int, Int), Boolean]): Option[Int] =
    if found((i, j)) then None
    else
        val (left1, right1) = vv(i).zipWithIndex.splitAt(j)
        val left2           = left1.reverse.takeWhile(_._1.isDigit).reverse
        val right2          = right1.takeWhile(_._1.isDigit)
        val numberPart      = left2 ++ right2
        numberPart.foreach:
            case (_, idx) => found.addOne((i, idx) -> true)
        Some(numberPart.map(_._1).mkString.toInt)

def findNumbers(symbols: Vector[(Char, (Int, Int))], vv: Vector[Vector[Char]]) =
    val found   = mutable.Map[(Int, Int), Boolean]().withDefaultValue(false)
    val gears   = mutable.Map[(Int, Int), Vector[Int]]().withDefaultValue(Vector.empty)
    val results = for
        (symbol, (i, j)) <- symbols
        x                <- coords
        y                <- coords
        if vv(i + x)(j + y).isDigit
    yield getNumber(vv, i + x, j + y, found).tap:
        case Some(x) if symbol == '*' =>
            val old = gears((i, j))
            gears((i, j)) = old.appended(x)
        case _                        => ()

    val res = results.collect:
        case Some(x) => x

    (res, gears.toMap)

end findNumbers

def partNumbers(vv: Vector[Vector[Char]]) =
    val symbols = for
        (v, i) <- vv.zipWithIndex
        (e, j) <- v.zipWithIndex
        if isSymbol(e)
    yield (e, (i, j))

    findNumbers(symbols, vv)

end partNumbers

@main
def main: Unit =
    val source = Source.fromResource("day3.txt")

    val input = LazyList
        .from(source.getLines())
        .map: x =>
            x.toVector
        .toVector

    val (res, gears) = partNumbers(input)

    gears.values.filter(_.length == 2).map(_.product).sum.tap(println)

    source.close()

end main
