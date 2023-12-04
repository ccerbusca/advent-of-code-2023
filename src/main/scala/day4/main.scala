package advent
package day4

import scala.io.Source
import scala.util.chaining.*
import fastparse.*
import NoWhitespace.*

import scala.annotation.tailrec
import scala.collection.immutable.Queue

def number[$: P]: P[Int] = P(" ".? ~ CharIn("0-9").rep(min = 1, max = 3).!)
    .map: x =>
        x.toInt

def cardParser[$: P]: P[Int] = P("Card" ~ " ".rep ~ CharIn("0-9").rep(min = 1, max = 3).! ~ ": ")
    .map: x =>
        x.toInt

def numberSequence[$: P] = P(number.rep(sep = " "))
    .map: x =>
        x.toSet

case class Card(id: Int, winningNumbers: Set[Int], cardNumbers: Set[Int]):
    def matchingNumbers: Set[Int] = winningNumbers.intersect(cardNumbers)

def lineParser[$: P] = P(cardParser ~/ numberSequence ~ " | " ~ numberSequence ~ End)
    .map(Card.apply.tupled)

def points(c: Card) =
    val overlap = c.winningNumbers.intersect(c.cardNumbers)
    if overlap.nonEmpty then scala.math.pow(2, overlap.size - 1).toInt
    else 0

end points

def countScratchCards(cards: Vector[Card]) =
    val map = cards.map(c => c.id -> c).toMap

    @tailrec
    def loop(toProcess: Queue[Card], won: Vector[Card]): Vector[Card] =
        toProcess.dequeueOption match
        case Some((card, q)) =>
            val matches  = card.matchingNumbers.size
            val cardsWon =
                for i <- (card.id + 1) to (card.id + matches)
                yield map(i)
            loop(q.appendedAll(cardsWon), won.appendedAll(cardsWon))

        case None =>
            won

    loop(Queue.from(cards), cards).length

end countScratchCards

@main
def main =
    val source = Source.fromResource("day4.txt")

//    //Part1
//    val input = LazyList
//        .from(source.getLines())
//        .map: s =>
//            val Parsed.Success(res, _) = parse(s, lineParser(_))
//            res
//        .map(points)
//        .sum
//        .tap(println)

    val input = LazyList
        .from(source.getLines())
        .map: s =>
            val Parsed.Success(res, _) = parse(s, lineParser(_))
            res
        .toVector

    countScratchCards(input).tap(println)

    source.close()

end main
