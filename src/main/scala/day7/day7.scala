package day7

import scala.io.Source
import scala.util.chaining.*

val cardStrength = "AKQT98765432J"

enum HandType(val priority: Int):
    case HighCard     extends HandType(1)
    case Pair         extends HandType(2)
    case TwoPair      extends HandType(3)
    case ThreeOfAKind extends HandType(4)
    case FullHouse    extends HandType(5)
    case FourOfAKind  extends HandType(6)
    case FiveOfAKind  extends HandType(7)

end HandType

def handTypeStrength(hand: Hand): HandType =
    val sorted = hand
        .handMap.removed('J').toList
        .sortBy(_._2)(Ordering[Int].reverse)

    val jokers = hand.handMap.getOrElse('J', 0)

    sorted.headOption.map(_._2).getOrElse(0) + jokers match
    case 5 => HandType.FiveOfAKind
    case 4 => HandType.FourOfAKind
    case 3 =>
        sorted(1)._2 match
        case 2 => HandType.FullHouse
        case 1 => HandType.ThreeOfAKind
    case 2 =>
        sorted(1)._2 match
        case 2 => HandType.TwoPair
        case 1 => HandType.Pair
    case 1 => HandType.HighCard
    case _ => throw new RuntimeException(s"Invalid hand: $hand")

    end match

end handTypeStrength

case class Hand(hand: String, handMap: Map[Char, Int], bid: Long):

    def strongerThan(h2: Hand): Boolean =
        val h1Type = handTypeStrength(this)
        val h2Type = handTypeStrength(h2)
        if h1Type.priority > h2Type.priority then true
        else if h1Type.priority < h2Type.priority then false
        else
            hand.zip(h2.hand)
                .dropWhile(_ == _)
                .map((c1, c2) => cardStrength.indexOf(c1) < cardStrength.indexOf(c2))
                .headOption
                .getOrElse(throw new RuntimeException("Hands are equal"))

        end if

    end strongerThan

end Hand

@main
def main =
    val source = Source.fromResource("day7.txt")

    source
        .getLines()
        .to(LazyList)
        .map: line =>
            val lineParts = line.split("\\s+")
            val hand      = lineParts.head
            val handMap   = hand.groupMapReduce(identity)(_ => 1)(_ + _)
            val bid       = lineParts.last.toInt
            Hand(hand, handMap, bid)
        .sortWith: (h1, h2) =>
            h2.strongerThan(h1)
        .zipWithIndex
        .map: (h, i) =>
            (h, i.toLong + 1L)
        .map: (h, rank) =>
            h.bid * rank
        .sum
        .tap(println)

    source.close()

end main
