package advent
package day2

import scala.io.Source
import scala.util.chaining.*
import fastparse.*, SingleLineWhitespace.*
import scala.collection.mutable

enum Color:
    case Red(count: Int)   extends Color
    case Blue(count: Int)  extends Color
    case Green(count: Int) extends Color

end Color

object Color:

    def fromLine(s: String, count: Int): Color =
        s match
        case "red"   => Red(count)
        case "blue"  => Blue(count)
        case "green" => Green(count)
        case _       => throw new RuntimeException("wrong color")

end Color

case class Round(draws: List[Color])
case class Game(id: Int, rounds: List[Round])

def gameID[$: P]: P[Int] = P("Game" ~ CharIn("0-9").rep(max = 4).! ~ ":")
    .map: x =>
        x.toInt

def colorParser[$: P] = P(CharIn("0-9").rep(max = 4).! ~/ ("red" | "green" | "blue").!)
    .map:
        case (count, color) => Color.fromLine(color, count.toInt)

def roundParser[$: P] = P(colorParser.rep(sep = ","))
    .map: draws =>
        Round(draws.toList)

def gameParser[$: P] = P(roundParser.rep(sep = ";") ~ End)

def lineParser[$: P] = P(gameID ~/ gameParser)
    .map: x =>
        Game(x._1, x._2.toList)

val redCubes   = 12
val greenCubes = 13
val blueCubes  = 14

def checkGame(game: Game) =
    game
        .rounds
        .forall: round =>
            round
                .draws.forall: draw =>
                    draw match
                    case Color.Red(count)   => count <= redCubes
                    case Color.Blue(count)  => count <= blueCubes
                    case Color.Green(count) => count <= greenCubes

def maximumPower(game: Game) =
    val blue  = game
        .rounds
        .map(
          _.draws
              .collect:
                  case Color.Blue(count) => count
              .maxOption
              .getOrElse(Int.MinValue)
        )
        .max
    val red   = game
        .rounds
        .map(
          _.draws
              .collect:
                  case Color.Red(count) => count
              .maxOption
              .getOrElse(Int.MinValue)
        )
        .max
    val green = game
        .rounds
        .map(
          _.draws
              .collect:
                  case Color.Green(count) => count
              .maxOption
              .getOrElse(Int.MinValue)
        )
        .max
    red * green * blue

end maximumPower

@main
def main(): Unit =
    val source = Source.fromResource("day2.txt")

    // Part1
//    LazyList
//        .from(source.getLines())
//        .map: s =>
//            val Parsed.Success(game, _) = parse(s, lineParser(_))
//            game
//        .toList
//        .filter(checkGame)
//        .map: x =>
//            x.id.tap(println)
//        .sum
//        .tap(println)
    LazyList
        .from(source.getLines())
        .map: s =>
            val Parsed.Success(game, _) = parse(s, lineParser(_))
            game
        .map(maximumPower)
        .sum
        .tap(println)

    source.close()

end main
