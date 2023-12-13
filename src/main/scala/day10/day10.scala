package day10

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.chaining.*

enum Position(val x: Int, val y: Int):
    case North extends Position(-1, 0)
    case South extends Position(1, 0)
    case East  extends Position(0, 1)
    case West  extends Position(0, -1)

    def opposite =
        this match
        case Position.North => South
        case Position.South => North
        case Position.East  => West
        case Position.West  => East

end Position

import day10.Position.*

sealed abstract class Pipe(val positions: Set[Position]):

    def canConnect(pipe: Pipe): Boolean =
        positions.exists: p =>
            pipe.positions.contains(p.opposite)

end Pipe

object Pipe:

    case object Vertical extends Pipe(Set(North, South))

    case object Horizontal    extends Pipe(Set(West, East))
    case object L             extends Pipe(Set(North, East))
    case object J             extends Pipe(Set(West, North))
    case object _7            extends Pipe(Set(West, South))
    case object F             extends Pipe(Set(South, East))
    case object Ground        extends Pipe(Set.empty)
    case class Start(p: Pipe) extends Pipe(p.positions)

    def parse(c: Char): Pipe =
        c match
        case '|' => Vertical
        case '-' => Horizontal
        case 'L' => L
        case 'J' => J
        case '7' => _7
        case 'F' => F
        case '.' => Ground
        case 'S' => throw new RuntimeException("Should have handled this earlier")

    private val values: List[Pipe] = List(
        Vertical,
        Horizontal,
        L,
        J,
        _7,
        F,
    )

    def fromPositions(positions: Set[Position]): Pipe =
        values.find(_.positions.intersect(positions).size == 2).get

end Pipe

case class Cell(
    i: Int,
    j: Int,
    pipe: Pipe,
)

def inferStart(i: Int, j: Int, input: Vector[Vector[(Int, Int, Char)]]): Pipe =
    val positions =
        for
            position <- Position.values.toSet
            (i2, j2, c) = input(i + position.x)(j + position.y)
            cell        = Pipe.parse(c)
            if cell.positions.map(_.opposite).contains(position)
        yield position

    Pipe.fromPositions(positions)

end inferStart

def farthestPoint(input: Vector[Vector[Cell]]): Int =
    val start = input
        .to(LazyList)
        .map(_.find(_.pipe.isInstanceOf[Pipe.Start]))
        .collectFirst { case Some(pipe) => pipe }
        .get

    val stack = mutable.Stack[(Cell, Int)]()

    @tailrec
    def loop(stack: mutable.Stack[(Cell, Int)], visited: Set[Cell]): Int =
        val (current, length) = stack.pop()
        if current == start && length > 0 then length / 2
        else
            val cells = for
                position <- current.pipe.positions.toVector
                if current.i + position.x >= 0 && current.i + position.x < input.length
                if current.j + position.y >= 0 && current.j + position.y < input.head.length
                c @ Cell(_, _, p) = input(current.i + position.x)(current.j + position.y)
                if !visited.contains(c)
                if p.canConnect(current.pipe)
            yield (c, length + 1)

            loop(stack.pushAll(cells), visited + current - start)
        end if
    end loop

    loop(stack.push(start -> 0), Set.empty)

end farthestPoint

@main
def main =
    val source = io.Source.fromResource("day10.txt")

    val lines = source
        .getLines()
        .toVector

    val chars = lines
        .zipWithIndex
        .map:
            case (line, i) =>
                line.zipWithIndex.map: (c, j) =>
                        (i, j, c)
        .map(_.toVector)

    val input =
        chars
            .map: line =>
                line.map: (i, j, c) =>
                    Cell(i, j, if c != 'S' then Pipe.parse(c) else Pipe.Start(inferStart(i, j, chars)))

    farthestPoint(input).tap(println)

    source.close()

end main
