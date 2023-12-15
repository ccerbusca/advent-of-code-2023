package day15

import scala.util.chaining.*

def hash(step: String): Int =
    step.map(_.toInt).foldLeft(0): (acc, c) =>
            ((acc + c) * 17) % 256

case class Lens(label: String, focalLength: Int)

@main
def main =
    val source = io.Source.fromResource("day15.txt")

    source
        .getLines().next()
        .split(",")
        .to(LazyList)
        .foldLeft(Map.empty[Int, Vector[Lens]].withDefaultValue(Vector.empty)): (acc, step) =>
            if step.endsWith("-") then
                val label   = step.substring(0, step.length - 1)
                val key     = hash(label)
                val current = acc(key)
                acc.updated(key, current.filter(_.label != label))
            else
                val parts   = step.split("=")
                val lens    = Lens(parts(0), parts(1).toInt)
                val key     = hash(lens.label)
                val current = acc(key)
                val updatedLenses = current.zipWithIndex.find(_._1.label == lens.label) match
                    case Some((_, idx)) => current.updated(idx, lens)
                    case None => current.appended(lens)
                acc.updated(key, updatedLenses)
        .tap(println)
        .map: (boxNr, lenses) =>
            lenses
                .zipWithIndex.map: (lens, i) =>
                    (1 + boxNr) * (i + 1) * lens.focalLength
                .sum
        .sum
        .tap(println)

    source.close()

end main
