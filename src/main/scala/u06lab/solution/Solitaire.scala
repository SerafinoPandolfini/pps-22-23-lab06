package u06lab.solution

import scala.collection.IterableFactory

object Solitaire extends App:
  type Cell = (Int, Int)
  type Solution = Seq[Cell]
  type IterableFactory = Solution => Iterable[Solution]
  given IterableFactory = List(_).view

  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(w: Int, h: Int)(using factory : IterableFactory): Iterable[Solution] =
    def _placeMarks(s: Int): Iterable[Solution] = s match
      case 1 => factory(Seq((w / 2, h / 2)))
      case _ =>
        for
          sequence <- _placeMarks(s - 1)
          x <- 0 until w
          y <- 0 until h
          newCell = (x, y)
          if !sequence.contains(newCell) && isCellAdjacent(newCell, sequence.head)
        yield
          Seq(newCell).appendedAll(sequence)
    _placeMarks(w * h)

  def isCellAdjacent(cell: Cell, last: Cell): Boolean =
    (cell._1 == last._1 && (cell._2 - last._2).abs.equals(2)) ||
      (cell._2 == last._2 && (cell._1 - last._1).abs.equals(2)) ||
      ((cell._1 - last._1).abs.equals(1) && (cell._2 - last._2).abs.equals(1))

  val w = 1
  val h = 1
  val solutions = placeMarks(w, h)
  solutions.foreach(sol => println("\n"+render(solution = sol, width = w, height = h)))
  println("There are " + solutions.size + " solutions")