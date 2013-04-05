package benky.sudoku

import collection.mutable.ListBuffer
import format.{CNFOutputFormat, SimpleInputFormat}

/**
 * Simple application which converts given Sudoku puzzle to SAT
 */
object Sudoku2SAT {

"a"
  val ValueList = 1 to 9

  def main(args: Array[String]) {

    if (args.length != 1) {
      println("Usage: sudoku2SAT [sudoku_filename]")
      sys.exit(1)
    }

    val buffer = ListBuffer.empty[List[Predicate]]

    // Add preset values
    buffer.appendAll(new SimpleInputFormat().readFile(args(0)).map(i => List(VAR(i._1, i._2, i._3))))

    // Add rules for rows
    buffer.appendAll(rows.flatMap(addValuesToGroups(_)))

    // Add rules for columns
    buffer.appendAll(columns.flatMap(addValuesToGroups(_)))

    // Add rules for squares
    buffer.appendAll(squares.flatMap(addValuesToGroups(_)))

    // Add rules for other things
    buffer.appendAll(product(0 to 8).flatMap((oneLabel _).tupled(_)))

    new CNFOutputFormat().format(buffer.toList)
  }

  def addValuesToGroups(group: List[(Int, Int)]) = ValueList.map {
    i => group.map {
      case (a, b) => VAR(a, b, i)
    }
  }

  /**
   * Creates rules for each row
   */
  def rows = (0 to 8).map(x => (0 to 8).map(y => (x, y)).toList).toList

  /**
   * Creates rules for each column
   */
  def columns = (0 to 8).map(x => (0 to 8).map(y => (y, x)).toList).toList

  /**
   * Creates rules for each square
   */
  def squares = product(0 to 2).map((quadrant _).tupled(_))

  /**
   * Helper method which generates coordinates for each square
   */
  def quadrant(x: Int, y: Int) = product(0 to 2).map {
    case (i, j) => (x * 3 + i, y * 3 + j)
  }

  /**
   * Ensures that every cell is label with exactly one value
   */
  def oneLabel(x: Int, y: Int) = {
    def notBoth(i: Int, j: Int) = List(NOT(VAR(x, y, i)), NOT(VAR(x, y, j)))
    val atLeastOne = ValueList.map(i => VAR(x, y, i))
    val pairs = product(ValueList).filter(i => i._1 != i._2)
    atLeastOne.toList :: pairs.map((notBoth _).tupled(_)).toList
  }

  /**
   * Creates cartesian product of given list (list x list)
   */
  def product(list: Range) = (for (i <- list; j <- list) yield (i, j)).toList
}
