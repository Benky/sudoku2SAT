import collection.mutable.ListBuffer
import io.Source

object Sudoku2SAT {

  val ValueList = 1 to 9

  def main(args: Array[String]) {

    if (args.length != 1) {
      println("Usage: sudoku2SAT [sudoku_filename]")
      sys.exit(1)
    }

    val buffer = ListBuffer.empty[List[Int]]

    // Add preset values
    buffer.appendAll(readFile(args(0)).map(i => List(Function.tupled(coordinatesToVar _)(i))))

    // Add rules for rows
    buffer.appendAll(rows.flatMap(addValuesToGroups(_)))

    // Add rules for columns
    buffer.appendAll(columns.flatMap(addValuesToGroups(_)))

    // Add rules for squares
    buffer.appendAll(squares.flatMap(addValuesToGroups(_)))

    // Add rules for other things
    buffer.appendAll(product(0 to 8).flatMap(Function.tupled(oneLabel _)(_)))

    printCNF(buffer.toList)
  }

  /**
   * CNF output
   */
  def printCNF(lines: Seq[Seq[Int]]) {
    // print header
    println("p cnf " + lines.map(_.max).max + " " + lines.length)
    // print lines sorted by sub-list length
    lines.sortBy(-_.length).foreach(line => println(line.mkString(" ") + " 0"))
  }

  def readFile(name: String) = Source
    .fromFile(name)
    .getLines()
    .map(_.zipWithIndex)
    .zipWithIndex
    .flatMap {
    case (line, row) => line.toList.filter(_._1 != '0').map {
      case (c, column) => (row, column, c.toInt - 48)
    }
  }

  def addValuesToGroups(group: List[(Int, Int)]) = ValueList.map {
    i => group.map {
      case (a, b) => coordinatesToVar(a, b, i)
    }
  }

  /**
   * Converts given coordinates to variable accepted by SAT solvers
   */
  def coordinatesToVar(x: Int, y: Int, z: Int) = ((z - 1) * 81 + x * 9 + y + 1)

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
  def squares = product(0 to 2).map(Function.tupled(quadrant _)(_))

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
    def notBoth(i: Int, j: Int) = List(-coordinatesToVar(x, y, i), -coordinatesToVar(x, y, j))
    val atLeastOne = ValueList.map(i => coordinatesToVar(x, y, i))
    val pairs = product(ValueList).filter(i => i._1 != i._2)
    atLeastOne.toList :: pairs.map(Function.tupled(notBoth _)(_)).toList
  }

  /**
   * Creates cartesian product of given list (list x list)
   */
  def product(list: Range) = (for (i <- list; j <- list) yield (i, j)).toList
}
