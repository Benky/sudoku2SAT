package benky.sudoku.format

import benky.sudoku.{NOT, VAR, Predicate}

/**
 * Prints given of predicates in CNF format so it can be passed to any SAT solver
 */
class CNFFormat extends OutputFormat {
  def format(lines: Seq[Seq[Predicate]]) {
    val convertedLines = lines.map(line => line.map(coordinatesToVar(_)))

    // print header
    println("p cnf " + convertedLines.map(_.max).max + " " + lines.length)

    // print lines sorted by sub-list length
    convertedLines.sortBy(-_.length).foreach(line => println(line.mkString(" ") + " 0"))
  }

  /**
   * Converts given coordinates to variable accepted by SAT solvers
   *
   * Creates bijection between given predicate and Integers. Predicate (row, column, value) is mapped to
   * <code>((value - 1) * 81 + row * 9 + column + 1)</code>
   */
  def coordinatesToVar(p: Predicate): Int = p match {
    case VAR(x, y, v) => ((v - 1) * 81 + x * 9 + y + 1)
    case NOT(pp) => (-1) * coordinatesToVar(pp)
  }
}
